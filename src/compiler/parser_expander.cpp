#include "compiler/parser_expander.hpp"

#include "compiler/analyser.hpp"
#include "compiler/compiler.hpp"
#include "compiler/syntax_list.hpp"
#include "context.hpp"
#include "io/write.hpp"
#include "runtime/action.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/string.hpp"
#include "runtime/syntax.hpp"
#include "util/define_procedure.hpp"
#include "util/list_iterator.hpp"
#include "variable.hpp"
#include "vm/vm.hpp"

#include <ranges>
#include <string>
#include <vector>

#ifndef WIN32
#include <csignal>
#else
#include <intrin.h>
#endif

namespace insider {

static std::string
syntax_to_string(context& ctx, ptr<syntax> stx) {
  return datum_to_string(ctx, syntax_to_datum(ctx, stx));
}

namespace {
  class environment_extender {
  public:
    explicit
    environment_extender(parsing_context& pc,
                         std::vector<tracked_ptr<variable>> extension)
      : pc_{pc}
    {
      pc.environment.push_back(std::move(extension));
    }

    ~environment_extender() {
      pc_.environment.pop_back();
    }

    environment_extender(environment_extender const&) = delete;
    void operator = (environment_extender const&) = delete;

  private:
    parsing_context& pc_;
  };

  class internal_definition_context_guard {
  public:
    explicit
    internal_definition_context_guard(parsing_context& pc)
      : pc_{pc}
    {
      pc_.use_site_scopes.emplace_back();
    }

    ~internal_definition_context_guard() {
      pc_.use_site_scopes.pop_back();
    }

    internal_definition_context_guard(internal_definition_context_guard const&)
    = delete;
    void operator = (internal_definition_context_guard const&) = delete;

  private:
    parsing_context& pc_;
  };

  class parser_action : public action<parser_action> {
  public:
    parser_action(context& ctx, tracked_ptr<syntax> stx, std::string_view msg)
      : action{ctx}
      , msg_{msg}
      , stx_{std::move(stx)}
    { }

    parser_action(context& ctx, ptr<syntax> stx, std::string_view msg)
      : action{ctx}
      , msg_{msg}
      , stx_{track(ctx, stx)}
    { }

    ~parser_action() { this->check(); }

    std::string
    format() const {
      return fmt::format("{}: {}: {}",
                         format_location(stx_->location()),
                         msg_,
                         syntax_to_string(ctx_, stx_.get()));
    }

  private:
    std::string_view    msg_;
    tracked_ptr<syntax> stx_;
  };
}

static parsing_context
make_subcontext(parsing_context& pc) {
  return parsing_context{pc.ctx, pc.module_, pc.origin, {}, {}};
}

static environment_extender
extend_environment(parsing_context& pc, ptr<scope> s) {
  std::vector<tracked_ptr<variable>> ext;
  for (scope::binding const& b : *s)
    if (b.variable)
      ext.push_back(track(pc.ctx, b.variable));

  return environment_extender{pc, std::move(ext)};
}

static bool
is_in_scope(parsing_context& pc, ptr<variable> const& var) {
  if (var->global)
    return true; // Globals are always in scope even when they are not imported.

  for (auto const& level : pc.environment)
    for (auto const& v : level)
      if (var == v)
        return true;

  return false;
}

static ptr<variable>
lookup_variable_binding(ptr<syntax> id) {
  if (auto binding = lookup(id))
    if (binding->variable)
      return binding->variable;

  return {};
}

static ptr<variable>
lookup_variable(parsing_context& pc, ptr<syntax> id) {
  if (auto var = lookup_variable_binding(id)) {
    if (is_in_scope(pc, var))
      return var;
    else
      throw make_compile_error<out_of_scope_variable_error>(
        id, "{}: Not in scope", identifier_name(id)
      );
  }

  return {};
}

static ptr<core_form_type>
lookup_core(parsing_context& pc, ptr<syntax> id) {
  auto var = lookup_variable_binding(id);
  if (!var || !var->global)
    return {};  // Core forms are never defined in a local scope.

  ptr<> form = pc.ctx.get_top_level(*var->global);
  return match<core_form_type>(form);
}

static ptr<syntax>
expect_id(context& ctx, ptr<syntax> x) {
  if (!is_identifier(x))
    throw make_compile_error<syntax_error>(x, "Expected identifier, got {}",
                                           syntax_to_string(ctx, x));

  return x;
}

static ptr<transformer>
lookup_transformer(ptr<syntax> id) {
  if (auto binding = lookup(id)) {
    if (binding->transformer)
      return binding->transformer;
    else
      return {};
  }

  return {};
}

static bool
scope_is_use_site(ptr<scope> s, use_site_scopes_list const& use_site_scopes) {
  return std::ranges::find(use_site_scopes, s,
                           &tracked_ptr<scope>::get) != use_site_scopes.end();
}

static ptr<syntax>
remove_use_site_scopes(context& ctx, ptr<syntax> expr,
                       use_site_scopes_list const& use_site_scopes) {
  assert(is_identifier(expr));

  auto to_remove = expr->scopes() | std::views::filter([&] (ptr<scope> s) {
    return scope_is_use_site(s, use_site_scopes);
  });
  for (ptr<scope> s : to_remove)
    expr = expr->remove_scope(ctx.store, s);

  return expr;
}

static ptr<syntax>
maybe_remove_use_site_scopes(parsing_context& pc, ptr<syntax> name) {
  if (!pc.use_site_scopes.empty())
    return remove_use_site_scopes(pc.ctx, name, pc.use_site_scopes.back());
  else
    return name;
}

static ptr<syntax>
call_transformer_with_continuation_barrier(context& ctx, ptr<> callable,
                                           tracked_ptr<syntax> const& stx) {
  ptr<> result
    = call_with_continuation_barrier(ctx, callable, {stx.get()}).get();
  if (auto s = match<syntax>(result))
    return s;
  else
    throw std::runtime_error{fmt::format(
      "Syntax transformer didn't return a syntax: {}",
      datum_to_string(ctx, result)
    )};
}

static tracked_ptr<syntax>
call_transformer(context& ctx, ptr<transformer> t, tracked_ptr<syntax> stx,
                 std::vector<tracked_ptr<scope>>* use_site_scopes) {
  auto introduced_env = make_tracked<scope>(
    ctx, ctx,
    fmt::format("introduced environment for syntax expansion at {}",
                format_location(stx->location()))
  );
  stx = track(ctx, stx->add_scope(ctx.store, introduced_env.get()));

  auto use_site_scope = make_tracked<scope>(
    ctx, ctx,
    fmt::format("use-site scope for syntax expansion at {}",
                format_location(stx->location()))
  );
  stx = track(ctx, stx->add_scope(ctx.store, use_site_scope.get()));

  if (use_site_scopes)
    use_site_scopes->push_back(use_site_scope);

  ptr<syntax> result
    = call_transformer_with_continuation_barrier(ctx, t->callable(), stx);
  return track(ctx, result->flip_scope(ctx.store, introduced_env.get()));
}

// If the head of the given list is bound to a transformer, run the transformer
// on the datum, and repeat.
//
// Causes a garbage collection.
static tracked_ptr<syntax>
expand(context& ctx, tracked_ptr<syntax> stx,
       std::vector<tracked_ptr<scope>>* use_site_scopes) {
  parser_action a(ctx, stx, "Expanding macro use");

  bool expanded;
  do {
    expanded = false;

    if (auto lst = syntax_match<pair>(ctx, stx.get())) {
      ptr<syntax> head = expect<syntax>(car(lst));
      if (is_identifier(head)) {
        if (ptr<transformer> t = lookup_transformer(head)) {
          stx = call_transformer(ctx, t, stx, use_site_scopes);
          expanded = true;
        }
      }
    }
  } while (expanded);

  return stx;
}

static tracked_ptr<syntax>
expand(parsing_context& pc, tracked_ptr<syntax> const& stx) {
  if (pc.record_use_site_scopes()) {
    std::vector<tracked_ptr<scope>> use_site_scopes;
    tracked_ptr<syntax> result = expand(pc.ctx, stx, &use_site_scopes);
    pc.use_site_scopes.back().insert(pc.use_site_scopes.back().end(),
                                     std::move_iterator{use_site_scopes.begin()},
                                     std::move_iterator{use_site_scopes.end()});
    return result;
  } else
    return expand(pc.ctx, stx, nullptr);
}

template <auto Analyse>
static tracked_ptr<>
eval_at_expand_time(parsing_context& pc, ptr<syntax> datum) {
  auto meta_pc = make_subcontext(pc);
  auto proc = compile_syntax(meta_pc.ctx, Analyse(meta_pc, datum),
                             meta_pc.module_);
  return call_with_continuation_barrier(meta_pc.ctx, proc, {});
}

// Causes a garbage collection.
static tracked_ptr<>
eval_meta(parsing_context& pc, ptr<syntax> datum) {
  return eval_at_expand_time<analyse_meta>(pc, datum);
}

static tracked_ptr<>
eval_transformer(parsing_context& pc, ptr<syntax> datum) {
  return eval_at_expand_time<analyse_transformer>(pc, datum);
}

// Causes a garbage collection
static ptr<transformer>
make_transformer(parsing_context& pc, ptr<syntax> expr) {
  parser_action a(pc.ctx, expr, "Evaluating transformer");
  auto transformer_proc = eval_transformer(pc, expr); // GC
  return make<transformer>(pc.ctx, transformer_proc.get());
}

namespace {
struct body_content {
  struct internal_variable {
    tracked_ptr<syntax> id;
    tracked_ptr<syntax> init;
    tracked_ptr<variable> var;
  };

  std::vector<tracked_ptr<syntax>> forms;
  std::vector<internal_variable>   internal_variable_defs;
};
}

static ptr<core_form_type>
match_core_form(parsing_context& pc, ptr<syntax> stx) {
  if (auto cf = syntax_match<core_form_type>(pc.ctx, stx))
    return cf;
  else if (syntax_is<symbol>(stx))
    return lookup_core(pc, stx);
  return {};
}

static ptr<core_form_type>
match_core_form_in_head_of_list(parsing_context& pc, ptr<syntax> stx) {
  if (auto p = syntax_match<pair>(pc.ctx, stx))
    return match_core_form(pc, expect<syntax>(car(p)));
  else
    return {};
}

static void
expand_begin(parsing_context& pc, ptr<syntax> stx,
             std::vector<tracked_ptr<syntax>>& stack) {
  ptr<> lst = syntax_to_list(pc.ctx, stx);
  std::vector<tracked_ptr<syntax>> subforms;
  for (ptr<> e : list_range{cdr(assume<pair>(lst))})
    subforms.push_back(track(pc.ctx, expect<syntax>(e)));

  std::ranges::copy(std::views::reverse(subforms), std::back_inserter(stack));
}

static ptr<>
add_scope_to_list(context& ctx, ptr<> x, ptr<scope> s) {
  if (auto stx = match<syntax>(x))
    return stx->add_scope(ctx.store, s);
  else
    return map(ctx, x, [&] (ptr<> elem) {
      return expect<syntax>(elem)->add_scope(ctx.store, s);
    });
}

static std::tuple<tracked_ptr<syntax>, ptr<syntax>>
parse_name_and_expr(parsing_context& pc, ptr<syntax> stx,
                    std::string const& form_name) {
  ptr<> datum = syntax_to_list(pc.ctx, stx);
  auto name = expect_id(pc.ctx, expect<syntax>(cadr(assume<pair>(datum))));
  auto name_without_scope = maybe_remove_use_site_scopes(pc, name);
  auto expr = expect<syntax>(caddr(assume<pair>(datum)));
  if (!datum || list_length(datum) != 3)
    throw make_compile_error<syntax_error>(stx, "Invalid {} syntax", form_name);
  return {track(pc.ctx, name_without_scope), expr};
}

static void
process_internal_define(
  parsing_context& pc,
  std::vector<body_content::internal_variable>& internal_variables,
  tracked_ptr<syntax> const& expr
) {
  auto [id, init] = parse_name_and_expr(pc, expr.get(), "define");
  auto var = make_tracked<variable>(pc.ctx, identifier_name(id.get()));
  internal_variables.emplace_back(
    body_content::internal_variable{id, track(pc.ctx, init), var}
  );
  define(pc.ctx.store, id.get(), var.get());
  pc.environment.back().emplace_back(std::move(var));
}

static void
process_define_syntax(parsing_context& pc, ptr<syntax> stx) {
  if (pc.module_->get_type() == module_::type::immutable)
    throw std::runtime_error{"Can't mutate an immutable environment"};

  auto [name, expr] = parse_name_and_expr(pc, stx, "define-syntax");
  auto new_tr = make_transformer(pc, expr); // GC
  if (lookup_transformer(name.get()))
    redefine(pc.ctx.store, name.get(), new_tr);
  else
    define(pc.ctx.store, name.get(), new_tr);
}

static tracked_ptr<>
eval_meta_expression(parsing_context& pc, ptr <syntax> stx) {
  parser_action a{pc.ctx, stx, "Evaluating meta expression"};
  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (!datum || list_length(datum) != 2)
    throw std::runtime_error{"Invalid meta syntax"};

  ptr<syntax> expr = expect<syntax>(cadr(assume<pair>(datum)));
  return eval_meta(pc, expr);
}

static bool
is_definition_form(context& ctx, ptr<core_form_type> f) {
  return f == ctx.constants->define || f == ctx.constants->define_syntax;
}

static bool
process_stack_of_internal_defines(parsing_context& pc, body_content& result,
                                  std::vector<tracked_ptr<syntax>> stack) {
  environment_extender internal_env{pc, {}};
  internal_definition_context_guard idc{pc};

  bool seen_expression = false;
  while (!stack.empty()) {
    tracked_ptr<syntax> expr = expand(pc, stack.back()); // GC
    stack.pop_back();

    ptr<core_form_type> form = match_core_form_in_head_of_list(pc, expr.get());
    if (is_definition_form(pc.ctx, form) && seen_expression)
      throw make_compile_error<syntax_error>(expr.get(),
                                             "{} after a nondefinition",
                                             form->name);

    if (form == pc.ctx.constants->define_syntax)
      process_define_syntax(pc, expr.get());
    else if (form == pc.ctx.constants->define)
      process_internal_define(pc, result.internal_variable_defs, expr);
    else if (form == pc.ctx.constants->begin)
      expand_begin(pc, expr.get(), stack);
    else if (form == pc.ctx.constants->meta)
      eval_meta_expression(pc, expr.get());
    else {
      seen_expression = true;
      result.forms.push_back(expr);
    }
  }

  return seen_expression;
}

static std::vector<insider::tracked_ptr<syntax>>
body_expressions_to_stack(parsing_context& pc, ptr<> body_exprs,
                          source_location const& loc) {
  ptr<> list = syntax_to_list(pc.ctx, body_exprs);
  if (!list)
    throw make_compile_error<syntax_error>(loc, "Expected list of expressions");

  std::vector<tracked_ptr<syntax>> stack;
  for (ptr<> e : list_range{list})
    stack.push_back(track(pc.ctx, expect<syntax>(e)));
  std::reverse(stack.begin(), stack.end());
  return stack;
}

// Process the beginning of a body by adding new transformer and variable
// definitions to the environment and expanding the heads of each form in the
// list. Also checks that the list is followed by at least one expression and
// that internal definitions are not interleaved with expessions. Internal
// variable definitions are bound in the returned environment to #void values;
// internal transformer definitions are bound to the transformer.
//
// Causes a garbage collection.
static body_content
process_internal_defines(parsing_context& pc, ptr<> body_exprs,
                         source_location const& loc) {
  body_exprs = add_scope_to_list(
    pc.ctx, body_exprs,
    make<scope>(pc.ctx, pc.ctx,
                fmt::format("outside edge scope at {}", format_location(loc)))
  );
  body_content result;
  bool seen_expression
    = process_stack_of_internal_defines(
      pc, result,
      body_expressions_to_stack(pc, body_exprs, loc)
    );

  if (!seen_expression) {
    if (!result.forms.empty())
      throw make_compile_error<syntax_error>(
        loc, "No expression after a sequence of internal definitions"
      );
    else
      throw make_compile_error<syntax_error>(loc, "Empty body");
  }

  return result;
}

static std::vector<std::unique_ptr<expression>>
parse_expression_list(parsing_context& pc,
                      std::vector<tracked_ptr<syntax>> const& exprs) {
  auto parsed
    = exprs | std::views::transform([&] (tracked_ptr<syntax> const& e) {
      return parse(pc, e.get());
    });
  return {parsed.begin(), parsed.end()};
}

static sequence_expression
parse_body(parsing_context& pc, ptr<> data, source_location const& loc) {
  body_content content = process_internal_defines(pc, data, loc); // GC

  if (!content.internal_variable_defs.empty()) {
    // Simulate a letrec*.

    std::vector<definition_pair_expression> definition_exprs;
    std::vector<tracked_ptr<variable>> variables;
    for (auto const& [id, init, var] : content.internal_variable_defs) {
      variables.push_back(var);
      auto void_expr = make_expression<literal_expression>(
        track(pc.ctx, pc.ctx.constants->void_)
      );
      definition_exprs.emplace_back(id, var, std::move(void_expr));
    }

    environment_extender subenv{pc, variables};

    sequence_expression body_sequence;
    for (std::size_t i = 0; i < definition_exprs.size(); ++i) {
      tracked_ptr<syntax> init_stx = content.internal_variable_defs[i].init;
      std::unique_ptr<expression> init_expr = parse(pc, init_stx.get());
      variables[i]->is_set = true;
      body_sequence.expressions.push_back(
        make_expression<local_set_expression>(std::move(variables[i]),
                                              std::move(init_expr))
      );
    }

    sequence_expression proper_body_sequence{
      parse_expression_list(pc, content.forms)
    };
    body_sequence.expressions.insert(
      body_sequence.expressions.end(),
      std::move_iterator(proper_body_sequence.expressions.begin()),
      std::move_iterator(proper_body_sequence.expressions.end())
    );

    sequence_expression result;
    result.expressions.emplace_back(
      make_expression<let_expression>(std::move(definition_exprs),
                                      std::move(body_sequence))
    );
    return result;
  }
  else
    return sequence_expression{parse_expression_list(pc, content.forms)};
}

namespace {
struct definition_pair {
  tracked_ptr<syntax> id;
  tracked_ptr<syntax> expression;
};
}

static definition_pair
parse_definition_pair(parsing_context& pc, ptr<syntax> stx,
                      std::string_view form_name) {
  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (!datum || datum == pc.ctx.constants->null)
    throw make_compile_error<syntax_error>(
      stx, "Invalid {} syntax: Expected a list, got {}",
      form_name, syntax_to_string(pc.ctx, stx)
    );

  auto id = expect_id(pc.ctx, expect<syntax>(car(assume<pair>(datum))));

  if (cdr(assume<pair>(datum)) == pc.ctx.constants->null)
    throw make_compile_error<syntax_error>(
      stx, "Invalid {} syntax: No expression for {}",
      form_name, identifier_name(id)
    );

  return {track(pc.ctx, id),
          track(pc.ctx, expect<syntax>(cadr(assume<pair>(datum))))};
}

static auto
parse_let_common(parsing_context& pc, ptr<syntax> stx,
                 std::string_view form_name) {
  source_location loc = stx->location();
  tracked_ptr<> datum = track(pc.ctx, syntax_to_list(pc.ctx, stx));
  if (!datum || list_length(datum.get()) < 3)
    throw make_compile_error<syntax_error>(stx, "Invalid {} syntax", form_name);

  ptr<syntax> bindings_stx = expect<syntax>(cadr(assume<pair>(datum.get())));
  ptr<> bindings = syntax_to_list(pc.ctx, bindings_stx);
  if (!bindings)
    throw make_compile_error<syntax_error>(
      bindings_stx, "Invalid {} syntax in binding definitions", form_name
    );

  std::vector<definition_pair> definitions;
  while (bindings != pc.ctx.constants->null) {
    auto binding = expect<syntax>(car(assume<pair>(bindings)));
    if (!syntax_is<pair>(binding))
      throw make_compile_error<syntax_error>(
        binding, "Invalid {} syntax in binding definitions", form_name
      );

    definitions.push_back(parse_definition_pair(pc, binding, form_name));
    bindings = cdr(assume<pair>(bindings));
  }

  ptr<> body = cddr(expect<pair>(datum.get()));
  return std::tuple{definitions, track(pc.ctx, body)};
}

static std::unique_ptr<expression>
parse_let(parsing_context& pc, ptr<syntax> stx_) {
  using namespace std::literals;
  auto stx = track(pc.ctx, stx_);
  auto [definitions, body] = parse_let_common(pc, stx.get(), "let"sv);

  auto subscope = make_tracked<scope>(
    pc.ctx, pc.ctx,
    fmt::format("let body at {}", format_location(stx->location()))
  );

  std::vector<definition_pair_expression> definition_exprs;
  for (definition_pair const& dp : definitions) {
    tracked_ptr<syntax> id
      = track(pc.ctx, dp.id->add_scope(pc.ctx.store, subscope.get()));
    auto var = make_tracked<variable>(pc.ctx, identifier_name(id.get()));
    define(pc.ctx.store, id.get(), var.get());

    definition_exprs.emplace_back(
      id, std::move(var), parse(pc, dp.expression.get())
    );
  }

  ptr<> body_with_scope = add_scope_to_list(pc.ctx, body.get(), subscope.get());
  auto subenv = extend_environment(pc, subscope.get());
  return make_expression<let_expression>(std::move(definition_exprs),
                                         parse_body(pc, body_with_scope,
                                                    stx->location()));
}

static std::unique_ptr<expression>
parse_letrec_star(parsing_context& pc, ptr<syntax> stx) {
  using namespace std::literals;
  source_location loc = stx->location();

  auto [definitions, body] = parse_let_common(pc, stx, "letrec*"sv);

  auto subscope = make_tracked<scope>(pc.ctx, pc.ctx,
                                      fmt::format("letrec* body at {}",
                                                  format_location(loc)));

  std::vector<definition_pair_expression> definition_exprs;
  std::vector<tracked_ptr<variable>> variables;
  for (definition_pair const& dp : definitions) {
    tracked_ptr<syntax> id = track(pc.ctx, dp.id->add_scope(pc.ctx.store,
                                                            subscope.get()));
    auto var = make_tracked<variable>(pc.ctx, identifier_name(dp.id.get()));
    define(pc.ctx.store, id.get(), var.get());

    variables.push_back(var);
    auto void_expr = make_expression<literal_expression>(
      track(pc.ctx, pc.ctx.constants->void_)
    );
    definition_exprs.emplace_back(id, var, std::move(void_expr));
  }

  tracked_ptr<> body_with_scope
    = track(pc.ctx, add_scope_to_list(pc.ctx, body.get(), subscope.get()));
  auto subenv = extend_environment(pc, subscope.get());

  sequence_expression body_sequence;
  for (std::size_t i = 0; i < definition_exprs.size(); ++i) {
    ptr<syntax> expr
      = definitions[i].expression->add_scope(pc.ctx.store, subscope.get());
    std::unique_ptr<expression> init_expr = parse(pc, expr);
    variables[i]->is_set = true;
    body_sequence.expressions.push_back(
      make_expression<local_set_expression>(std::move(variables[i]),
                                            std::move(init_expr))
    );
  }

  auto proper_body_sequence = parse_body(pc, body_with_scope.get(), loc);
  body_sequence.expressions.insert(
    body_sequence.expressions.end(),
    std::move_iterator(proper_body_sequence.expressions.begin()),
    std::move_iterator(proper_body_sequence.expressions.end())
  );

  return make_expression<let_expression>(std::move(definition_exprs),
                                         std::move(body_sequence));
}

static std::unique_ptr<expression>
parse_let_syntax(parsing_context& pc, ptr<syntax> stx) {
  using namespace std::literals;
  source_location loc = stx->location();

  auto [definitions, body] = parse_let_common(pc, stx, "let-syntax"sv);

  auto subscope = make_tracked<scope>(
    pc.ctx, pc.ctx,
    fmt::format("body scope for let-syntax at {}",
                format_location(loc))
  );
  for (definition_pair const& dp : definitions) {
    tracked_ptr<syntax> id
      = track(pc.ctx, dp.id->add_scope(pc.ctx.store, subscope.get()));

    auto transformer = make_transformer(pc, dp.expression.get()); // GC
    define(pc.ctx.store, id.get(), transformer);
  }

  ptr<> body_with_scope = add_scope_to_list(pc.ctx, body.get(), subscope.get());
  auto subenv = extend_environment(pc, subscope.get());

  return make_expression<sequence_expression>(
    parse_body(pc, body_with_scope, loc)
  );
}

static std::unique_ptr<expression>
parse_letrec_syntax(parsing_context& pc, ptr<syntax> stx) {
  using namespace std::literals;
  source_location loc = stx->location();

  auto [definitions, body] = parse_let_common(pc, stx, "letrec-syntax"sv);

  auto subscope
    = make_tracked<scope>(pc.ctx, pc.ctx,
                          fmt::format("body scope for letrec-syntax at {}",
                                      format_location(loc)));
  for (definition_pair const& dp : definitions) {
    tracked_ptr<syntax> id
      = track(pc.ctx, dp.id->add_scope(pc.ctx.store, subscope.get()));
    ptr<syntax> expression
      = dp.expression->add_scope(pc.ctx.store, subscope.get());
    auto transformer = make_transformer(pc, expression);
    define(pc.ctx.store, id.get(), transformer);
  }

  ptr<> body_with_scope = add_scope_to_list(pc.ctx, body.get(), subscope.get());
  auto subenv = extend_environment(pc, subscope.get());

  return make_expression<sequence_expression>(
    parse_body(pc, body_with_scope, loc)
  );
}

static std::unique_ptr<expression>
parse_lambda(parsing_context& pc, ptr<syntax> stx) {
  source_location loc = stx->location();

  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (!datum || cdr(assume<pair>(datum)) == pc.ctx.constants->null)
    throw make_compile_error<syntax_error>(stx, "Invalid lambda syntax");

  ptr<syntax> param_stx = expect<syntax>(cadr(assume<pair>(datum)));
  ptr<> param_names = param_stx;
  std::vector<tracked_ptr<variable>> parameters;
  bool has_rest = false;
  auto subscope = make_tracked<scope>(
    pc.ctx, pc.ctx,
    fmt::format("lambda body at {}", format_location(loc))
  );
  while (!semisyntax_is<null_type>(param_names)) {
    if (auto param = semisyntax_match<pair>(pc.ctx, param_names)) {
      auto id = expect_id(pc.ctx, expect<syntax>(car(param)));
      auto id_with_scope = id->add_scope(pc.ctx.store, subscope.get());

      auto var = make_tracked<variable>(pc.ctx, identifier_name(id));
      parameters.push_back(var);
      define(pc.ctx.store, id_with_scope, var.get());

      param_names = cdr(param);
    }
    else if (semisyntax_is<symbol>(param_names)) {
      has_rest = true;
      auto name = expect<syntax>(param_names);
      auto name_with_scope = name->add_scope(pc.ctx.store, subscope.get());

      auto var = make_tracked<variable>(pc.ctx,
                                        identifier_name(name_with_scope));
      parameters.push_back(var);
      define(pc.ctx.store, name_with_scope, var.get());
      break;
    }
    else
      throw make_compile_error<syntax_error>(
        param_stx, "Unexpected value in lambda parameters: {}",
        datum_to_string(pc.ctx, param_names)
      );
  }

  ptr<> body = cddr(assume<pair>(datum));
  ptr<> body_with_scope = add_scope_to_list(pc.ctx, body, subscope.get());

  auto subenv = extend_environment(pc, subscope.get());

  return make_expression<lambda_expression>(
    std::move(parameters),
    has_rest,
    parse_body(pc, body_with_scope, loc),
    fmt::format("<lambda at {}>", format_location(loc)),
    std::vector<tracked_ptr<variable>>{}
  );
}

static std::unique_ptr<expression>
parse_if(parsing_context& pc, ptr<syntax> stx) {
  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (list_length(datum) != 3 && list_length(datum) != 4)
    throw make_compile_error<syntax_error>(stx, "Invalid if syntax");
  ptr<pair> list = assume<pair>(datum);

  tracked_ptr<syntax> test_expr = track(pc.ctx, expect<syntax>(cadr(list)));
  tracked_ptr<syntax> then_expr = track(pc.ctx, expect<syntax>(caddr(list)));
  tracked_ptr<syntax> else_expr{pc.ctx.store};
  if (cdddr(list) != pc.ctx.constants->null)
    else_expr = track(pc.ctx, expect<syntax>(cadddr(list)));

  return make_expression<if_expression>(
    parse(pc, test_expr.get()),
    parse(pc, then_expr.get()),
    else_expr ? parse(pc, else_expr.get()) : nullptr
  );
}

static std::unique_ptr<expression>
parse_application(parsing_context& pc, ptr<syntax> stx) {
  auto datum = track(pc.ctx, syntax_to_list(pc.ctx, stx));
  if (!datum)
    throw make_compile_error<syntax_error>(stx, "Invalid function call syntax");

  std::unique_ptr<expression> target_expr
    = parse(pc, expect<syntax>(car(assume<pair>(datum.get()))));

  std::vector<std::unique_ptr<expression>> arguments;
  auto arg_expr = track(pc.ctx, cdr(assume<pair>(datum.get())));
  while (arg_expr.get() != pc.ctx.constants->null) {
    arguments.push_back(parse(pc, expect<syntax>(car(assume<pair>(arg_expr.get())))));
    arg_expr = track(pc.ctx, cdr(assume<pair>(arg_expr.get())));
  }

  return make_expression<application_expression>(std::move(target_expr),
                                                 std::move(arguments));
}

static std::unique_ptr<expression>
parse_sequence(parsing_context& pc, ptr<> stx) {
  std::vector<std::unique_ptr<expression>> exprs;
  for (tracked_ptr<> datum = track(pc.ctx, stx);
       !semisyntax_is<null_type>(datum.get());
       datum = track(pc.ctx, syntax_cdr(pc.ctx, datum.get())))
    exprs.push_back(parse(pc, syntax_car(pc.ctx, datum.get())));

  return make_expression<sequence_expression>(std::move(exprs));
}

static std::unique_ptr<expression>
parse_unknown_reference_in_interactive_module(parsing_context& pc,
                                              ptr<syntax> id) {
  return make_expression<unknown_reference_expression>(
    track(pc.ctx, id)
  );
}

static std::unique_ptr<expression>
parse_unknown_reference(parsing_context& pc, ptr<syntax> id) {
  if (pc.module_->get_type() == module_::type::interactive)
    return parse_unknown_reference_in_interactive_module(pc, id);
  else
    throw make_compile_error<unbound_variable_error>(
      id, "Identifier {} not bound to a variable", identifier_name(id)
    );
}

static std::unique_ptr<expression>
parse_reference(parsing_context& pc, ptr<syntax> id) {
  auto var = lookup_variable(pc, id);

  if (!var)
    return parse_unknown_reference(pc, id);
  else if (!var->global)
    return make_expression<local_reference_expression>(track(pc.ctx, var));
  else
    return make_expression<top_level_reference_expression>(*var->global,
                                                           identifier_name(id));
}

static std::unique_ptr<expression>
make_set_expression(parsing_context& pc, tracked_ptr<syntax> const& name,
                    ptr<syntax> expr, tracked_ptr<variable> var) {
  auto initialiser = parse(pc, expr);
  if (auto* l = std::get_if<lambda_expression>(&initialiser->value))
    l->name = identifier_name(name.get());

  if (!var->global) {
    var->is_set = true;
    return make_expression<local_set_expression>(var, std::move(initialiser));
  }
  else
    return make_expression<top_level_set_expression>(*var->global,
                                                     std::move(initialiser));
}

static std::unique_ptr<expression>
parse_define_or_set(parsing_context& pc, ptr<syntax> stx,
                    std::string const& form_name) {
  // Defines are processed in two passes: First all the define'd variables are
  // declared within the module or scope and initialised to #void; second, they
  // are assigned their values as if by set!.
  //
  // This function can be therefore be used for both set! and define forms -- in
  // either case, we emit a set! syntax.

  auto [name, expr] = parse_name_and_expr(pc, stx, form_name);

  auto var = lookup_variable(pc, name.get());
  if (!var)
    throw make_compile_error<unbound_variable_error>(
      name.get(), "Identifier {} not bound to a variable",
      identifier_name(name.get())
    );

  return make_set_expression(pc, name, expr, track(pc.ctx, var));
}

static std::unique_ptr<expression>
parse_define(parsing_context& pc, ptr<syntax> stx) {
  return parse_define_or_set(pc, stx, "define");
}

static std::unique_ptr<expression>
parse_set(parsing_context& pc, ptr<syntax> stx) {
  return parse_define_or_set(pc, stx, "set!");
}

static std::unique_ptr<expression>
parse_syntax_trap(parsing_context& pc, ptr<syntax> stx) {
#ifndef WIN32
  raise(SIGTRAP);
#else
  __debugbreak();
#endif

  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (!datum)
    throw make_compile_error<syntax_error>(stx, "Invalid syntax-trap syntax");

  return parse(pc, expect<syntax>(cadr(assume<pair>(datum))));
}

static std::unique_ptr<expression>
parse_syntax_error(parsing_context& pc, ptr<syntax> stx) {
  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (!datum || list_length(datum) < 2)
    throw make_compile_error<syntax_error>(
      stx, "Invalid syntax-error syntax, how ironic"
    );

  std::string result_msg = syntax_expect<string>(
    pc.ctx, expect<syntax>(cadr(assume<pair>(datum)))
  )->value();
  for (ptr<> irritant : list_range{cddr(assume<pair>(datum))})
    result_msg += " " + syntax_to_string(pc.ctx, expect<syntax>(irritant));

  throw std::runtime_error{result_msg};
}

static std::unique_ptr<expression>
parse_meta(parsing_context& pc, ptr<syntax> stx) {
  tracked_ptr<> datum = eval_meta_expression(pc, stx);
  return make_expression<literal_expression>(datum);
}

namespace {
struct qq_template;

struct cons_pattern {
  std::unique_ptr<qq_template> car;
  std::unique_ptr<qq_template> cdr;
};

struct list_pattern {
  std::vector<std::unique_ptr<qq_template>> elems;
  std::optional<cons_pattern> last;
};

struct vector_pattern {
  std::vector<std::unique_ptr<qq_template>> elems;
};

struct literal {
  tracked_ptr<syntax> value;
};

struct unquote {
  tracked_ptr<syntax> datum;
  bool splicing;
};

struct qq_template {
  using value_type = std::variant<
    list_pattern,
    vector_pattern,
    literal,
    unquote
  >;

  value_type          value;
  tracked_ptr<syntax> stx;

  qq_template(value_type value, tracked_ptr<syntax> const& stx)
    : value(std::move(value))
    , stx{stx}
  { }
};

struct quote_traits {
  static constexpr char const* splicing_form_name = "unquote-splicing";

  static bool
  is_qq_form(parsing_context& pc, ptr<> stx);

  static bool
  is_unquote(context& ctx, ptr<core_form_type> f) {
    return f == ctx.constants->unquote;
  }

  static bool
  is_unquote_splicing(context& ctx, ptr<core_form_type> f) {
    return f == ctx.constants->unquote_splicing;
  }

  static bool
  is_quasiquote(context& ctx, ptr<core_form_type> f) {
    return f == ctx.constants->quasiquote;
  }

  static std::unique_ptr<expression>
  wrap(context&, ptr<syntax>, std::unique_ptr<expression> expr,
       bool force_unwrapped);

  static ptr<>
  unwrap(context&, ptr<syntax>);
};

struct syntax_traits {
  static constexpr char const* splicing_form_name = "unsyntax-splicing";

  static bool
  is_qq_form(parsing_context& pc, ptr<> stx);

  static bool
  is_unquote(context& ctx, ptr<core_form_type> f) {
    return f == ctx.constants->unsyntax;
  }

  static bool
  is_unquote_splicing(context& ctx, ptr<core_form_type> f) {
    return f == ctx.constants->unsyntax_splicing;
  }

  static bool
  is_quasiquote(context& ctx, ptr<core_form_type> f) {
    return f == ctx.constants->quasisyntax;
  }

  static std::unique_ptr<expression>
  wrap(context&, ptr<syntax>, std::unique_ptr<expression> expr,
       bool force_unwrapped);

  static ptr<>
  unwrap(context&, ptr<syntax>);
};
} // anonymous namespace

bool
quote_traits::is_qq_form(parsing_context& pc, ptr<> stx) {
  if (auto p = semisyntax_match<pair>(pc.ctx, stx))
    if (auto form = match_core_form(pc, expect<syntax>(car(p))))
      return form == pc.ctx.constants->unquote
             || form == pc.ctx.constants->unquote_splicing
             || form == pc.ctx.constants->quasiquote;

  return false;
}

std::unique_ptr<expression>
quote_traits::wrap(context&, ptr<syntax>, std::unique_ptr<expression> expr,
                   bool) {
  return expr;
}

ptr<>
quote_traits::unwrap(context& ctx, ptr<syntax> stx) {
  return syntax_to_datum(ctx, stx);
}

bool
syntax_traits::is_qq_form(parsing_context& pc, ptr<> stx) {
  if (auto p = semisyntax_match<pair>(pc.ctx, stx))
    if (auto form = match_core_form(pc, expect<syntax>(car(p))))
      return form == pc.ctx.constants->unsyntax
             || form == pc.ctx.constants->unsyntax_splicing
             || form == pc.ctx.constants->quasisyntax;

  return false;
}

std::unique_ptr<expression>
syntax_traits::wrap(context& ctx, ptr<syntax> s,
                    std::unique_ptr<expression> expr,
                    bool force_unwrapped) {
  if (force_unwrapped)
    return expr;
  else
    return make_application(ctx, "datum->syntax",
                            make_expression<literal_expression>(track(ctx, s)),
                            std::move(expr));
}

ptr<>
syntax_traits::unwrap(context&, ptr<syntax> stx) {
  return stx;
}

template <typename Traits>
static std::unique_ptr<qq_template>
parse_qq_template(parsing_context& pc, tracked_ptr<syntax> const& stx,
                  unsigned quote_level);

template <typename Traits>
static std::tuple<unsigned, bool>
find_nested_level(parsing_context& pc, unsigned quote_level, ptr<pair> p) {
  if (auto form = match_core_form(pc, expect<syntax>(car(p)))) {
    if (Traits::is_unquote(pc.ctx, form))
      return {quote_level - 1, false};
    else if (Traits::is_unquote_splicing(pc.ctx, form))
      return {quote_level - 1, true};
    else if (Traits::is_quasiquote(pc.ctx, form))
      return {quote_level + 1, false};
  }

  return {quote_level, false};
}

template <typename Traits>
static bool
is_improper_qq_template(parsing_context& pc, ptr<>& elem) {
  // An improper list or a list of the form (x . ,y), which is the same as
  // (x unquote y) which is a proper list, but we don't want to consider
  // it being proper here.

  return (!semisyntax_is<pair>(syntax_cdr(pc.ctx, elem))
          && !is<null_type>(syntax_cdr(pc.ctx, elem)))
         || Traits::is_qq_form(pc, syntax_cdr(pc.ctx, elem));
}

template <typename Traits>
static std::unique_ptr<qq_template>
parse_list_qq_template_body(parsing_context& pc, tracked_ptr<syntax> const& stx,
                            unsigned nested_level) {
  bool all_literal = true;
  list_pattern result;

  ptr<> elem = stx.get();
  while (!semisyntax_is<null_type>(elem)) {
    if (is_improper_qq_template<Traits>(pc, elem))
      break;

    result.elems.push_back(
      parse_qq_template<Traits>(pc,
                                track(pc.ctx, syntax_car(pc.ctx, elem)),
                                nested_level)
    );
    elem = syntax_cdr(pc.ctx, elem);

    if (!std::holds_alternative<literal>(result.elems.back()->value))
      all_literal = false;
  }

  if (auto pair = semisyntax_match<insider::pair>(pc.ctx, elem)) {
    result.last = cons_pattern{
      parse_qq_template<Traits>(pc,
                                track(pc.ctx, expect<syntax>(car(pair))),
                                nested_level),
      parse_qq_template<Traits>(pc,
                                track(pc.ctx, expect<syntax>(cdr(pair))),
                                nested_level)
    };
    all_literal = all_literal
                  && std::holds_alternative<literal>(result.last->car->value)
                  && std::holds_alternative<literal>(result.last->cdr->value);
  }

  if (all_literal)
    return std::make_unique<qq_template>(literal{stx}, stx);
  else
    return std::make_unique<qq_template>(std::move(result), stx);
}

template <typename Traits>
static std::unique_ptr<qq_template>
parse_list_qq_template(parsing_context& pc, tracked_ptr<syntax> const& stx,
                       unsigned quote_level, ptr<pair> p) {
  auto [nested_level, splicing]
    = find_nested_level<Traits>(pc, quote_level, p);
  if (nested_level == 0) {
    auto unquote_stx = track(pc.ctx, syntax_cadr(pc.ctx, stx.get()));
    return std::make_unique<qq_template>(unquote{unquote_stx, splicing},
                                         unquote_stx);
  } else
    return parse_list_qq_template_body<Traits>(pc, stx, nested_level);
}

template <typename Traits>
static std::unique_ptr<qq_template>
parse_vector_qq_template(parsing_context& pc, tracked_ptr<syntax> const& stx,
                         unsigned quote_level, ptr <vector> v) {
  std::vector<std::unique_ptr<qq_template>> templates;
  templates.reserve(v->size());
  bool all_literal = true;

  for (std::size_t i = 0; i < v->size(); ++i) {
    templates.push_back(
      parse_qq_template<Traits>(pc,
                                track(pc.ctx, expect<syntax>(v->ref(i))),
                                quote_level)
    );
    if (!std::holds_alternative<literal>(templates.back()->value))
      all_literal = false;
  }

  if (all_literal)
    return std::make_unique<qq_template>(literal{stx}, stx);
  else
    return std::make_unique<qq_template>(vector_pattern{std::move(templates)},
                                         stx);
}

template <typename Traits>
static std::unique_ptr<qq_template>
parse_qq_template(parsing_context& pc, tracked_ptr<syntax> const& stx,
                  unsigned quote_level) {
  if (auto p = syntax_match<pair>(pc.ctx, stx.get()))
    return parse_list_qq_template<Traits>(pc, stx, quote_level, p);
  else if (auto v = syntax_match<vector>(pc.ctx, stx.get()))
    return parse_vector_qq_template<Traits>(pc, stx, quote_level, v);
  else
    return std::make_unique<qq_template>(literal{stx}, stx);
}

static bool
  is_splice(std::unique_ptr<qq_template> const& tpl) {
if (auto* expr = std::get_if<unquote>(&tpl->value))
if (expr->splicing)
return true;
return false;
}

template <typename Traits>
static std::unique_ptr<expression>
process_qq_template(parsing_context& pc, std::unique_ptr<qq_template> const& tpl,
                    bool force_unwrapped = false);

template <typename Traits>
static std::unique_ptr<expression>
make_qq_tail_expression(parsing_context& pc,
                        std::unique_ptr<qq_template> const& tpl,
                        list_pattern const& lp) {
  std::unique_ptr<expression> tail;
  if (lp.last) {
    if (is_splice(lp.last->cdr))
      throw make_compile_error<syntax_error>(
        tpl->stx.get(), "Invalid use of {}", Traits::splicing_form_name
      );

    if (is_splice(lp.last->car))
      tail = make_application(
        pc.ctx, "append",
        process_qq_template<Traits>(pc, lp.last->car, true),
        process_qq_template<Traits>(pc, lp.last->cdr, true)
      );
    else
      tail = make_application(
        pc.ctx, "cons",
        process_qq_template<Traits>(pc, lp.last->car),
        process_qq_template<Traits>(pc, lp.last->cdr)
      );
  }
  return tail;
}

template <typename Traits>
static std::unique_ptr<expression>
process_qq_pattern(parsing_context& pc,
                   std::unique_ptr<qq_template> const& tpl,
                   bool force_unwrapped,
                   list_pattern const& lp) {
  auto tail = make_qq_tail_expression<Traits>(pc, tpl, lp);

  for (auto const& elem : lp.elems | std::views::reverse) {
    if (tail) {
      if (is_splice(elem))
        tail = make_application(pc.ctx, "append",
                                process_qq_template<Traits>(pc, elem, true),
                                std::move(tail));
      else
        tail = make_application(
          pc.ctx, "cons",
          process_qq_template<Traits>(pc, elem),
          std::move(tail)
        );
    } else {
      if (is_splice(elem))
        tail = process_qq_template<Traits>(pc, elem, true);
      else
        tail = make_application(
          pc.ctx, "cons",
          process_qq_template<Traits>(pc, elem),
          make_expression<literal_expression>(track(pc.ctx,
                                                    pc.ctx.constants->null))
        );
    }
  }

  assert(tail);
  return Traits::wrap(pc.ctx, tpl->stx.get(), std::move(tail),
                      force_unwrapped);
}

template <typename Traits>
static std::unique_ptr<expression>
process_qq_vector_pattern_with_splices(parsing_context& pc,
                                       std::unique_ptr<qq_template> const& tpl,
                                       bool force_unwrapped,
                                       vector_pattern const& vp) {
  std::vector<std::unique_ptr<expression>> args;
  std::vector<std::unique_ptr<expression>> chunk;
  for (std::unique_ptr<qq_template> const& elem : vp.elems) {
    if (is_splice(elem)) {
      if (!chunk.empty()) {
        args.push_back(make_application(pc.ctx, "vector", std::move(chunk)));
        chunk.clear();
      }

      args.push_back(
        make_application(pc.ctx, "list->vector",
                         process_qq_template<Traits>(pc, elem, true))
      );
    }
    else
      chunk.push_back(process_qq_template<Traits>(pc, elem, true));
  }

  if (!chunk.empty())
    args.push_back(make_application(pc.ctx, "vector", std::move(chunk)));

  auto result = make_application(pc.ctx, "vector-append", std::move(args));
  return Traits::wrap(pc.ctx, tpl->stx.get(), std::move(result),
                      force_unwrapped);
}

template <typename Traits>
static std::unique_ptr<expression>
process_qq_vector_pattern_without_splices(
  parsing_context& pc,
  std::unique_ptr<qq_template> const& tpl,
  bool force_unwrapped,
  vector_pattern const& vp
) {
  std::vector<std::unique_ptr<expression>> elements;
  elements.reserve(vp.elems.size());

  for (std::unique_ptr<qq_template> const& elem : vp.elems)
    elements.push_back(process_qq_template<Traits>(pc, elem));

  return Traits::wrap(
    pc.ctx, tpl->stx.get(),
    make_application(pc.ctx, "vector", std::move(elements)),
    force_unwrapped
  );
}

template <typename Traits>
static std::unique_ptr<expression>
process_qq_pattern(parsing_context& pc,
                   std::unique_ptr<qq_template> const& tpl,
                   bool force_unwrapped,
                   vector_pattern const& vp) {
  // If there are no unquote-splicings in the vector, we will simply construct
  // the vector from its elements. If there are unquote-splicings, we will
  // translate it to (vector-append v1 v2 ... (list->vector spliced elements)
  // v3 v4 ...).

  bool any_splices = std::ranges::any_of(vp.elems, is_splice);
  if (any_splices)
    return process_qq_vector_pattern_with_splices<Traits>(
      pc, tpl, force_unwrapped, vp
    );
  else
    return process_qq_vector_pattern_without_splices<Traits>(
      pc, tpl, force_unwrapped, vp
    );
}

template <typename Traits>
static std::unique_ptr<expression>
process_qq_pattern(parsing_context& pc,
                   std::unique_ptr<qq_template> const& tpl,
                   bool force_unwrapped,
                   unquote const& expr) {
  return Traits::wrap(
    pc.ctx, tpl->stx.get(), parse(pc, expr.datum.get()), force_unwrapped
  );
}

template <typename Traits>
static std::unique_ptr<expression>
process_qq_pattern(parsing_context& pc,
                   std::unique_ptr<qq_template> const&,
                   bool,
                   literal const& lit) {
  return make_expression<literal_expression>(
    track(pc.ctx, Traits::unwrap(pc.ctx, lit.value.get()))
  );
}

template <typename Traits>
static std::unique_ptr<expression>
process_qq_template(parsing_context& pc,
                    std::unique_ptr<qq_template> const& tpl,
                    bool force_unwrapped) {
  return std::visit(
    [&] (auto const& pattern) {
      return process_qq_pattern<Traits>(pc, tpl, force_unwrapped, pattern);
    },
    tpl->value
  );
}

static std::unique_ptr<expression>
parse_quasiquote(parsing_context& pc, ptr<syntax> stx) {
  return process_qq_template<quote_traits>(
    pc,
    parse_qq_template<quote_traits>(pc,
                                    track(pc.ctx, syntax_cadr(pc.ctx, stx)),
                                    1)
  );
}

static std::unique_ptr<expression>
parse_quasisyntax(parsing_context& pc, ptr<syntax> stx) {
  return process_qq_template<syntax_traits>(
    pc,
    parse_qq_template<syntax_traits>(pc,
                                     track(pc.ctx, syntax_cadr(pc.ctx, stx)),
                                     1)
  );
}

std::unique_ptr<expression>
parse(parsing_context& pc, ptr<syntax> s) {
  ptr<syntax> stx = expand(pc, track(pc.ctx, s)).get(); // GC

  if (syntax_is<symbol>(stx))
    return parse_reference(pc, stx);
  else if (syntax_is<pair>(stx)) {
    auto head = syntax_car(pc.ctx, stx);
    if (auto form = match_core_form(pc, head)) {
      if (form == pc.ctx.constants->let)
        return parse_let(pc, stx);
      if (form == pc.ctx.constants->letrec_star)
        return parse_letrec_star(pc, stx);
      else if (form == pc.ctx.constants->set)
        return parse_set(pc, stx);
      else if (form == pc.ctx.constants->lambda)
        return parse_lambda(pc, stx);
      else if (form == pc.ctx.constants->if_)
        return parse_if(pc, stx);
      else if (form == pc.ctx.constants->begin)
        return parse_sequence(pc, syntax_cdr(pc.ctx, stx));
      else if (form == pc.ctx.constants->define)
        return parse_define(pc, stx);
      else if (form == pc.ctx.constants->quote)
        return make_expression<literal_expression>(
          track(pc.ctx, syntax_to_datum(pc.ctx, syntax_cadr(pc.ctx, stx)))
        );
      else if (form == pc.ctx.constants->syntax)
        return make_expression<literal_expression>(
          track(pc.ctx, syntax_cadr(pc.ctx, stx))
        );
      else if (form == pc.ctx.constants->quasiquote)
        return parse_quasiquote(pc, stx);
      else if (form == pc.ctx.constants->quasisyntax)
        return parse_quasisyntax(pc, stx);
      else if (form == pc.ctx.constants->syntax_trap)
        return parse_syntax_trap(pc, stx);
      else if (form == pc.ctx.constants->syntax_error)
        return parse_syntax_error(pc, stx);
      else if (form == pc.ctx.constants->unquote)
        throw make_compile_error<syntax_error>(stx, "invalid use of unquote");
      else if (form == pc.ctx.constants->unquote_splicing)
        throw make_compile_error<syntax_error>(
          stx, "invalid use of unquote-splicing"
        );
      else if (form == pc.ctx.constants->unsyntax)
        throw make_compile_error<syntax_error>(stx, "invalid use of unsyntax");
      else if (form == pc.ctx.constants->unsyntax_splicing)
        throw make_compile_error<syntax_error>(
          stx, "invalid use of unsyntax-splicing"
        );
      else if (form == pc.ctx.constants->let_syntax)
        return parse_let_syntax(pc, stx);
      else if (form == pc.ctx.constants->letrec_syntax)
        return parse_letrec_syntax(pc, stx);
      else if (form == pc.ctx.constants->meta)
        return parse_meta(pc, stx);
      else
        assert(form != pc.ctx.constants->meta
               && form != pc.ctx.constants->define_syntax);
    }

    return parse_application(pc, stx);
  }
  else
    return make_expression<literal_expression>(
      track(pc.ctx, syntax_to_datum(pc.ctx, stx))
    );
}

static void
process_top_level_define(parsing_context& pc, tracked_ptr<syntax> const& stx) {
  if (pc.module_->get_type() == module_::type::immutable)
    throw std::runtime_error{"Can't mutate an immutable environment"};

  auto [name, expr] = parse_name_and_expr(pc, stx.get(), "define");
  auto var = lookup_variable(pc, name.get());
  if (!var) {
    auto index = pc.ctx.add_top_level(pc.ctx.constants->void_,
                                      identifier_name(name.get()));
    var = make<variable>(pc.ctx, identifier_name(name.get()), index);
    define(pc.ctx.store, name.get(), std::move(var));
  }
}

static tracked_ptr<syntax>
process_top_level_form(parsing_context& pc,
                       std::vector<tracked_ptr<syntax>>& stack,
                       tracked_ptr<syntax> const& stx) {
  if (auto lst = track(pc.ctx, syntax_to_list(pc.ctx, stx.get()))) {
    if (lst.get() == pc.ctx.constants->null)
      throw make_compile_error<syntax_error>(stx.get(), "Empty application");

    ptr<pair> p = assume<pair>(lst.get());
    if (auto form = match_core_form(pc, expect<syntax>(car(p)))) {
      if (form == pc.ctx.constants->define_syntax) {
        process_define_syntax(pc, stx.get());
        return tracked_ptr<syntax>(pc.ctx.store);
      } else if (form == pc.ctx.constants->define) {
        process_top_level_define(pc, stx);
        return stx;
      } else if (form == pc.ctx.constants->begin) {
        expand_begin(pc, stx.get(), stack);
        return tracked_ptr<syntax>(pc.ctx.store);
      } else if (form == pc.ctx.constants->meta) {
        tracked_ptr<> datum = eval_meta_expression(pc, stx.get());
        return track(pc.ctx, datum_to_syntax(pc.ctx, stx.get(), datum.get()));
      }
    }
  }

  return stx;
}

static std::vector<tracked_ptr<syntax>>
body_to_stack(std::vector<tracked_ptr<syntax>> const& body) {
  std::vector<tracked_ptr<syntax>> stack;
  stack.reserve(body.size());
  std::ranges::copy(std::views::reverse(body), std::back_inserter(stack));
  return stack;
}

static std::vector<tracked_ptr<syntax>>
process_top_level_forms(parsing_context& pc,
                        std::vector<tracked_ptr<syntax>> const& body) {
  std::vector<tracked_ptr<syntax>> stack = body_to_stack(body);
  std::vector<tracked_ptr<syntax>> result;
  while (!stack.empty()) {
    tracked_ptr<syntax> stx = expand(pc, stack.back()); // GC
    stack.pop_back();

    tracked_ptr<syntax> to_emit = process_top_level_form(pc, stack, stx);
    if (to_emit)
      result.push_back(std::move(to_emit));
  }

  return result;
}

static std::vector<tracked_ptr<syntax>>
add_module_scope_to_body(context& ctx, std::vector<ptr<syntax>> const& body,
                         ptr<scope> s) {
  std::vector<tracked_ptr<syntax>> result;
  result.reserve(body.size());

  for (ptr<syntax> e : body)
    result.push_back(track(ctx, e->add_scope(ctx.store, s)));

  return result;
}

// Gather syntax and top-level variable definitions, expand top-level macro
// uses. Adds the found top-level syntaxes and variables to the module. Returns
// a list of the expanded top-level commands.
//
// Causes a garbage collection.
std::vector<tracked_ptr<syntax>>
expand_top_level(parsing_context& pc, tracked_ptr<module_> const& m,
                 std::vector<ptr<syntax>> const& exprs) {
  auto body = add_module_scope_to_body(pc.ctx, exprs, m->scope());

  internal_definition_context_guard idc{pc};
  return process_top_level_forms(pc, body);
}

static tracked_ptr<syntax>
expand_proc(context& ctx, tracked_ptr<syntax> stx) {
  return expand(ctx, std::move(stx), nullptr);
}

void
export_parser_expander(context& ctx, ptr<module_> result) {
  define_procedure<expand_proc>(ctx, "expand", result);
}

} // namespace insider
