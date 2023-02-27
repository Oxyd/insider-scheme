#include "compiler/parser_expander.hpp"

#include "compiler/analyser.hpp"
#include "compiler/ast.hpp"
#include "compiler/compiler.hpp"
#include "compiler/parsing_context.hpp"
#include "compiler/source_location.hpp"
#include "compiler/syntax_list.hpp"
#include "context.hpp"
#include "io/write.hpp"
#include "memory/member_visitor.hpp"
#include "memory/preserve.hpp"
#include "memory/root.hpp"
#include "runtime/action.hpp"
#include "runtime/basic_types.hpp"
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

template <typename T>
static std::vector<ptr<T>>
untrack_vector(std::vector<root_ptr<T>> const& v) {
  std::vector<ptr<T>> result;
  result.reserve(v.size());
  for (auto const& value : v)
    result.push_back(value.get());
  return result;
}

namespace {
  class environment_checkpoint {
  public:
    explicit
    environment_checkpoint(parsing_context& pc)
      : pc_{pc}
      , original_size_{pc.environment.size()}
    { }

    ~environment_checkpoint() {
      assert(pc_.environment.size() >= original_size_);
      pc_.environment.resize(original_size_);
    }

    environment_checkpoint(environment_checkpoint const&) = delete;
    void operator = (environment_checkpoint const&) = delete;

  private:
    parsing_context& pc_;
    std::size_t      original_size_;
  };

  class environment_extender {
  public:
    explicit
    environment_extender(parsing_context& pc)
      : pc_{pc}
    {
      pc.environment.emplace_back();
    }

    environment_extender(parsing_context& pc,
                         std::vector<variable> extension)
      : pc_{pc}
    {
      pc.environment.push_back(std::move(extension));
    }

    environment_extender(parsing_context& pc, std::ranges::range auto extension)
      : pc_{pc}
    {
      pc.environment.push_back(std::vector(extension.begin(), extension.end()));
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
    parser_action(context& ctx, root_ptr<syntax> stx, std::string_view msg)
      : action{ctx}
      , msg_{msg}
      , stx_{std::move(stx)}
    { }

    parser_action(context& ctx, ptr<syntax> stx, std::string_view msg)
      : action{ctx}
      , msg_{msg}
      , stx_{ctx.store, stx}
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
    root_ptr<syntax> stx_;
  };
}

static parsing_context
make_meta_context(parsing_context& pc) {
  parsing_context meta{pc.state, pc.module_, pc.config, pc.origin};
  meta.is_meta = true;
  return meta;
}

static environment_extender
extend_environment(parsing_context& pc, ptr<scope> s) {
  std::vector<variable> ext;
  for (scope::binding const& b : *s)
    if (b.variable)
      ext.push_back(b.variable);

  return environment_extender{pc, std::move(ext)};
}

static void
append_to_environment(parsing_context& pc, ptr<scope> s) {
  auto& env = pc.environment.emplace_back();
  for (scope::binding const& b : *s)
    if (b.variable)
      env.push_back(b.variable);
}

static bool
is_in_scope(parsing_context& pc, variable var) {
  if (is<top_level_variable>(var))
    return true; // Globals are always in scope even when they are not imported.

  for (auto const& level : pc.environment)
    for (auto const& v : level)
      if (var == v)
        return true;

  return false;
}

static variable
lookup_variable_binding(ptr<syntax> id) {
  if (auto binding = lookup(id))
    if (binding->variable)
      return binding->variable;

  return {};
}

static void
throw_out_of_scope_error(ptr<syntax> id) {
  throw make_compile_error<out_of_scope_variable_error>(
    id, "{}: Not in scope", identifier_name(id)
  );
}

static variable
lookup_variable(parsing_context& pc, ptr<syntax> id) {
  if (auto var = lookup_variable_binding(id)) {
    if (is_in_scope(pc, var))
      return var;
    else
      throw_out_of_scope_error(id);
  }

  return {};
}

static variable
lookup_mutable_variable(parsing_context& pc, ptr<syntax> id) {
  if (auto binding = lookup(id))
    if (binding->variable) {
      auto var = binding->variable;

      if (!is_in_scope(pc, var))
        throw_out_of_scope_error(id);

      if (binding->imported)
        throw make_compile_error<immutable_binding_error>(
          id, "{}: Cannot mutate imported binding", identifier_name(id)
        );

      return var;
    }

  return {};
}

static ptr<core_form_type>
lookup_core(parsing_context& pc, ptr<syntax> id) {
  auto var = lookup_variable_binding(id);
  if (!var || !is<top_level_variable>(var))
    return {};  // Core forms are never defined in a local scope.

  ptr<> form = pc.ctx.get_top_level(assume<top_level_variable>(var)->index);
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
  return std::ranges::find(use_site_scopes, s) != use_site_scopes.end();
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
call_transformer_with_continuation_barrier(vm& state, ptr<> callable,
                                           ptr<syntax> stx) {
  ptr<> result = call_with_continuation_barrier(state, callable, {stx});
  if (auto s = match<syntax>(result))
    return s;
  else
    throw std::runtime_error{fmt::format(
      "Syntax transformer didn't return a syntax: {}",
      datum_to_string(state.ctx, result)
    )};
}

static ptr<syntax>
call_transformer(vm& state, ptr<transformer> t, ptr<syntax> stx,
                 std::vector<ptr<scope>>* use_site_scopes) {
  auto introduced_env = make<scope>(
    state.ctx, state.ctx,
    fmt::format("introduced environment for syntax expansion at {}",
                format_location(stx->location()))
  );
  stx = stx->add_scope(state.ctx.store, introduced_env);

  auto use_site_scope = make<scope>(
    state.ctx, state.ctx,
    fmt::format("use-site scope for syntax expansion at {}",
                format_location(stx->location()))
  );
  stx = stx->add_scope(state.ctx.store, use_site_scope);

  if (use_site_scopes)
    use_site_scopes->push_back(use_site_scope);

  auto p = preserve(state.ctx, introduced_env);
  ptr<syntax> result
    = call_transformer_with_continuation_barrier(state, t->callable(), stx);
  return result->flip_scope(state.ctx.store, introduced_env);
}

// If the head of the given list is bound to a transformer, run the transformer
// on the datum, and repeat.
//
// Causes a garbage collection.
static ptr<syntax>
expand(vm& state, ptr<syntax> stx, std::vector<ptr<scope>>* use_site_scopes) {
  context& ctx = state.ctx;
  parser_action a(ctx, stx, "Expanding macro use");
  auto p = preserve(ctx, stx);

  bool expanded;
  do {
    expanded = false;

    if (auto lst = syntax_match<pair>(ctx, stx)) {
      ptr<syntax> head = expect<syntax>(car(lst));
      if (is_identifier(head)) {
        if (ptr<transformer> t = lookup_transformer(head)) {
          stx = call_transformer(state, t, stx, use_site_scopes);
          expanded = true;
        }
      }
    }
  } while (expanded);

  return stx;
}

static ptr<syntax>
expand(parsing_context& pc, ptr<syntax> stx) {
  if (pc.record_use_site_scopes()) {
    ptr<syntax> result = expand(pc.state, stx, &pc.use_site_scopes.back());
    return result;
  } else
    return expand(pc.state, stx, nullptr);
}

template <auto Analyse>
static ptr<>
eval_at_expand_time(parsing_context& pc, ptr<syntax> datum) {
  auto meta_pc = make_meta_context(pc);
  auto proc = compile_syntax(meta_pc.ctx, Analyse(meta_pc, datum),
                             register_root(meta_pc.ctx, meta_pc.module_));
  return call_with_continuation_barrier(meta_pc.state, proc, {});
}

// Causes a garbage collection.
static ptr<>
eval_meta(parsing_context& pc, ptr<syntax> datum) {
  return eval_at_expand_time<analyse_meta>(pc, datum);
}

static ptr<>
eval_transformer(parsing_context& pc, ptr<syntax> datum) {
  return eval_at_expand_time<analyse_transformer>(pc, datum);
}

// Causes a garbage collection
static ptr<transformer>
make_transformer(parsing_context& pc, ptr<syntax> expr) {
  parser_action a(pc.ctx, expr, "Evaluating transformer");
  auto transformer_proc = eval_transformer(pc, expr); // GC
  return make<transformer>(pc.ctx, transformer_proc);
}

namespace {
  class body_content : public root_provider {
  public:
    struct internal_variable {
      ptr<syntax>         init;
      ptr<local_variable> var;
    };

    std::vector<ptr<syntax>>       forms;
    std::vector<internal_variable> internal_variable_defs;

    explicit
    body_content(context& ctx)
      : root_provider{ctx.store}
    { }

    void
    visit_roots(member_visitor const& f) override {
      for (ptr<syntax>& form : forms)
        f(form);

      for (internal_variable& iv : internal_variable_defs) {
        f(iv.init);
        f(iv.var);
      }
    }
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
             std::vector<ptr<syntax>>& stack) {
  ptr<> lst = syntax_to_list(pc.ctx, stx);
  std::vector<ptr<syntax>> subforms;
  for (ptr<> e : list_range{cdr(assume<pair>(lst))})
    subforms.push_back(expect<syntax>(e));

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

static ptr<>
add_list_of_scopes_to_list(context& ctx, ptr<> x,
                           std::vector<ptr<scope>> const& scopes) {
  for (ptr<scope> s : scopes)
    x = add_scope_to_list(ctx, x, s);
  return x;
}

static ptr<syntax>
add_list_of_scopes(context& ctx, ptr<syntax> stx,
                   std::vector<ptr<scope>> const& scopes) {
  for (ptr<scope> s : scopes)
    stx = stx->add_scope(ctx.store, s);
  return stx;
}

static std::tuple<ptr<syntax>, ptr<syntax>>
parse_name_and_expr(parsing_context& pc, ptr<syntax> stx,
                    std::string const& form_name) {
  ptr<> datum = syntax_to_list(pc.ctx, stx);
  auto name = expect_id(pc.ctx, expect<syntax>(cadr(assume<pair>(datum))));
  auto name_without_scope = maybe_remove_use_site_scopes(pc, name);
  auto expr = expect<syntax>(caddr(assume<pair>(datum)));
  if (!datum || list_length(datum) != 3)
    throw make_compile_error<syntax_error>(stx, "Invalid {} syntax", form_name);
  return {name_without_scope, expr};
}

static void
process_internal_define(
  parsing_context& pc,
  std::vector<body_content::internal_variable>& internal_variables,
  ptr<syntax> expr
) {
  auto [id, init] = parse_name_and_expr(pc, expr, "define");
  auto var = make<local_variable>(pc.ctx, identifier_name(id));
  internal_variables.emplace_back(
    body_content::internal_variable{init, var}
  );
  define(pc.ctx.store, id, var);
  pc.environment.back().emplace_back(var);
}

static void
process_define_syntax(parsing_context& pc, ptr<syntax> stx) {
  if (pc.module_->get_type() == module_::type::immutable)
    throw std::runtime_error{"Can't mutate an immutable environment"};

  auto [name, expr] = parse_name_and_expr(pc, stx, "define-syntax");
  auto p = preserve(pc.ctx, name);

  auto new_tr = make_transformer(pc, expr); // GC
  if (lookup_transformer(name))
    redefine(pc.ctx.store, name, new_tr);
  else
    define(pc.ctx.store, name, new_tr);
}

static ptr<>
eval_meta_expression(parsing_context& pc, ptr<syntax> stx) {
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
process_internal_form(parsing_context& pc, body_content& result,
                      ptr<syntax> expr, ptr<core_form_type> form,
                      std::vector<insider::ptr<syntax>>& stack) {
  if (form == pc.ctx.constants->define_syntax)
    process_define_syntax(pc, expr);
  else if (form == pc.ctx.constants->define)
    process_internal_define(pc, result.internal_variable_defs, expr);
  else if (form == pc.ctx.constants->begin)
    expand_begin(pc, expr, stack);
  else if (form == pc.ctx.constants->meta)
    eval_meta_expression(pc, expr);
  else {
    result.forms.push_back(expr);
    return true;
  }

  return false;
}

static bool
process_stack_of_internal_defines(parsing_context& pc, body_content& result,
                                  std::vector<ptr<syntax>> stack) {
  environment_extender internal_env{pc};
  internal_definition_context_guard idc{pc};
  auto p = preserve(pc.ctx, stack);

  bool seen_expression = false;
  while (!stack.empty()) {
    ptr<syntax> expr = expand(pc, stack.back()); // GC
    stack.pop_back();

    ptr<core_form_type> form = match_core_form_in_head_of_list(pc, expr);
    if (is_definition_form(pc.ctx, form) && seen_expression)
      throw make_compile_error<syntax_error>(expr,
                                             "{} after a nondefinition",
                                             form->name);

    seen_expression = process_internal_form(pc, result, expr, form, stack);
  }

  return seen_expression;
}

static std::vector<ptr<syntax>>
body_expressions_to_stack(parsing_context& pc, ptr<> body_exprs,
                          source_location const& loc) {
  ptr<> list = syntax_to_list(pc.ctx, body_exprs);
  if (!list)
    throw make_compile_error<syntax_error>(loc, "Expected list of expressions");

  std::vector<ptr<syntax>> stack;
  for (ptr<> e : list_range{list})
    stack.push_back(expect<syntax>(e));
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
  body_content result{pc.ctx};
  bool seen_expression
    = process_stack_of_internal_defines(
        pc, result, body_expressions_to_stack(pc, body_exprs, loc)
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

static std::vector<expression>
parse_expression_list(parsing_context& pc,
                      std::vector<ptr<syntax>> const& exprs) {
  std::vector<expression> result;
  auto p = preserve(pc.ctx, result);

  result.reserve(exprs.size());
  for (ptr<syntax> e : exprs)
    result.push_back(parse(pc, e));

  return result;
}

static auto
extract_definition_pairs_for_internal_definitions(parsing_context& pc,
                                                  body_content const& content) {
  std::vector<definition_pair_expression> definition_exprs;

  for (auto const& [init, var] : content.internal_variable_defs) {
    auto void_expr = make<literal_expression>(
      pc.ctx, pc.ctx.constants->void_
    );
    definition_exprs.emplace_back(var, void_expr);
  }

  return definition_exprs;
}

static std::vector<expression>
make_internal_definition_set_expressions(
  parsing_context& pc,
  body_content const& content,
  std::vector<definition_pair_expression> const& definition_exprs
) {
  std::vector<expression> result;
  auto p1 = preserve(pc.ctx, result);

  for (std::size_t i = 0; i < definition_exprs.size(); ++i) {
    ptr<local_variable> var = content.internal_variable_defs[i].var;
    ptr<syntax> init_stx = content.internal_variable_defs[i].init;

    auto p2 = preserve(pc.ctx, var);
    expression init_expr = parse(pc, init_stx);
    result.emplace_back(make<local_set_expression>(pc.ctx, var, init_expr));
  }
  return result;
}

static ptr<sequence_expression>
parse_body_with_internal_definitions(parsing_context& pc,
                                     body_content const& content) {
  auto definition_exprs
    = extract_definition_pairs_for_internal_definitions(pc, content);
  environment_extender subenv{
    pc,
    content.internal_variable_defs
      | std::views::transform(&body_content::internal_variable::var)
      | std::views::transform([] (ptr<local_variable> v) -> variable {
          return v;
        })
  };

  auto p1 = preserve(pc.ctx, definition_exprs);
  std::vector<expression> set_exprs
    = make_internal_definition_set_expressions(pc, content, definition_exprs);

  auto p2 = preserve(pc.ctx, set_exprs);
  auto body_exprs = parse_expression_list(pc, content.forms);

  return make<sequence_expression>(
    pc.ctx,
    std::vector{
      make<let_expression>(
        pc.ctx,
        std::move(definition_exprs),
        make<sequence_expression>(
          pc.ctx,
          std::array{std::move(set_exprs), std::move(body_exprs)}
            | std::views::join
        )
      )
    }
  );
}

static ptr<sequence_expression>
parse_body(parsing_context& pc, ptr<> data, source_location const& loc) {
  body_content content = process_internal_defines(pc, data, loc); // GC

  if (!content.internal_variable_defs.empty())
    return parse_body_with_internal_definitions(pc, content);
  else
    return make<sequence_expression>(
      pc.ctx,
      parse_expression_list(pc, content.forms)
    );
}

namespace {
  struct definition_pair {
    ptr<syntax> id;
    ptr<syntax> expression;

    void
    visit_members(member_visitor const& f) const {
      f(id);
      f(expression);
    }
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

  return {id, expect<syntax>(cadr(assume<pair>(datum)))};
}

static auto
parse_let_common(parsing_context& pc, ptr<syntax> stx,
                 std::string_view form_name) {
  source_location loc = stx->location();
  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (!datum || list_length(datum) < 3)
    throw make_compile_error<syntax_error>(stx, "Invalid {} syntax", form_name);

  ptr<syntax> bindings_stx = expect<syntax>(cadr(assume<pair>(datum)));
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

  ptr<> body = cddr(expect<pair>(datum));
  return std::tuple{definitions, body};
}

static ptr<let_expression>
parse_let(parsing_context& pc, ptr<syntax> stx) {
  using namespace std::literals;
  auto [definitions, body] = parse_let_common(pc, stx, "let"sv);

  auto subscope = make<scope>(
    pc.ctx, pc.ctx,
    fmt::format("let body at {}", format_location(stx->location()))
  );

  std::vector<definition_pair_expression> definition_exprs;
  auto p = preserve(pc.ctx, stx, definitions, body, subscope, definition_exprs);

  for (definition_pair const& dp : definitions) {
    ptr<syntax> id = dp.id->add_scope(pc.ctx.store, subscope);
    auto var = make<local_variable>(pc.ctx, identifier_name(id));
    define(pc.ctx.store, id, var);
    auto q = preserve(pc.ctx, id, var);

    expression init_expr = parse(pc, dp.expression);
    definition_exprs.emplace_back(var, init_expr);
  }

  ptr<> body_with_scope = add_scope_to_list(pc.ctx, body, subscope);
  auto subenv = extend_environment(pc, subscope);
  auto body_stx = parse_body(pc, body_with_scope, stx->location());

  return make<let_expression>(pc.ctx, definition_exprs, body_stx);
}

static ptr<sequence_expression>
parse_let_syntax(parsing_context& pc, ptr<syntax> stx) {
  using namespace std::literals;
  source_location loc = stx->location();

  auto [definitions, body] = parse_let_common(pc, stx, "let-syntax"sv);

  auto subscope = make<scope>(
    pc.ctx, pc.ctx,
    fmt::format("body scope for let-syntax at {}",
                format_location(loc))
  );
  auto p = preserve(pc.ctx, definitions, body, subscope);

  for (definition_pair const& dp : definitions) {
    root_ptr<syntax> id{pc.ctx.store, dp.id->add_scope(pc.ctx.store, subscope)};

    auto transformer = make_transformer(pc, dp.expression); // GC
    define(pc.ctx.store, id.get(), transformer);
  }

  ptr<> body_with_scope = add_scope_to_list(pc.ctx, body, subscope);
  auto subenv = extend_environment(pc, subscope);
  return parse_body(pc, body_with_scope, loc);
}

static ptr<sequence_expression>
parse_letrec_syntax(parsing_context& pc, ptr<syntax> stx) {
  using namespace std::literals;
  source_location loc = stx->location();

  auto [definitions, body] = parse_let_common(pc, stx, "letrec-syntax"sv);

  auto subscope
    = make<scope>(pc.ctx, pc.ctx,
                  fmt::format("body scope for letrec-syntax at {}",
                              format_location(loc)));

  auto p = preserve(pc.ctx, definitions, body, subscope);

  for (definition_pair const& dp : definitions) {
    root_ptr<syntax> id{pc.ctx.store, dp.id->add_scope(pc.ctx.store, subscope)};
    ptr<syntax> expression
      = dp.expression->add_scope(pc.ctx.store, subscope);
    auto transformer = make_transformer(pc, expression);
    define(pc.ctx.store, id.get(), transformer);
  }

  ptr<> body_with_scope = add_scope_to_list(pc.ctx, body, subscope);
  auto subenv = extend_environment(pc, subscope);

  return parse_body(pc, body_with_scope, loc);
}

static lambda_expression::parameter
define_lambda_parameter(parsing_context& pc, ptr<syntax> name,
                        ptr<scope> subscope, bool optional) {
  auto name_with_scope = name->add_scope(pc.ctx.store, subscope);
  auto var = make<local_variable>(pc.ctx, identifier_name(name_with_scope));
  define(pc.ctx.store, name_with_scope, var);
  return {.variable = var, .optional = optional};
}

static lambda_expression::parameter
parse_required_lambda_parameter(parsing_context& pc, ptr<syntax> name,
                                ptr<scope> subscope) {
  return define_lambda_parameter(pc, name, subscope, false);
}

static std::tuple<lambda_expression::parameter, ptr<syntax>, ptr<syntax>>
parse_optional_lambda_parameter(parsing_context& pc, ptr<syntax> param,
                                ptr<scope> subscope) {
  ptr<> lst = syntax_to_list(pc.ctx, param);
  if (!lst || list_length(lst) != 2)
    throw make_compile_error<syntax_error>(param,
                                           "Invalid optional parameter syntax");

  auto name = expect<syntax>(car(assume<pair>(lst)));
  auto default_expr = expect<syntax>(cadr(assume<pair>(lst)));

  return {define_lambda_parameter(pc, name, subscope, true), default_expr, name};
}

static std::tuple<lambda_expression::parameter, ptr<syntax>, ptr<syntax>>
parse_positional_lambda_parameter(parsing_context& pc, ptr<syntax> param,
                                  ptr<scope> subscope) {
  if (syntax_is<pair>(param))
    return parse_optional_lambda_parameter(pc, param, subscope);
  else {
    auto id = expect_id(pc.ctx, param);
    return {parse_required_lambda_parameter(pc,
                                            id,
                                            subscope),
            {},
            id};
  }
}

namespace {
  struct parsed_parameter {
    lambda_expression::parameter param;
    ptr<keyword>                 name;
    ptr<syntax>                  default_expr;
    ptr<syntax>                  id;

    void
    visit_members(member_visitor const& f) const {
      param.visit_members(f);
      f(name);
      f(default_expr);
      f(id);
    }
  };
}

static std::tuple<parsed_parameter, ptr<>>
parse_lambda_parameter(parsing_context& pc, ptr<pair> params,
                       ptr<scope> subscope) {
  auto param_stx = expect<syntax>(car(params));
  if (param_stx->contains<keyword>()) {
    auto kw = assume<keyword>(param_stx->get_expression_without_update());
    param_stx = syntax_cadr(pc.ctx, params);
    auto [param, default_expr, id]
      = parse_positional_lambda_parameter(pc, param_stx, subscope);
    return {{param, kw, default_expr, id},
            syntax_cdr(pc.ctx, syntax_cdr(pc.ctx, params))};
  } else {
    auto [param, default_expr, id]
      = parse_positional_lambda_parameter(pc, param_stx, subscope);
    return {{param, {}, default_expr, id},
            syntax_cdr(pc.ctx, params)};
  }
}

static auto
parse_lambda_parameters(parsing_context& pc, ptr<syntax> param_stx,
                        source_location const& loc) {
  ptr<> param_names = param_stx;
  std::vector<parsed_parameter> parameters;

  auto subscope = make<scope>(
    pc.ctx, pc.ctx,
    fmt::format("lambda body at {}", format_location(loc))
  );

  bool has_rest = false;
  bool has_optional = false;
  while (!semisyntax_is<null_type>(param_names)) {
    if (auto param = semisyntax_match<pair>(pc.ctx, param_names)) {
      auto [p, rest] = parse_lambda_parameter(pc, param, subscope);
      parameters.push_back(p);

      if (has_optional && !p.param.optional)
        throw make_compile_error<syntax_error>(
          expect<syntax>(car(param)),
          "Required parameter after optional"
        );

      has_optional = has_optional || p.param.optional;
      param_names = rest;
    } else if (semisyntax_is<symbol>(param_names)) {
      auto id = assume<syntax>(param_names);
      auto p = parse_required_lambda_parameter(pc, id, subscope);
      parameters.push_back({p, {}, {}, id});
      has_rest = true;
      break;
    } else
      throw make_compile_error<syntax_error>(
        param_stx, "Unexpected value in lambda parameters: {}",
        datum_to_string(pc.ctx, param_names)
      );
  }

  return std::tuple{subscope, parameters, has_rest};
}

static void
throw_if_duplicate_keyword_parameter(
  std::vector<parsed_parameter> const& params,
  ptr<syntax> stx
) {
  // Quadratic algorithm to prevent allocation. Parameter counts will be at most
  // a few hundred in extreme cases, so it'll be fine.

  for (auto p = params.begin(); p != params.end(); ++p)
    if (p->name)
      if (std::find_if(std::next(p), params.end(),
                       [&] (auto const& param) {
                         return param.name == p->name;
                       }) != params.end())
        throw make_compile_error<syntax_error>(stx,
                                               "Duplicate keyword parameter");
}

static std::vector<lambda_expression::parameter>
make_parameters(std::vector<parsed_parameter> const& params) {
  std::vector<lambda_expression::parameter> result;
  result.reserve(params.size());
  for (auto const& p : params)
    result.push_back(p.param);
  return result;
}

static std::vector<ptr<keyword>>
make_parameter_names(std::vector<parsed_parameter> const& params) {
  std::vector<ptr<keyword>> result;
  result.reserve(params.size());
  for (auto const& p : params)
    result.push_back(p.name);
  return result;
}

namespace {
  struct lambda_default_expressions {
    std::vector<definition_pair_expression> dps;
    std::vector<ptr<scope>>                 scopes;

    void
    visit_members(member_visitor const& f) const {
      for (definition_pair_expression const& dp : dps)
        dp.visit_members(f);
      for (ptr<scope> const& s : scopes)
        f(s);
    }
  };
}

static ptr<if_expression>
make_default_init_expression(context& ctx, ptr<local_variable> var,
                             expression default_expr) {
  return make<if_expression>(
    ctx,
    make_application(
      ctx, "eq?",
      make<local_reference_expression>(ctx, var),
      make<literal_expression>(ctx, ctx.constants->default_value)
    ),
    default_expr,
    make<local_reference_expression>(ctx, var)
  );
}

static lambda_default_expressions
make_lambda_default_expressions(parsing_context& pc,
                                ptr<scope> params_scope,
                                std::vector<parsed_parameter> const& params,
                                source_location const& loc) {
  lambda_default_expressions result;
  result.scopes.push_back(params_scope);
  auto p = preserve(pc.ctx, result);

  for (parsed_parameter const& param : params)
    if (param.default_expr) {
      auto subscope = make<scope>(
        pc.ctx, pc.ctx,
        fmt::format("default parameter {} for lambda at {}",
                    identifier_name(param.id),
                    format_location(loc))
      );
      result.scopes.push_back(subscope);

      ptr<syntax> id = add_list_of_scopes(pc.ctx, param.id, result.scopes);
      auto var = make<local_variable>(pc.ctx, identifier_name(id));
      define(pc.ctx.store, id, var);
      auto q = preserve(pc.ctx, var);

      ptr<syntax> default_stx
        = add_list_of_scopes(pc.ctx, param.default_expr, result.scopes);
      expression default_expr = parse(pc, default_stx);

      expression init_expr = make_default_init_expression(pc.ctx,
                                                          param.param.variable,
                                                          default_expr);

      result.dps.emplace_back(var, init_expr);

      append_to_environment(pc, subscope);
    }

  return result;
}

static expression
make_defaults_let(context& ctx,
                  std::vector<definition_pair_expression> const& dps,
                  expression body) {
  for (definition_pair_expression const& dp : dps | std::views::reverse)
    body = make<let_expression>(ctx, std::vector{dp}, body);
  return body;
}

static expression
parse_lambda_body(parsing_context& pc, ptr<> body_stx, ptr<scope> params_scope,
                  std::vector<parsed_parameter> const& params,
                  source_location const& loc) {
  environment_checkpoint cp{pc};
  append_to_environment(pc, params_scope);

  lambda_default_expressions defaults
    = make_lambda_default_expressions(pc, params_scope, params, loc);
  auto p = preserve(pc.ctx, defaults);

  ptr<> body_with_scope = add_list_of_scopes_to_list(pc.ctx, body_stx,
                                                     defaults.scopes);
  expression body = parse_body(pc, body_with_scope, loc);
  return make_defaults_let(pc.ctx, defaults.dps, body);
}

static ptr<lambda_expression>
parse_lambda(parsing_context& pc, ptr<syntax> stx) {
  source_location loc = stx->location();

  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (!datum || cdr(assume<pair>(datum)) == pc.ctx.constants->null)
    throw make_compile_error<syntax_error>(stx, "Invalid lambda syntax");

  auto [subscope, parameters, has_rest]
    = parse_lambda_parameters(pc, expect<syntax>(cadr(assume<pair>(datum))),
                              loc);

  auto p = preserve(pc.ctx, parameters);

  throw_if_duplicate_keyword_parameter(parameters, stx);

  ptr<> body_stx = cddr(assume<pair>(datum));
  expression body = parse_lambda_body(pc, body_stx, subscope, parameters, loc);

  return make<lambda_expression>(
    pc.ctx,
    pc.ctx,
    make_parameters(parameters),
    make_parameter_names(parameters),
    has_rest,
    body,
    fmt::format("<lambda at {}>", format_location(loc)),
    std::vector<ptr<local_variable>>{}
  );
}

static ptr<if_expression>
parse_if(parsing_context& pc, ptr<syntax> stx) {
  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (list_length(datum) != 3 && list_length(datum) != 4)
    throw make_compile_error<syntax_error>(stx, "Invalid if syntax");
  ptr<pair> list = assume<pair>(datum);

  ptr<syntax> test_stx = expect<syntax>(cadr(list));
  ptr<syntax> then_stx = expect<syntax>(caddr(list));
  ptr<syntax> else_stx;
  if (cdddr(list) != pc.ctx.constants->null)
    else_stx = expect<syntax>(cadddr(list));

  expression test_expr;
  expression then_expr;
  expression else_expr;
  auto p = preserve(pc.ctx, test_stx, then_stx, else_stx,
                    test_expr, then_expr, else_expr);

  test_expr = parse(pc, test_stx);
  then_expr = parse(pc, then_stx);
  if (else_stx)
    else_expr = parse(pc, else_stx);
  else
    else_expr = make<literal_expression>(pc.ctx,
                                         pc.ctx.constants->void_);

  return make<if_expression>(pc.ctx, test_expr, then_expr, else_expr);
}

static std::tuple<expression, ptr<keyword>, ptr<>>
parse_keyword_argument(parsing_context& pc, ptr<pair> args_expr) {
  auto name_stx = assume<syntax>(car(args_expr));
  auto name = syntax_assume_without_update<keyword>(name_stx);

  if (cdr(args_expr) == pc.ctx.constants->null)
    throw make_compile_error<syntax_error>(name_stx,
                                           "Keyword not followed by expression");

  auto arg_expr = expect<syntax>(cadr(args_expr));

  auto p = preserve(pc.ctx, name, args_expr);
  return {parse(pc, arg_expr), name, cddr(args_expr)};
}

static std::tuple<expression, ptr<keyword>, ptr<>>
parse_positional_argument(parsing_context& pc, ptr<pair> args_expr) {
  auto p = preserve(pc.ctx, args_expr);
  auto arg_expr = expect<syntax>(car(args_expr));
  return {parse(pc, arg_expr), {}, cdr(args_expr)};
}

static std::tuple<expression, ptr<keyword>, ptr<>>
parse_argument(parsing_context& pc, ptr<pair> args_expr) {
  auto expr = expect<syntax>(car(args_expr));
  if (expr->contains<keyword>())
    return parse_keyword_argument(pc, args_expr);
  else
    return parse_positional_argument(pc, args_expr);
}

static ptr<application_expression>
parse_application(parsing_context& pc, ptr<syntax> stx) {
  auto datum = syntax_to_list(pc.ctx, stx);
  if (!datum)
    throw make_compile_error<syntax_error>(stx, "Invalid function call syntax");

  expression target_expr
    = parse(pc, expect<syntax>(car(assume<pair>(datum))));

  std::vector<expression> arguments;
  std::vector<ptr<keyword>> argument_names;
  auto args_expr = cdr(assume<pair>(datum));
  auto p = preserve(pc.ctx, stx, datum, target_expr, arguments, argument_names,
                    args_expr);

  while (args_expr != pc.ctx.constants->null) {
    auto [arg, name, rest] = parse_argument(pc, assume<pair>(args_expr));
    arguments.push_back(arg);
    argument_names.push_back(name);
    args_expr = rest;
  }

  return make<application_expression>(pc.ctx, stx->location(), target_expr,
                                      arguments, argument_names);
}

static expression
parse_sequence(parsing_context& pc, ptr<> stx) {
  std::vector<expression> exprs;
  auto p = preserve(pc.ctx, stx, exprs);

  while (!semisyntax_is<null_type>(stx)) {
    exprs.push_back(parse(pc, syntax_car(pc.ctx, stx)));
    stx = syntax_cdr(pc.ctx, stx);
  }

  return make<sequence_expression>(pc.ctx, exprs);
}

static expression
parse_unknown_reference_in_interactive_module(parsing_context& pc,
                                              ptr<syntax> id) {
  return make<unknown_reference_expression>(pc.ctx, id);
}

static expression
parse_unknown_reference(parsing_context& pc, ptr<syntax> id) {
  if (pc.module_->get_type() == module_::type::interactive)
    return parse_unknown_reference_in_interactive_module(pc, id);
  else
    throw make_compile_error<unbound_variable_error>(
      id, "Identifier {} not bound to a variable", identifier_name(id)
    );
}

static expression
parse_reference(parsing_context& pc, ptr<syntax> id) {
  auto var = lookup_variable(pc, id);

  if (!var)
    return parse_unknown_reference(pc, id);
  else if (auto local_var = match<local_variable>(var))
    return make<local_reference_expression>(pc.ctx, local_var);
  else
    return make<top_level_reference_expression>(
      pc.ctx, assume<top_level_variable>(var)
    );
}

static expression
make_set_expression(parsing_context& pc, ptr<syntax> name,
                    ptr<syntax> expr, variable var, bool is_initialisation) {
  auto p = preserve(pc.ctx, name, var);

  auto value_expr = parse(pc, expr);
  if (auto l = match<lambda_expression>(value_expr))
    l->set_name(identifier_name(name));

  if (auto local_var = match<local_variable>(var))
    return make<local_set_expression>(pc.ctx, local_var, value_expr);
  else
    return make<top_level_set_expression>(pc.ctx,
                                          assume<top_level_variable>(var),
                                          value_expr,
                                          is_initialisation);
}

static expression
parse_define(ptr<syntax> stx) {
  throw make_compile_error<syntax_error>(stx, "Unexpected define");
}

static expression
parse_init(parsing_context& pc, ptr<syntax> stx) {
  auto [name, expr] = parse_name_and_expr(pc, stx, "define");

  auto var = lookup_mutable_variable(pc, name);
  assert(var);

  return make_set_expression(pc, name, expr, var, true);
}

static expression
parse_set(parsing_context& pc, ptr<syntax> stx) {
  auto [name, expr] = parse_name_and_expr(pc, stx, "set!");

  auto var = lookup_mutable_variable(pc, name);
  if (!var)
    throw make_compile_error<unbound_variable_error>(
      name, "Identifier {} not bound to a variable", identifier_name(name)
    );

  return make_set_expression(pc, name, expr, var, false);
}

static expression
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

static expression
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

static expression
parse_meta(parsing_context& pc, ptr<syntax> stx) {
  ptr<> datum = eval_meta_expression(pc, stx);
  return make<literal_expression>(pc.ctx, datum);
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
    root_ptr<syntax> value;
  };

  struct unquote {
    root_ptr<syntax> datum;
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
    root_ptr<syntax> stx;

    qq_template(value_type value, root_ptr<syntax> const& stx)
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

    static expression
    wrap(context&, ptr<syntax>, expression expr, bool force_unwrapped);

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

    static expression
    wrap(context&, ptr<syntax>, expression expr, bool force_unwrapped);

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

expression
quote_traits::wrap(context&, ptr<syntax>, expression expr, bool) {
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

expression
syntax_traits::wrap(context& ctx, ptr<syntax> s, expression expr,
                    bool force_unwrapped) {
  if (force_unwrapped)
    return expr;
  else
    return make_application(ctx, "datum->syntax",
                            make<literal_expression>(ctx, s),
                            expr);
}

ptr<>
syntax_traits::unwrap(context&, ptr<syntax> stx) {
  return stx;
}

template <typename Traits>
static std::unique_ptr<qq_template>
parse_qq_template(parsing_context& pc, root_ptr<syntax> const& stx,
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
parse_list_qq_template_body(parsing_context& pc, root_ptr<syntax> const& stx,
                            unsigned nested_level) {
  bool all_literal = true;
  list_pattern result;

  ptr<> elem = stx.get();
  while (!semisyntax_is<null_type>(elem)) {
    if (is_improper_qq_template<Traits>(pc, elem))
      break;

    result.elems.push_back(
      parse_qq_template<Traits>(pc,
                                register_root(pc.ctx, syntax_car(pc.ctx, elem)),
                                nested_level)
    );
    elem = syntax_cdr(pc.ctx, elem);

    if (!std::holds_alternative<literal>(result.elems.back()->value))
      all_literal = false;
  }

  if (auto pair = semisyntax_match<insider::pair>(pc.ctx, elem)) {
    result.last = cons_pattern{
      parse_qq_template<Traits>(pc,
                                register_root(pc.ctx, expect<syntax>(car(pair))),
                                nested_level),
      parse_qq_template<Traits>(pc,
                                register_root(pc.ctx, expect<syntax>(cdr(pair))),
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
parse_list_qq_template(parsing_context& pc, root_ptr<syntax> const& stx,
                       unsigned quote_level, ptr<pair> p) {
  auto [nested_level, splicing]
    = find_nested_level<Traits>(pc, quote_level, p);
  if (nested_level == 0) {
    auto unquote_stx = register_root(pc.ctx, syntax_cadr(pc.ctx, stx.get()));
    return std::make_unique<qq_template>(unquote{unquote_stx, splicing},
                                         unquote_stx);
  } else
    return parse_list_qq_template_body<Traits>(pc, stx, nested_level);
}

template <typename Traits>
static std::unique_ptr<qq_template>
parse_vector_qq_template(parsing_context& pc, root_ptr<syntax> const& stx,
                         unsigned quote_level, ptr <vector> v) {
  std::vector<std::unique_ptr<qq_template>> templates;
  templates.reserve(v->size());
  bool all_literal = true;

  for (std::size_t i = 0; i < v->size(); ++i) {
    templates.push_back(
      parse_qq_template<Traits>(pc,
                                register_root(pc.ctx, expect<syntax>(v->ref(i))),
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
parse_qq_template(parsing_context& pc, root_ptr<syntax> const& stx,
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
static expression
process_qq_template(parsing_context& pc, std::unique_ptr<qq_template> const& tpl,
                    bool force_unwrapped = false);

template <typename Traits>
static root<expression>
make_qq_tail_expression(parsing_context& pc,
                        std::unique_ptr<qq_template> const& tpl,
                        list_pattern const& lp) {
  root<expression> tail{pc.ctx.store};
  if (lp.last) {
    if (is_splice(lp.last->cdr))
      throw make_compile_error<syntax_error>(
        tpl->stx.get(), "Invalid use of {}", Traits::splicing_form_name
      );

    if (is_splice(lp.last->car)) {
      root<expression> car{
        pc.ctx.store, process_qq_template<Traits>(pc, lp.last->car, true)
      };
      root<expression> cdr{
        pc.ctx.store, process_qq_template<Traits>(pc, lp.last->cdr, true)
      };
      tail = make_application(pc.ctx, "append", car.get(), cdr.get());
    } else {
      root<expression> car{
        pc.ctx.store, process_qq_template<Traits>(pc, lp.last->car)
      };
      root<expression> cdr{
        pc.ctx.store, process_qq_template<Traits>(pc, lp.last->cdr)
      };
      tail = make_application(pc.ctx, "cons", car.get(), cdr.get());
    }
  }
  return tail;
}

template <typename Traits>
static expression
process_qq_pattern(parsing_context& pc,
                   std::unique_ptr<qq_template> const& tpl,
                   bool force_unwrapped,
                   list_pattern const& lp) {
  root<expression> tail = make_qq_tail_expression<Traits>(pc, tpl, lp);

  for (auto const& elem : lp.elems | std::views::reverse) {
    if (tail.get()) {
      if (is_splice(elem))
        tail = make_application(pc.ctx, "append",
                                process_qq_template<Traits>(pc, elem, true),
                                tail.get());
      else
        tail = make_application(
          pc.ctx, "cons",
          process_qq_template<Traits>(pc, elem),
          tail.get()
        );
    } else {
      if (is_splice(elem))
        tail = process_qq_template<Traits>(pc, elem, true);
      else {
        root<expression> car{pc.ctx.store,
                             process_qq_template<Traits>(pc, elem)};
        tail = make_application(
          pc.ctx, "cons",
          car.get(),
          make<literal_expression>(pc.ctx, pc.ctx.constants->null)
        );
      }
    }
  }

  assert(tail.get());
  return Traits::wrap(pc.ctx, tpl->stx.get(), tail.get(),
                      force_unwrapped);
}

template <typename Traits>
static expression
process_qq_vector_pattern_with_splices(parsing_context& pc,
                                       std::unique_ptr<qq_template> const& tpl,
                                       bool force_unwrapped,
                                       vector_pattern const& vp) {
  std::vector<expression> args;
  std::vector<expression> chunk;
  for (std::unique_ptr<qq_template> const& elem : vp.elems) {
    if (is_splice(elem)) {
      if (!chunk.empty()) {
        args.emplace_back(make_application(pc.ctx, "vector", std::move(chunk)));
        chunk.clear();
      }

      args.emplace_back(
        make_application(pc.ctx, "list->vector",
                         process_qq_template<Traits>(pc, elem, true))
      );
    }
    else
      chunk.emplace_back(process_qq_template<Traits>(pc, elem, true));
  }

  if (!chunk.empty())
    args.emplace_back(make_application(pc.ctx, "vector", std::move(chunk)));

  auto result = make_application(pc.ctx, "vector-append", std::move(args));
  return Traits::wrap(pc.ctx, tpl->stx.get(), result, force_unwrapped);
}

template <typename Traits>
static expression
process_qq_vector_pattern_without_splices(
  parsing_context& pc,
  std::unique_ptr<qq_template> const& tpl,
  bool force_unwrapped,
  vector_pattern const& vp
) {
  std::vector<expression> elements;
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
static expression
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
static expression
process_qq_pattern(parsing_context& pc,
                   std::unique_ptr<qq_template> const& tpl,
                   bool force_unwrapped,
                   unquote const& expr) {
  return Traits::wrap(
    pc.ctx, tpl->stx.get(), parse(pc, expr.datum.get()), force_unwrapped
  );
}

template <typename Traits>
static expression
process_qq_pattern(parsing_context& pc,
                   std::unique_ptr<qq_template> const&,
                   bool,
                   literal const& lit) {
  return make<literal_expression>(
    pc.ctx, Traits::unwrap(pc.ctx, lit.value.get())
  );
}

template <typename Traits>
static expression
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

static expression
parse_quasiquote(parsing_context& pc, ptr<syntax> stx) {
  return process_qq_template<quote_traits>(
    pc,
    parse_qq_template<quote_traits>(
      pc,
      register_root(pc.ctx, syntax_cadr(pc.ctx, stx)),
      1
    )
  );
}

static expression
parse_quasisyntax(parsing_context& pc, ptr<syntax> stx) {
  return process_qq_template<syntax_traits>(
    pc,
    parse_qq_template<syntax_traits>(
      pc,
      register_root(pc.ctx, syntax_cadr(pc.ctx, stx)),
      1
    )
  );
}

static bool
is_self_quoting(ptr<> value) {
  return !is<keyword>(value);
}

expression
parse(parsing_context& pc, ptr<syntax> s) {
  ptr<syntax> stx = expand(pc, s); // GC

  if (syntax_is<symbol>(stx))
    return parse_reference(pc, stx);
  else if (syntax_is<pair>(stx)) {
    auto head = syntax_car(pc.ctx, stx);
    if (auto form = match_core_form(pc, head)) {
      if (form == pc.ctx.constants->let)
        return parse_let(pc, stx);
      else if (form == pc.ctx.constants->set)
        return parse_set(pc, stx);
      else if (form == pc.ctx.constants->lambda)
        return parse_lambda(pc, stx);
      else if (form == pc.ctx.constants->if_)
        return parse_if(pc, stx);
      else if (form == pc.ctx.constants->begin)
        return parse_sequence(pc, syntax_cdr(pc.ctx, stx));
      else if (form == pc.ctx.constants->define)
        return parse_define(stx);
      else if (form == pc.ctx.constants->init)
        return parse_init(pc, stx);
      else if (form == pc.ctx.constants->quote)
        return make<literal_expression>(
          pc.ctx, syntax_to_datum(pc.ctx, syntax_cadr(pc.ctx, stx))
        );
      else if (form == pc.ctx.constants->syntax)
        return make<literal_expression>(pc.ctx, syntax_cadr(pc.ctx, stx));
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
  else if (is_self_quoting(stx->get_expression_without_update()))
    return make<literal_expression>(
      pc.ctx, syntax_to_datum(pc.ctx, stx)
    );
  else
    throw make_compile_error<syntax_error>(stx, "keyword used as expression");
}

static ptr<syntax>
process_top_level_define(parsing_context& pc, ptr<syntax> stx) {
  if (pc.module_->get_type() == module_::type::immutable)
    throw std::runtime_error{"Can't mutate an immutable environment"};

  auto [name, expr] = parse_name_and_expr(pc, stx, "define");
  auto var = lookup_variable(pc, name);
  if (!var) {
    auto index = pc.ctx.add_top_level_mutable(pc.ctx.constants->void_,
                                              identifier_name(name));
    var = make<top_level_variable>(pc.ctx, identifier_name(name), index);
    define(pc.ctx.store, name, var);
  }

  return make<syntax>(pc.ctx,
                      make_list(pc.ctx,
                                make<syntax>(pc.ctx, pc.ctx.constants->init),
                                name,
                                expr));
}

static ptr<syntax>
process_top_level_form(parsing_context& pc,
                       std::vector<ptr<syntax>>& stack,
                       ptr<syntax> stx) {
  if (auto lst = register_root(pc.ctx, syntax_to_list(pc.ctx, stx))) {
    if (lst.get() == pc.ctx.constants->null)
      throw make_compile_error<syntax_error>(stx, "Empty application");

    ptr<pair> p = assume<pair>(lst.get());
    if (auto form = match_core_form(pc, expect<syntax>(car(p)))) {
      if (form == pc.ctx.constants->define_syntax) {
        process_define_syntax(pc, stx);
        return {};
      } else if (form == pc.ctx.constants->define) {
        return process_top_level_define(pc, stx);
      } else if (form == pc.ctx.constants->begin) {
        expand_begin(pc, stx, stack);
        return {};
      } else if (form == pc.ctx.constants->meta) {
        auto p = preserve(pc.ctx, stx);
        ptr<> datum = eval_meta_expression(pc, stx);
        return datum_to_syntax(pc.ctx, stx, datum);
      }
    }
  }

  return stx;
}

static std::vector<ptr<syntax>>
body_to_stack(std::vector<ptr<syntax>> const& body) {
  std::vector<ptr<syntax>> stack;
  stack.reserve(body.size());
  std::ranges::copy(std::views::reverse(body), std::back_inserter(stack));
  return stack;
}

static std::vector<ptr<syntax>>
process_top_level_forms(parsing_context& pc,
                        std::vector<ptr<syntax>> const& body) {
  std::vector<ptr<syntax>> stack = body_to_stack(body);
  std::vector<ptr<syntax>> result;
  auto p = preserve(pc.ctx, stack, result);

  while (!stack.empty()) {
    ptr<syntax> stx = expand(pc, stack.back()); // GC
    stack.pop_back();

    ptr<syntax> to_emit = process_top_level_form(pc, stack, stx);
    if (to_emit)
      result.push_back(to_emit);
  }

  return result;
}

static std::vector<ptr<syntax>>
add_module_scope_to_body(context& ctx, std::vector<ptr<syntax>> const& body,
                         ptr<scope> s) {
  std::vector<ptr<syntax>> result;
  result.reserve(body.size());

  for (ptr<syntax> e : body)
    result.push_back(e->add_scope(ctx.store, s));

  return result;
}

// Gather syntax and top-level variable definitions, expand top-level macro
// uses. Adds the found top-level syntaxes and variables to the module. Returns
// a list of the expanded top-level commands.
//
// Causes a garbage collection.
std::vector<ptr<syntax>>
expand_top_level(parsing_context& pc, root_ptr<module_> const& m,
                 std::vector<ptr<syntax>> const& exprs) {
  auto body = add_module_scope_to_body(pc.ctx, exprs, m->scope());

  internal_definition_context_guard idc{pc};
  return process_top_level_forms(pc, body);
}

static ptr<syntax>
expand_proc(vm& state, ptr<syntax> stx) {
  return expand(state, stx, nullptr);
}

void
export_parser_expander(context& ctx, ptr<module_> result) {
  define_procedure<expand_proc>(ctx, "expand", result);
}

} // namespace insider
