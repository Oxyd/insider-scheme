#include "analyser.hpp"

#include "compiler/compiler.hpp"
#include "compiler/expression.hpp"
#include "compiler/module_specifier.hpp"
#include "compiler/source_code_provider.hpp"
#include "compiler/syntax_list.hpp"
#include "io/read.hpp"
#include "io/write.hpp"
#include "memory/tracked_ptr.hpp"
#include "runtime/action.hpp"
#include "runtime/integer.hpp"
#include "runtime/symbol.hpp"
#include "runtime/syntax.hpp"
#include "util/define_procedure.hpp"
#include "util/list_iterator.hpp"
#include "vm/vm.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <iterator>
#include <memory>
#include <optional>
#include <ranges>
#include <set>
#include <unordered_map>
#include <utility>
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

using use_site_scopes_list = std::vector<tracked_ptr<scope>>;

namespace {
  struct parsing_context {
    context& ctx;
    tracked_ptr<insider::module_> module_;
    source_file_origin const& origin;
    std::vector<std::vector<std::shared_ptr<variable>>> environment;
    std::vector<use_site_scopes_list> use_site_scopes;

    bool
    record_use_site_scopes() const {
      return !use_site_scopes.empty();
    }
  };

  class environment_extender {
  public:
    explicit
    environment_extender(parsing_context& pc,
                         std::vector<std::shared_ptr<variable>> extension)
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
}

static environment_extender
extend_environment(parsing_context& pc, ptr<scope> s) {
  std::vector<std::shared_ptr<variable>> ext;
  for (scope::binding const& b : *s)
    if (b.variable)
      ext.push_back(b.variable);

  return environment_extender{pc, std::move(ext)};
}

static bool
is_in_scope(parsing_context& pc, std::shared_ptr<variable> const& var) {
  if (var->global)
    return true; // Globals are always in scope even when they are not imported.

  for (auto const& level : pc.environment)
    for (auto const& v : level)
      if (var == v)
        return true;

  return false;
}

static std::shared_ptr<variable>
lookup_variable_binding(ptr<syntax> id) {
  if (auto binding = lookup(id))
    if (binding->variable)
      return binding->variable;

  return {};
}

static std::shared_ptr<variable>
lookup_variable(parsing_context& pc, ptr<syntax> id) {
  if (auto var = lookup_variable_binding(id)) {
    if (is_in_scope(pc, var))
      return var;
    else
      throw make_syntax_error(id, "{}: Not in scope", identifier_name(id));
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
    throw make_syntax_error(x, "Expected identifier, got {}",
                            syntax_to_string(ctx, x));

  return x;
}

template <typename T, typename... Args>
std::unique_ptr<expression>
make_expression(Args&&... args) {
  return std::make_unique<expression>(expression{T(std::forward<Args>(args)...)});
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
  simple_action a(ctx, stx, "Expanding macro use");

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

static std::unique_ptr<expression>
analyse_meta(parsing_context& pc, ptr<syntax> stx);

// Causes a garbage collection.
static tracked_ptr<>
eval_meta(parsing_context& pc, ptr<syntax> datum) {
  auto proc = compile_syntax(pc.ctx, analyse_meta(pc, datum), pc.module_);
  return call_with_continuation_barrier(pc.ctx, proc, {});
}

// Causes a garbage collection
static ptr<transformer>
make_transformer(parsing_context& pc, ptr<syntax> expr) {
  simple_action a(pc.ctx, expr, "Evaluating transformer");
  auto transformer_proc = eval_meta(pc, expr); // GC
  return make<transformer>(pc.ctx, transformer_proc.get());
}

static std::unique_ptr<expression>
parse(parsing_context& pc, ptr<syntax> stx);

namespace {
  struct body_content {
    struct internal_variable {
      tracked_ptr<syntax> id;
      tracked_ptr<syntax> init;
      std::shared_ptr<variable> var;
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

static void
expand_begin(parsing_context& pc, ptr<> stx,
             std::vector<tracked_ptr<syntax>>& stack) {
  std::vector<tracked_ptr<syntax>> subforms;
  for (ptr<> e : in_list{cdr(assume<pair>(stx))})
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

static ptr<>
remove_scope_from_list(context& ctx, ptr<> x, ptr<scope> s) {
  if (auto stx = match<syntax>(x))
    return stx->remove_scope(ctx.store, s);
  else
    return map(ctx, x, [&] (ptr<> elem) {
      return expect<syntax>(elem)->remove_scope(ctx.store, s);
    });
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
process_internal_defines(parsing_context& pc, ptr<> data,
                         source_location const& loc) {
  body_content result;
  auto outside_scope = make_tracked<scope>(
    pc.ctx, pc.ctx,
    fmt::format("outside edge scope at {}", format_location(loc))
  );
  data = add_scope_to_list(pc.ctx, data, outside_scope.get());

  environment_extender internal_env{pc, {}};

  internal_definition_context_guard idc{pc};

  ptr<> list = syntax_to_list(pc.ctx, data);
  if (!list)
    throw make_syntax_error(loc, "Expected list of expressions");

  std::vector<tracked_ptr<syntax>> stack;
  for (ptr<> e : in_list{list})
    stack.push_back(track(pc.ctx, expect<syntax>(e)));
  std::reverse(stack.begin(), stack.end());

  bool seen_expression = false;
  while (!stack.empty()) {
    tracked_ptr<syntax> expr = expand(pc, stack.back()); // GC
    stack.pop_back();

    if (auto p = syntax_to_list(pc.ctx, expr.get())) {
      ptr<core_form_type> form
        = match_core_form(pc, expect<syntax>(car(expect<pair>(p))));

      if (form == pc.ctx.constants->define_syntax) {
        if (seen_expression)
          throw make_syntax_error(expr.get(),
                                  "define-syntax after a nondefinition");

        auto name = track(
          pc.ctx, expect_id(pc.ctx, expect<syntax>(cadr(assume<pair>(p))))
        );
        name = track(
          pc.ctx,
          remove_use_site_scopes(pc.ctx, name.get(), pc.use_site_scopes.back())
        );

        auto transformer
          = make_transformer(pc, expect<syntax>(caddr(assume<pair>(p)))); // GC
        define(pc.ctx.store, name.get(), transformer);

        continue;
      }
      else if (form == pc.ctx.constants->define) {
        if (seen_expression)
          throw make_syntax_error(expr.get(), "define after a nondefinition");

        auto id = expect_id(pc.ctx, expect<syntax>(cadr(assume<pair>(p))));
        id = remove_use_site_scopes(pc.ctx, id, pc.use_site_scopes.back());

        auto init = expect<syntax>(caddr(assume<pair>(p)));
        auto var = std::make_shared<variable>(identifier_name(id));

        result.internal_variable_defs.emplace_back(
          body_content::internal_variable{track(pc.ctx, id),
                                          track(pc.ctx, init),
                                          var}
        );
        define(pc.ctx.store, id, var);
        pc.environment.back().emplace_back(std::move(var));

        continue;
      }
      else if (form == pc.ctx.constants->begin) {
        expand_begin(pc, p, stack);
        continue;
      }
    }

    seen_expression = true;
    result.forms.push_back(expr);
  }

  if (!seen_expression) {
    if (!result.forms.empty())
      throw make_syntax_error(
        loc, "No expression after a sequence of internal definitions"
      );
    else
      throw make_syntax_error(loc, "Empty body");
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
    std::vector<std::shared_ptr<variable>> variables;
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
  simple_action a(pc.ctx, stx, "Parsing {} definition pair", form_name);

  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (!datum || datum == pc.ctx.constants->null)
    throw make_syntax_error(stx, "Invalid {} syntax: Expected a list, got {}",
                            form_name, syntax_to_string(pc.ctx, stx));

  auto id = expect_id(pc.ctx, expect<syntax>(car(assume<pair>(datum))));

  if (cdr(assume<pair>(datum)) == pc.ctx.constants->null)
    throw make_syntax_error(stx, "Invalid {} syntax: No expression for {}",
                            form_name, identifier_name(id));

  return {track(pc.ctx, id),
          track(pc.ctx, expect<syntax>(cadr(assume<pair>(datum))))};
}

static auto
parse_let_common(parsing_context& pc, ptr<syntax> stx,
                 std::string_view form_name) {
  source_location loc = stx->location();
  tracked_ptr<> datum = track(pc.ctx, syntax_to_list(pc.ctx, stx));
  if (!datum || list_length(datum.get()) < 3)
    throw make_syntax_error(stx, "Invalid {} syntax", form_name);

  ptr<syntax> bindings_stx = expect<syntax>(cadr(assume<pair>(datum.get())));
  ptr<> bindings = syntax_to_list(pc.ctx, bindings_stx);
  if (!bindings)
    throw make_syntax_error(bindings_stx,
                            "Invalid {} syntax in binding definitions",
                            form_name);

  std::vector<definition_pair> definitions;
  while (bindings != pc.ctx.constants->null) {
    auto binding = expect<syntax>(car(assume<pair>(bindings)));
    if (!syntax_is<pair>(binding))
      throw make_syntax_error(binding,
                              "Invalid {} syntax in binding definitions",
                              form_name);

    definitions.push_back(parse_definition_pair(pc, binding, form_name));
    bindings = cdr(assume<pair>(bindings));
  }

  ptr<> body = cddr(expect<pair>(datum.get()));
  return std::tuple{definitions, track(pc.ctx, body)};
}

static std::unique_ptr<expression>
parse_let(parsing_context& pc, ptr<syntax> stx_) {
  using namespace std::literals;
  simple_action a(pc.ctx, stx_, "Parsing let");

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
    auto var = std::make_shared<variable>(identifier_name(id.get()));
    define(pc.ctx.store, id.get(), var);

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
  simple_action a(pc.ctx, stx, "Parsing letrec*");
  source_location loc = stx->location();

  auto [definitions, body] = parse_let_common(pc, stx, "letrec*"sv);

  auto subscope = make_tracked<scope>(pc.ctx, pc.ctx,
                                      fmt::format("letrec* body at {}",
                                                  format_location(loc)));

  std::vector<definition_pair_expression> definition_exprs;
  std::vector<std::shared_ptr<variable>> variables;
  for (definition_pair const& dp : definitions) {
    tracked_ptr<syntax> id = track(pc.ctx, dp.id->add_scope(pc.ctx.store,
                                                            subscope.get()));
    auto var = std::make_shared<variable>(identifier_name(dp.id.get()));
    define(pc.ctx.store, id.get(), var);

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
  simple_action a(pc.ctx, stx, "Parsing let-syntax");
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
  simple_action a(pc.ctx, stx, "Parsing letrec-syntax");
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
  simple_action a(pc.ctx, stx, "Parsing lambda");

  source_location loc = stx->location();

  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (!datum || cdr(assume<pair>(datum)) == pc.ctx.constants->null)
    throw make_syntax_error(stx, "Invalid lambda syntax");

  ptr<syntax> param_stx = expect<syntax>(cadr(assume<pair>(datum)));
  ptr<> param_names = param_stx;
  std::vector<std::shared_ptr<variable>> parameters;
  bool has_rest = false;
  auto subscope = make_tracked<scope>(
    pc.ctx, pc.ctx,
    fmt::format("lambda body at {}", format_location(loc))
  );
  while (!semisyntax_is<null_type>(param_names)) {
    if (auto param = semisyntax_match<pair>(pc.ctx, param_names)) {
      auto id = expect_id(pc.ctx, expect<syntax>(car(param)));
      auto id_with_scope = id->add_scope(pc.ctx.store, subscope.get());

      auto var = std::make_shared<variable>(identifier_name(id));
      parameters.push_back(var);
      define(pc.ctx.store, id_with_scope, std::move(var));

      param_names = cdr(param);
    }
    else if (semisyntax_is<symbol>(param_names)) {
      has_rest = true;
      auto name = expect<syntax>(param_names);
      auto name_with_scope = name->add_scope(pc.ctx.store, subscope.get());

      auto var = std::make_shared<variable>(identifier_name(name_with_scope));
      parameters.push_back(var);
      define(pc.ctx.store, name_with_scope, std::move(var));
      break;
    }
    else
      throw make_syntax_error(
        param_stx, "Unexpected value in lambda parameters: {}",
        datum_to_string(pc.ctx, param_names)
      );
  }

  ptr<> body = cddr(assume<pair>(datum));
  ptr<> body_with_scope = add_scope_to_list(pc.ctx, body, subscope.get());

  auto subenv = extend_environment(pc, subscope.get());

  return make_expression<lambda_expression>(
    std::move(parameters), has_rest,
    parse_body(pc, body_with_scope, loc),
    fmt::format("<lambda at {}>", format_location(loc)),
    std::vector<std::shared_ptr<insider::variable>>{}
  );
}

static std::unique_ptr<expression>
parse_if(parsing_context& pc, ptr<syntax> stx) {
  simple_action a(pc.ctx, stx, "Parsing if");

  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (list_length(datum) != 3 && list_length(datum) != 4)
    throw make_syntax_error(stx, "Invalid if syntax");
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
  simple_action a(pc.ctx, stx, "Parsing procedure application");

  auto datum = track(pc.ctx, syntax_to_list(pc.ctx, stx));
  if (!datum)
    throw make_syntax_error(stx, "Invalid function call syntax");

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
parse_box(parsing_context& pc, ptr<syntax> stx) {
  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (list_length(datum) != 2)
    throw make_syntax_error(stx, "Invalid box syntax");

  return make_expression<box_expression>(
    parse(pc, expect<syntax>(cadr(assume<pair>(datum))))
  );
}

static std::unique_ptr<expression>
parse_unbox(parsing_context& pc, ptr<syntax> stx) {
  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (list_length(datum) != 2)
    throw make_syntax_error(stx, "Invalid unbox syntax");

  return make_expression<unbox_expression>(
    parse(pc, expect<syntax>(cadr(assume<pair>(datum))))
  );
}

static std::unique_ptr<expression>
parse_box_set(parsing_context& pc, ptr<syntax> stx) {
  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (list_length(datum) != 3)
    throw make_syntax_error(stx, "Invalid box-set! syntax");

  return make_expression<box_set_expression>(
    parse(pc, expect<syntax>(cadr(assume<pair>(datum)))),
    parse(pc, expect<syntax>(caddr(assume<pair>(datum))))
  );
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
    throw make_syntax_error(id, "Identifier {} not bound to a variable",
                            identifier_name(id));
}

static std::unique_ptr<expression>
parse_reference(parsing_context& pc, ptr<syntax> id) {
  auto var = lookup_variable(pc, id);

  if (!var)
    return parse_unknown_reference(pc, id);
  else if (!var->global)
    return make_expression<local_reference_expression>(std::move(var));
  else
    return make_expression<top_level_reference_expression>(*var->global,
                                                           identifier_name(id));
}

static std::tuple<tracked_ptr<syntax>, ptr<syntax>>
parse_name_and_expr(parsing_context& pc, ptr<syntax> stx,
                    std::string const& form_name) {
  ptr<> datum = syntax_to_list(pc.ctx, stx);
  auto name = expect_id(pc.ctx, expect<syntax>(cadr(assume<pair>(datum))));
  auto name_without_scope = maybe_remove_use_site_scopes(pc, name);
  auto expr = expect<syntax>(caddr(assume<pair>(datum)));
  if (!datum || list_length(datum) != 3)
    throw make_syntax_error(stx, "Invalid {} syntax", form_name);
  return {track(pc.ctx, name_without_scope), expr};
}

static std::unique_ptr<expression>
make_set_expression(parsing_context& pc, tracked_ptr<syntax> const& name,
                    ptr<syntax> expr, std::shared_ptr<variable> var) {
  auto initialiser = parse(pc, expr);
  if (auto* l = std::get_if<lambda_expression>(&initialiser->value))
    l->name = identifier_name(name.get());

  if (!var->global) {
    var->is_set = true;
    return make_expression<local_set_expression>(std::move(var),
                                                 std::move(initialiser));
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
    throw make_syntax_error(name.get(), "Identifier {} not bound to a variable",
                            identifier_name(name.get()));

  return make_set_expression(pc, name, expr, std::move(var));
}

static std::unique_ptr<expression>
define_new_toplevel(parsing_context& pc,
                    tracked_ptr<syntax> const& id,
                    ptr<syntax> expr) {
  std::string name = identifier_name(id.get());
  operand index = pc.ctx.add_top_level(pc.ctx.constants->void_, name);
  auto var = std::make_shared<variable>(name, index);
  define(pc.ctx.store, id.get(), var);
  return make_set_expression(pc, id, expr, std::move(var));
}

static std::unique_ptr<expression>
parse_define(parsing_context& pc, ptr<syntax> stx) {
  if (pc.module_->get_type() == module_::type::immutable)
    throw std::runtime_error{"Can't mutate an immutable environment"};

  auto [name, expr] = parse_name_and_expr(pc, stx, "define");
  auto var = lookup_variable(pc, name.get());
  if (!var)
    return define_new_toplevel(pc, name, expr);
  else
    return make_set_expression(pc, name, expr, std::move(var));
}

static std::unique_ptr<expression>
make_void_expression(parsing_context& pc) {
  return make_expression<literal_expression>(
    track(pc.ctx, pc.ctx.constants->void_)
  );
}

static std::unique_ptr<expression>
parse_define_syntax(parsing_context& pc, ptr<syntax> stx) {
  if (pc.module_->get_type() == module_::type::immutable)
    throw std::runtime_error{"Can't mutate an immutable environment"};

  auto [name, expr] = parse_name_and_expr(pc, stx, "define-syntax");
  auto new_tr = make_transformer(pc, expr); // GC
  if (lookup_transformer(name.get()))
    redefine(pc.ctx.store, name.get(), new_tr);
  else
    define(pc.ctx.store, name.get(), new_tr);
  return make_void_expression(pc);
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
    throw make_syntax_error(stx, "Invalid syntax-trap syntax");

  return parse(pc, expect<syntax>(cadr(assume<pair>(datum))));
}

static std::unique_ptr<expression>
parse_syntax_error(parsing_context& pc, ptr<syntax> stx) {
  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (!datum || list_length(datum) < 2)
    throw make_syntax_error(stx, "Invalid syntax-error syntax, how ironic");

  std::string result_msg = syntax_expect<string>(
    pc.ctx, expect<syntax>(cadr(assume<pair>(datum)))
  )->value();
  for (ptr<> irritant : in_list{cddr(assume<pair>(datum))})
    result_msg += " " + syntax_to_string(pc.ctx, expect<syntax>(irritant));

  throw std::runtime_error{result_msg};
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

static std::unique_ptr<expression>
make_internal_reference(context& ctx, std::string name) {
  std::optional<module_::binding_type> binding
    = ctx.internal_module()->find(ctx.intern(name));
  assert(binding);
  assert(binding->variable);
  assert(binding->variable->global);

  return make_expression<top_level_reference_expression>(
    *binding->variable->global,
    std::move(name)
  );
}

template <typename... Args>
static std::unique_ptr<expression>
make_application(context& ctx, std::string const& name, Args&&... args) {
  return make_expression<application_expression>(make_internal_reference(ctx, name),
                                                 std::forward<Args>(args)...);
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
                  unsigned quote_level) {
  if (auto p = syntax_match<pair>(pc.ctx, stx.get())) {
    unsigned nested_level = quote_level;

    if (auto form = match_core_form(pc, expect<syntax>(car(p)))) {
      if (Traits::is_unquote(pc.ctx, form)) {
        auto unquote_stx = track(pc.ctx, syntax_cadr(pc.ctx, stx.get()));
        if (quote_level == 0)
          return std::make_unique<qq_template>(unquote{unquote_stx, false},
                                               unquote_stx);
        else
          nested_level = quote_level - 1;
      }
      else if (Traits::is_unquote_splicing(pc.ctx, form)) {
        auto unquote_stx = track(pc.ctx, syntax_cadr(pc.ctx, stx.get()));
        if (quote_level == 0)
          return std::make_unique<qq_template>(unquote{unquote_stx, true},
                                               unquote_stx);
        else
          nested_level = quote_level - 1;
      }
      else if (Traits::is_quasiquote(pc.ctx, form))
        nested_level = quote_level + 1;
    }

    bool all_literal = true;
    list_pattern result;

    ptr<> elem = stx.get();
    while (!semisyntax_is<null_type>(elem)) {
      auto current = semisyntax_expect<pair>(pc.ctx, elem);
      if ((!semisyntax_is<pair>(syntax_cdr(pc.ctx, elem))
           && !is<null_type>(cdr(current)))
          || Traits::is_qq_form(pc, syntax_cdr(pc.ctx, elem)))
        // An improper list or a list of the form (x . ,y), which is the same as
        // (x unquote y) which is a proper list, but we don't want to consider
        // it being proper here.
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
  else if (auto v = syntax_match<vector>(pc.ctx, stx.get())) {
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
                    bool force_unwrapped = false) {
  if (auto* cp = std::get_if<list_pattern>(&tpl->value)) {
    std::unique_ptr<expression> tail;
    if (cp->last) {
      if (is_splice(cp->last->cdr))
        throw make_syntax_error(tpl->stx.get(),
                                fmt::format("Invalid use of {}",
                                            Traits::splicing_form_name));

      if (is_splice(cp->last->car))
        tail = make_application(
          pc.ctx, "append",
          process_qq_template<Traits>(pc, cp->last->car, true),
          process_qq_template<Traits>(pc, cp->last->cdr, true)
        );
      else
        tail = make_expression<cons_expression>(
          process_qq_template<Traits>(pc, cp->last->car),
          process_qq_template<Traits>(pc, cp->last->cdr)
        );
    }

    for (auto elem = cp->elems.rbegin(); elem != cp->elems.rend(); ++elem) {
      if (tail) {
        if (is_splice(*elem))
          tail = make_application(pc.ctx, "append",
                                  process_qq_template<Traits>(pc, *elem, true),
                                  std::move(tail));
        else
          tail = make_expression<cons_expression>(
            process_qq_template<Traits>(pc, *elem),
            std::move(tail)
          );
      } else {
        if (is_splice(*elem))
          tail = process_qq_template<Traits>(pc, *elem, true);
        else
          tail = make_expression<cons_expression>(
            process_qq_template<Traits>(pc, *elem),
            make_expression<literal_expression>(track(pc.ctx,
                                                      pc.ctx.constants->null))
          );
      }
    }

    assert(tail);
    return Traits::wrap(pc.ctx, tpl->stx.get(), std::move(tail),
                        force_unwrapped);
  }
  else if (auto* vp = std::get_if<vector_pattern>(&tpl->value)) {
    // If there are no unquote-splicings in the vector, we will simply construct
    // the vector from its elements. If there are unquote-splicings, we will
    // translate it to (vector-append v1 v2 ... (list->vector spliced elements)
    // v3 v4 ...).

    bool any_splices = false;
    for (std::unique_ptr<qq_template> const& elem : vp->elems)
      if (is_splice(elem)) {
        any_splices = true;
        break;
      }

    if (any_splices) {
      auto result = make_expression<application_expression>(
        make_internal_reference(pc.ctx, "vector-append")
      );
      auto& app = std::get<application_expression>(result->value);

      std::vector<std::unique_ptr<expression>> chunk;
      for (std::unique_ptr<qq_template> const& elem : vp->elems) {
        if (is_splice(elem)) {
          if (!chunk.empty()) {
            app.arguments.push_back(
              make_expression<make_vector_expression>(std::move(chunk))
            );
            chunk.clear();
          }

          app.arguments.push_back(
            make_application(pc.ctx, "list->vector",
                             process_qq_template<Traits>(pc, elem, true))
          );
        }
        else
          chunk.push_back(process_qq_template<Traits>(pc, elem, true));
      }

      if (!chunk.empty())
        app.arguments.push_back(
          make_expression<make_vector_expression>(std::move(chunk))
        );

      return Traits::wrap(pc.ctx, tpl->stx.get(), std::move(result),
                          force_unwrapped);
    }
    else {
      std::vector<std::unique_ptr<expression>> elements;
      elements.reserve(vp->elems.size());

      for (std::unique_ptr<qq_template> const& elem : vp->elems)
        elements.push_back(process_qq_template<Traits>(pc, elem));

      return Traits::wrap(
        pc.ctx, tpl->stx.get(),
        make_expression<make_vector_expression>(std::move(elements)),
        force_unwrapped
      );
    }
  }
  else if (auto* expr = std::get_if<unquote>(&tpl->value))
    return Traits::wrap(
      pc.ctx, tpl->stx.get(), parse(pc, expr->datum.get()), force_unwrapped
    );
  else if (auto* lit = std::get_if<literal>(&tpl->value))
    return make_expression<literal_expression>(
      track(pc.ctx, Traits::unwrap(pc.ctx, lit->value.get()))
    );

  assert(!"Forgot a pattern");
  return {};
}

static std::unique_ptr<expression>
parse_quasiquote(parsing_context& pc, ptr<syntax> stx) {
  simple_action a(pc.ctx, stx, "Parsing quasiquote");
  return process_qq_template<quote_traits>(
    pc,
    parse_qq_template<quote_traits>(pc,
                                    track(pc.ctx, syntax_cadr(pc.ctx, stx)),
                                    0)
  );
}

static std::unique_ptr<expression>
parse_quasisyntax(parsing_context& pc, ptr<syntax> stx) {
  simple_action a(pc.ctx, stx, "Parsing quasisyntax");
  return process_qq_template<syntax_traits>(
    pc,
    parse_qq_template<syntax_traits>(pc,
                                     track(pc.ctx, syntax_cadr(pc.ctx, stx)),
                                     0)
  );
}

static tracked_ptr<>
eval_meta_expression(parsing_context& pc, ptr <syntax> stx) {
  simple_action a{pc.ctx, stx, "Evaluating meta expression"};
  ptr<> datum = syntax_to_list(pc.ctx, stx);
  if (!datum || list_length(datum) != 2)
    throw std::runtime_error{"Invalid meta syntax"};

  ptr<syntax> expr = expect<syntax>(cadr(assume<pair>(datum)));
  return eval_meta(pc, expr);
}

static std::unique_ptr<expression>
parse_meta(parsing_context& pc, ptr<syntax> stx) {
  return make_expression<literal_expression>(eval_meta_expression(pc, stx));
}

static std::unique_ptr<expression>
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
      else if (form == pc.ctx.constants->box)
        return parse_box(pc, stx);
      else if (form == pc.ctx.constants->unbox)
        return parse_unbox(pc, stx);
      else if (form == pc.ctx.constants->box_set)
        return parse_box_set(pc, stx);
      else if (form == pc.ctx.constants->begin)
        return parse_sequence(pc, syntax_cdr(pc.ctx, stx));
      else if (form == pc.ctx.constants->define)
        return parse_define(pc, stx);
      else if (form == pc.ctx.constants->define_syntax)
        return parse_define_syntax(pc, stx);
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
      else if (form == pc.ctx.constants->begin_for_syntax)
        throw make_syntax_error(stx, "begin-for-syntax not at top level");
      else if (form == pc.ctx.constants->syntax_trap)
        return parse_syntax_trap(pc, stx);
      else if (form == pc.ctx.constants->syntax_error)
        return parse_syntax_error(pc, stx);
      else if (form == pc.ctx.constants->unquote)
        throw make_syntax_error(stx, "invalid use of unquote");
      else if (form == pc.ctx.constants->unquote_splicing)
        throw make_syntax_error(stx, "invalid use of unquote-splicing");
      else if (form == pc.ctx.constants->unsyntax)
        throw make_syntax_error(stx, "invalid use of unsyntax");
      else if (form == pc.ctx.constants->unsyntax_splicing)
        throw make_syntax_error(stx, "invalid use of unsyntax-splicing");
      else if (form == pc.ctx.constants->let_syntax)
        return parse_let_syntax(pc, stx);
      else if (form == pc.ctx.constants->letrec_syntax)
        return parse_letrec_syntax(pc, stx);
      else if (form == pc.ctx.constants->meta)
        return parse_meta(pc, stx);
    }

    return parse_application(pc, stx);
  }
  else
    return make_expression<literal_expression>(
      track(pc.ctx, syntax_to_datum(pc.ctx, stx))
    );
}

template <auto F, typename... Args>
void
recurse(expression* s, Args&... args) {
  if (std::holds_alternative<literal_expression>(s->value)
      || std::holds_alternative<local_reference_expression>(s->value)
      || std::holds_alternative<top_level_reference_expression>(s->value)
      || std::holds_alternative<unknown_reference_expression>(s->value)) {
    // Nothing to recurse into.
  }
  else if (auto* app = std::get_if<application_expression>(&s->value)) {
    F(app->target.get(), args...);
    for (auto const& arg : app->arguments)
      F(arg.get(), args...);
  }
  else if (auto* let = std::get_if<let_expression>(&s->value)) {
    for (auto const& def : let->definitions)
      F(def.expression.get(), args...);
    for (auto const& expr : let->body.expressions)
      F(expr.get(), args...);
  }
  else if (auto* local_set = std::get_if<local_set_expression>(&s->value)) {
    F(local_set->expression.get(), args...);
  }
  else if (auto* top_level_set
           = std::get_if<top_level_set_expression>(&s->value)) {
    F(top_level_set->expression.get(), args...);
  }
  else if (auto* lambda = std::get_if<lambda_expression>(&s->value)) {
    for (auto const& expr : lambda->body.expressions)
      F(expr.get(), args...);
  }
  else if (auto* if_ = std::get_if<if_expression>(&s->value)) {
    F(if_->test.get(), args...);
    F(if_->consequent.get(), args...);
    if (if_->alternative)
      F(if_->alternative.get(), args...);
  }
  else if (auto* box = std::get_if<box_expression>(&s->value)) {
    F(box->expression.get(), args...);
  }
  else if (auto* unbox = std::get_if<unbox_expression>(&s->value)) {
    F(unbox->box_expr.get(), args...);
  }
  else if (auto* box_set = std::get_if<box_set_expression>(&s->value)) {
    F(box_set->box_expr.get(), args...);
    F(box_set->value_expr.get(), args...);
  }
  else if (auto* cons = std::get_if<cons_expression>(&s->value)) {
    F(cons->car.get(), args...);
    F(cons->cdr.get(), args...);
  }
  else if (auto* make_vector = std::get_if<make_vector_expression>(&s->value)) {
    for (std::unique_ptr<expression> const& e : make_vector->elements)
      F(e.get(), args...);
  }
  else if (auto* sequence = std::get_if<sequence_expression>(&s->value)) {
    for (std::unique_ptr<expression> const& e : sequence->expressions)
      F(e.get(), args...);
  }
  else
    assert(!"Forgot an expression type");
}

static void
box_variable_references(expression* s, std::shared_ptr<variable> const& var) {
  recurse<box_variable_references>(s, var);

  if (auto* ref = std::get_if<local_reference_expression>(&s->value)) {
    if (ref->variable == var) {
      local_reference_expression original_ref = *ref;
      s->value = unbox_expression{std::make_unique<expression>(original_ref)};
    }
  } else if (auto* set = std::get_if<local_set_expression>(&s->value))
    if (set->target == var) {
      local_set_expression original_set = std::move(*set);
      s->value = box_set_expression{
        std::make_unique<expression>(
          local_reference_expression{original_set.target}
        ),
        std::move(original_set.expression)
      };
    }
}

static void
box_set_variables(expression* s) {
  recurse<box_set_variables>(s);

  if (auto* let = std::get_if<let_expression>(&s->value)) {
    for (definition_pair_expression& def : let->definitions)
      if (def.variable->is_set) {
        box_variable_references(s, def.variable);

        std::unique_ptr<expression> orig_expr = std::move(def.expression);
        def.expression = std::make_unique<expression>(
          box_expression{std::move(orig_expr)}
        );
      }
  } else if (auto* lambda = std::get_if<lambda_expression>(&s->value)) {
    for (std::shared_ptr<variable> const& param : lambda->parameters)
      if (param->is_set) {
        box_variable_references(s, param);

        auto set = std::make_unique<expression>(local_set_expression{param, {}});
        auto box = std::make_unique<expression>(box_expression{});
        auto ref = std::make_unique<expression>(
          local_reference_expression{param}
        );
        std::get<box_expression>(box->value).expression = std::move(ref);
        std::get<local_set_expression>(set->value).expression = std::move(box);

        lambda->body.expressions.insert(lambda->body.expressions.begin(),
                                        std::move(set));
      }
  }
}

using variable_set = std::unordered_set<std::shared_ptr<variable>>;

// For each lambda find the list the free variables it uses.
static void
analyse_free_variables(expression* s, variable_set& bound_vars,
                       variable_set& free_vars) {
  if (auto* lambda = std::get_if<lambda_expression>(&s->value)) {
    variable_set inner_bound;
    for (auto const& param : lambda->parameters)
      inner_bound.emplace(param);

    variable_set inner_free;
    recurse<analyse_free_variables>(s, inner_bound, inner_free);

    for (auto const& v : inner_free) {
      lambda->free_variables.push_back(v);

      // Lambda expression's free variables count as variable references in the
      // enclosing procedure.

      if (!bound_vars.count(v))
        free_vars.emplace(v);
    }
  }
  else if (auto* let = std::get_if<let_expression>(&s->value)) {
    for (definition_pair_expression const& dp : let->definitions)
      bound_vars.emplace(dp.variable);

    recurse<analyse_free_variables>(s, bound_vars, free_vars);

    for (definition_pair_expression const& dp : let->definitions)
      bound_vars.erase(dp.variable);
  }
  else if (auto* ref = std::get_if<local_reference_expression>(&s->value)) {
    if (!bound_vars.count(ref->variable))
      free_vars.emplace(ref->variable);
  }
  else if (auto* set = std::get_if<local_set_expression>(&s->value)) {
    assert(bound_vars.count(set->target)); // Local set!s are boxed, so this
                                           // shouldn't happen.
    (void) set;
    recurse<analyse_free_variables>(s, bound_vars, free_vars);
  }
  else
    recurse<analyse_free_variables>(s, bound_vars, free_vars);
}

static void
analyse_free_variables(expression* s) {
  variable_set bound;
  variable_set free;

  analyse_free_variables(s, bound, free);

  assert(free.empty()); // Top-level can't have any free variables.
}

static std::unique_ptr<expression>
analyse_internal(parsing_context& pc, ptr<syntax> stx) {
  std::unique_ptr<expression> result = parse(pc, stx);
  box_set_variables(result.get());
  analyse_free_variables(result.get());
  return result;
}

static std::unique_ptr<expression>
analyse_meta(parsing_context& pc, ptr<syntax> stx) {
  tracked_ptr<syntax> expanded_stx = expand(pc, track(pc.ctx, stx));
  return analyse_internal(pc, expanded_stx.get());
}

std::unique_ptr<expression>
analyse(context& ctx, ptr<syntax> stx, tracked_ptr<module_> const& m,
        source_file_origin const& origin) {
  parameterize origin_param{ctx, ctx.constants->current_source_file_origin_tag,
                            make<opaque_value<source_file_origin>>(ctx, origin)};
  parameterize module_param{ctx, ctx.constants->current_expand_module_tag,
                            m.get()};
  parsing_context pc{ctx, m, origin, {}, {}};
  stx = stx->add_scope(ctx.store, m->scope());
  return analyse_internal(pc, stx);
}

static bool
is_directive(ptr<syntax> datum, std::string const& directive) {
  if (syntax_is<pair>(datum)) {
    auto p = syntax_assume_without_update<pair>(datum);
    auto car_stx = expect<syntax>(car(p));
    if (syntax_is<symbol>(car_stx))
      return syntax_assume_without_update<symbol>(car_stx)->value() == directive;
  }
  return false;
}

module_name
parse_module_name(context& ctx, ptr<syntax> stx) {
  module_name result;

  ptr<> datum = syntax_to_list(ctx, stx);
  if (!datum)
    throw make_syntax_error(stx, "Invalid module name");

  for (ptr<> elem : in_list{datum}) {
    ptr<syntax> e = expect<syntax>(elem);
    if (auto s = syntax_match_without_update<symbol>(e))
      result.push_back(s->value());
    else if (auto i = syntax_match_without_update<integer>(e))
      result.push_back(std::to_string(i->value()));
    else
      throw make_syntax_error(e, "Invalid module name");
  }

  return result;
}

static void
perform_begin_for_syntax(parsing_context& pc, module_specifier const& parent_pm,
                         tracked_ptr<> const& body) {
  simple_action a(pc.ctx, "Analysing begin-for-syntax");

  ptr<> body_without_scope
    = remove_scope_from_list(pc.ctx, body.get(), pc.module_->scope());
  module_specifier pm{pc.ctx.store, parent_pm.name, parent_pm.imports, {},
                      from_scheme<std::vector<ptr<syntax>>>(
                        pc.ctx, syntax_to_list(pc.ctx, body_without_scope)
                      ),
                      pc.origin};
  auto submodule = instantiate(pc.ctx, pm);
  execute(pc.ctx, submodule);
  import_all_top_level(pc.ctx, pc.module_, submodule);
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

static void
perform_top_level_define_syntax(parsing_context& pc, ptr<pair> p) {
  auto name = expect_id(pc.ctx, expect<syntax>(cadr(p)));
  auto name_without_scope = track(
    pc.ctx, remove_use_site_scopes(pc.ctx, name,
                                   pc.use_site_scopes.back())
  );
  auto tr = make_transformer(pc, expect<syntax>(caddr(p))); // GC
  define(pc.ctx.store, name_without_scope.get(), tr);
}

static void
perform_top_level_define(parsing_context& pc, tracked_ptr<syntax> const& stx,
                         tracked_ptr<> const& lst, ptr<pair> p) {
  if (list_length(lst.get()) != 3)
    throw make_syntax_error(stx.get(), "Invalid define syntax");

  auto name = expect_id(pc.ctx, expect<syntax>(cadr(p)));
  auto name_without_scope
    = remove_use_site_scopes(pc.ctx, name, pc.use_site_scopes.back());

  auto index = pc.ctx.add_top_level(
    pc.ctx.constants->void_,
    identifier_name(name_without_scope)
  );
  define(pc.ctx.store, name_without_scope,
         std::make_shared<variable>(identifier_name(name_without_scope),
                                    index));
}

static bool
process_top_level_form(parsing_context& pc, module_specifier const& pm,
                       std::vector<tracked_ptr<syntax>>& stack,
                       tracked_ptr<syntax> const& stx) {
  if (auto lst = track(pc.ctx, syntax_to_list(pc.ctx, stx.get()))) {
    if (lst.get() == pc.ctx.constants->null)
      throw make_syntax_error(stx.get(), "Empty application");

    ptr<pair> p = assume<pair>(lst.get());
    if (auto form = match_core_form(pc, expect<syntax>(car(p)))) {
      if (form == pc.ctx.constants->define_syntax) {
        perform_top_level_define_syntax(pc, p);
        return false;
      } else if (form == pc.ctx.constants->define) {
        perform_top_level_define(pc, stx, lst, p);
        return true;
      } else if (form == pc.ctx.constants->begin) {
        expand_begin(pc, p, stack);
        return false;
      } else if (form == pc.ctx.constants->begin_for_syntax) {
        perform_begin_for_syntax(pc, pm, track(pc.ctx, cdr(p)));
        return false;
      } else if (form == pc.ctx.constants->meta) {
        eval_meta_expression(pc, stx.get());
        return false;
      }
    }
  }

  return true;
}

static std::vector<tracked_ptr<syntax>>
body_to_stack(std::vector<tracked_ptr<syntax>> const& body) {
  std::vector<tracked_ptr<syntax>> stack;
  stack.reserve(body.size());
  std::ranges::copy(std::views::reverse(body), std::back_inserter(stack));
  return stack;
}

static std::vector<tracked_ptr<syntax>>
process_top_level_forms(parsing_context& pc, module_specifier const& pm,
                        std::vector<tracked_ptr<syntax>> const& body) {
  std::vector<tracked_ptr<syntax>> stack = body_to_stack(body);
  std::vector<tracked_ptr<syntax>> result;
  while (!stack.empty()) {
    tracked_ptr<syntax> stx = expand(pc, stack.back()); // GC
    stack.pop_back();

    bool emit = process_top_level_form(pc, pm, stack, stx);
    if (emit)
      result.push_back(stx);
  }

  return result;
}

// Gather syntax and top-level variable definitions, expand top-level macro
// uses. Adds the found top-level syntaxes and variables to the module. Returns
// a list of the expanded top-level commands.
//
// Causes a garbage collection.
static std::vector<tracked_ptr<syntax>>
expand_top_level(parsing_context& pc, tracked_ptr<module_> const& m,
                 module_specifier const& pm) {
  simple_action a(pc.ctx, "Expanding module top-level");

  auto body = add_module_scope_to_body(pc.ctx, pm.body, m->scope());

  internal_definition_context_guard idc{pc};
  return process_top_level_forms(pc, pm, body);
}

import_specifier
parse_import_specifier(context& ctx, ptr<syntax> stx) {
  ptr<> spec = syntax_to_list(ctx, stx);
  if (!spec)
    throw make_syntax_error(stx, "import: Expected a non-empty list");

  auto p = assume<pair>(spec);

  if (auto head = syntax_match_without_update<symbol>(expect<syntax>(car(p)))) {
    if (head->value() == "only") {
      import_specifier::only result;
      result.from = std::make_unique<import_specifier>(
        parse_import_specifier(ctx, expect<syntax>(cadr(p)))
      );

      for (ptr<> identifier : in_list{cddr(p)})
        result.identifiers.push_back(
          syntax_expect_without_update<symbol>(
            expect<syntax>(identifier)
          )->value()
        );

      return import_specifier{std::move(result)};
    }
    else if (head->value() == "except") {
      import_specifier::except result;
      result.from = std::make_unique<import_specifier>(
        parse_import_specifier(ctx, expect<syntax>(cadr(p)))
      );

      for (ptr<> identifier : in_list{cddr(p)})
        result.identifiers.push_back(
          syntax_expect_without_update<symbol>(
            expect<syntax>(identifier)
          )->value()
        );

      return import_specifier{std::move(result)};
    }
    else if (head->value() == "prefix") {
      import_specifier::prefix result;
      result.from = std::make_unique<import_specifier>(
        parse_import_specifier(ctx, expect<syntax>(cadr(p)))
      );
      result.prefix_ = syntax_expect_without_update<symbol>(
        expect<syntax>(caddr(p))
      )->value();

      return import_specifier{std::move(result)};
    }
    else if (head->value() == "rename") {
      import_specifier::rename result;
      result.from = std::make_unique<import_specifier>(
        parse_import_specifier(ctx, expect<syntax>(cadr(p)))
      );

      for (ptr<> name_pair_stx : in_list{cddr(p)}) {
        ptr<> name_pair = syntax_to_list(ctx, expect<syntax>(name_pair_stx));
        if (list_length(name_pair) != 2)
          throw make_syntax_error(expect<syntax>(name_pair_stx),
                                  "import: rename: Expected a list of length 2");

        auto np = assume<pair>(name_pair);

        result.renames.emplace_back(
          syntax_expect_without_update<symbol>(expect<syntax>(car(np)))->value(),
          syntax_expect_without_update<symbol>(expect<syntax>(cadr(np)))->value()
        );
      }

      return import_specifier{std::move(result)};
    }
    else
      return import_specifier{parse_module_name(ctx, stx)};
  }
  else
    return import_specifier{parse_module_name(ctx, stx)};

  assert(!"Unreachable");
  throw std::logic_error{"Unreachable"};
}

static void
process_library_import(context& ctx, module_specifier& result, ptr<syntax> stx) {
  for (ptr<> set : in_list{syntax_to_list(ctx, syntax_cdr(ctx, stx))})
    result.imports.push_back(parse_import_specifier(ctx, assume<syntax>(set)));
}

static void
process_library_export(context& ctx, module_specifier& result, ptr<syntax> stx) {
  for (ptr<> name : in_list{syntax_to_list(ctx, syntax_cdr(ctx, stx))})
    result.exports.push_back(
      syntax_expect_without_update<symbol>(assume<syntax>(name))->value()
    );
}

static module_specifier
read_plain_module(context& ctx, std::vector<ptr<syntax>> const& contents,
                  source_file_origin origin) {
  module_specifier result{ctx.store, std::move(origin)};
  auto current = contents.begin();

  while (current != contents.end())
    if (is_directive(*current, "library")) {
      result.name = parse_module_name(ctx, syntax_cadr(ctx, *current));
      ++current;
    } else if (is_directive(*current, "import")) {
      process_library_import(ctx, result, *current);
      ++current;
    } else if (is_directive(*current, "export")) {
      process_library_export(ctx, result, *current);
      ++current;
    } else
      break;

  result.body.reserve(contents.end() - current);
  for (; current != contents.end(); ++current)
    result.body.push_back(*current);

  return result;
}

static void
process_library_body(context& ctx, module_specifier& result, ptr<syntax> stx) {
  auto body = syntax_to_list(ctx, stx);
  result.body.reserve(result.body.size() + list_length(body) - 1);

  if (body != ctx.constants->null)
    body = cdr(assume<pair>(body));

  while (body != ctx.constants->null) {
    result.body.push_back(expect<syntax>(car(assume<pair>(body))));
    body = cdr(assume<pair>(body));
  }
}

template <auto Reader>
static void
perform_library_include(context& ctx, module_specifier& result,
                        source_file_origin const& origin,
                        source_location const& loc, std::string const& name) {
  if (auto source = find_source_relative(ctx, origin, name)) {
    auto body = Reader(ctx, source->port.get().get());
    result.body.reserve(result.body.size() + body.size());
    std::ranges::copy(body, std::back_inserter(result.body));
  } else
    throw make_syntax_error(loc, fmt::format("File {} not found", name));
}

template <auto Reader>
static void
process_library_include(context& ctx, module_specifier& result,
                        source_file_origin const& origin, ptr<syntax> stx) {
  for (ptr<> filename : in_list{cdr(expect<pair>(syntax_to_datum(ctx, stx)))})
    perform_library_include<Reader>(ctx, result, origin, stx->location(),
                                    expect<string>(filename)->value());
}

static void
process_library_declaration(context& ctx, module_specifier& result,
                            source_file_origin const& origin, ptr<syntax> form);

static void
process_include_library_declarations(context& ctx, module_specifier& result,
                                     source_file_origin const& origin,
                                     ptr<syntax> stx) {
  auto l = cdr(expect<pair>(syntax_to_datum(ctx, stx)));
  for (ptr<> filename_obj : in_list{l}) {
    std::string filename = expect<string>(filename_obj)->value();
    if (auto source = find_source_relative(ctx, origin, filename)) {
      auto contents = read_syntax_multiple(ctx, source->port.get().get());
      for (ptr<syntax> const& s : contents)
        process_library_declaration(ctx, result, source->origin, s);
    }
    else
      throw make_syntax_error(stx, fmt::format("File {} not found", filename));
  }
}

static bool
eval_cond_expand_condition(context& ctx, ptr<syntax> condition);

static bool
eval_cond_expand_disjunction(context& ctx, ptr<> elements) {
  for (ptr<> cond : in_list{syntax_to_list(ctx, elements)})
    if (eval_cond_expand_condition(ctx, expect<syntax>(cond)))
      return true;
  return false;
}

static bool
eval_cond_expand_conjunction(context& ctx, ptr<> elements) {
  for (ptr<> cond : in_list{syntax_to_list(ctx, elements)})
    if (!eval_cond_expand_condition(ctx, expect<syntax>(cond)))
      return false;
  return true;
}

static bool
eval_cond_expand_condition(context& ctx, ptr<syntax> condition) {
  if (auto feature = syntax_match_without_update<symbol>(condition)) {
    if (feature == ctx.intern("else"))
      return true;
    else
      return memq(feature, ctx.features());
  } else if (is_directive(condition, "library"))
    return ctx.module_resolver().knows_module(
      ctx, parse_module_name(ctx, syntax_cadr(ctx, condition))
    );
  else if (is_directive(condition, "not"))
    return !eval_cond_expand_condition(ctx, syntax_cadr(ctx, condition));
  else if (is_directive(condition, "or"))
    return eval_cond_expand_disjunction(ctx, syntax_cdr(ctx, condition));
  else if (is_directive(condition, "and"))
    return eval_cond_expand_conjunction(ctx, syntax_cdr(ctx, condition));
  else
    throw make_syntax_error(condition, "Invalid cond-expand condition");
}

static void
process_library_declarations(context& ctx, module_specifier& result,
                             source_file_origin const& origin,
                             ptr<> list) {
  for (ptr<> decl : in_list{syntax_to_list(ctx, list)})
    process_library_declaration(ctx, result, origin, expect<syntax>(decl));
}

static bool
process_cond_expand_clause(context& ctx, module_specifier& result,
                           source_file_origin const& origin, ptr<syntax> stx) {
  auto condition = syntax_car(ctx, stx);
  auto body = syntax_cdr(ctx, stx);

  if (eval_cond_expand_condition(ctx, condition)) {
    process_library_declarations(ctx, result, origin, body);
    return true;
  } else
    return false;
}

static void
process_cond_expand(context& ctx, module_specifier& result,
                    source_file_origin const& origin, ptr<syntax> stx) {
  for (ptr<> clause : in_list{cdr(expect<pair>(syntax_to_list(ctx, stx)))}) {
    bool taken = process_cond_expand_clause(
      ctx, result, origin, expect<syntax>(clause)
    );
    if (taken)
      break;
  }
}

static void
process_library_declaration(context& ctx, module_specifier& result,
                            source_file_origin const& origin, ptr<syntax> form) {
  static constexpr std::vector<ptr<syntax>>
    (*read_syntax_multiple)(context&, ptr<textual_input_port>)
    = &insider::read_syntax_multiple;

  if (is_directive(form, "import"))
    process_library_import(ctx, result, form);
  else if (is_directive(form, "export"))
    process_library_export(ctx, result, form);
  else if (is_directive(form, "begin"))
    process_library_body(ctx, result, form);
  else if (is_directive(form, "include"))
    process_library_include<read_syntax_multiple>(ctx, result, origin, form);
  else if (is_directive(form, "include-ci"))
    process_library_include<read_syntax_multiple_ci>(ctx, result, origin, form);
  else if (is_directive(form, "include-library-declarations"))
    process_include_library_declarations(ctx, result, origin, form);
  else if (is_directive(form, "cond-expand"))
    process_cond_expand(ctx, result, origin, form);
  else
    throw make_syntax_error(form, "Invalid library declaration");
}

static module_specifier
read_define_library(context& ctx, ptr<syntax> form,
                    source_file_origin const& origin) {
  module_specifier result{ctx.store, origin};

  assert(expect<syntax>(syntax_car(ctx, form))->get_symbol()->value()
         == "define-library");
  if (syntax_cdr(ctx, form) == ctx.constants->null)
    throw make_syntax_error(form, "Invalid define-library syntax");

  result.name = parse_module_name(ctx, syntax_cadr(ctx, form));

  for (ptr<> decl : in_list{cddr(expect<pair>(syntax_to_list(ctx, form)))})
    process_library_declaration(ctx, result, origin, expect<syntax>(decl));

  return result;
}

module_specifier
read_module(context& ctx, std::vector<ptr<syntax>> const& contents,
            source_file_origin const& origin) {
  if (contents.empty())
    throw std::runtime_error("Empty module body");

  if (is_directive(contents.front(), "define-library"))
    return read_define_library(ctx, contents.front(), origin);
  else
    return read_plain_module(ctx, contents, origin);
}

std::optional<module_name>
read_library_name(context& ctx, ptr<textual_input_port> in) {
  try {
    ptr<syntax> first_datum = read_syntax(ctx, in);
    if (is_directive(first_datum, "library")
        || is_directive(first_datum, "define-library"))
      return parse_module_name(ctx, syntax_cadr(ctx, first_datum));
    else
      return {};
  }
  catch (read_error const&) {
    // The file probably isn't a library at all. That is not an error.
    return {};
  }
}

sequence_expression
analyse_module(context& ctx, tracked_ptr<module_> m, module_specifier const& pm,
               bool main_module) {
  parameterize origin_param{
    ctx, ctx.constants->current_source_file_origin_tag,
    make<opaque_value<source_file_origin>>(ctx, pm.origin)
  };
  parameterize main_module_param{
    ctx, ctx.constants->is_main_module_tag,
    main_module ? ctx.constants->t : ctx.constants->f
  };
  parameterize module_param{
    ctx, ctx.constants->current_expand_module_tag, m.get()
  };
  parsing_context pc{ctx, m, pm.origin, {}, {}};

  std::vector<tracked_ptr<syntax>> body = expand_top_level(pc, m, pm);

  sequence_expression result;
  for (tracked_ptr<syntax> const& datum : body)
    result.expressions.push_back(analyse_internal(pc, datum.get()));

  return result;
}

static tracked_ptr<syntax>
expand_proc(context& ctx, tracked_ptr<syntax> stx) {
  return expand(ctx, std::move(stx), nullptr);
}

void
export_analyser(context& ctx, ptr<module_> result) {
  ctx.constants->current_source_file_origin_tag
    = create_parameter_tag(ctx, ctx.constants->f);
  ctx.constants->is_main_module_tag
    = create_parameter_tag(ctx, ctx.constants->f);
  ctx.constants->current_expand_module_tag
    = create_parameter_tag(ctx, ctx.constants->f);

  define_procedure<expand_proc>(ctx, "expand", result, true);
  define_top_level(ctx, "current-source-file-origin-tag", result, true,
                   ctx.constants->current_source_file_origin_tag);
  define_top_level(ctx, "main-module?-tag", result, true,
                   ctx.constants->is_main_module_tag);
  define_top_level(ctx, "current-expand-module-tag", result, true,
                   ctx.constants->current_expand_module_tag);
}

} // namespace insider
