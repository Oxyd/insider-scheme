#include "analyser.hpp"

#include "action.hpp"
#include "compiler.hpp"
#include "converters.hpp"
#include "io.hpp"
#include "numeric.hpp"
#include "vm.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <optional>
#include <set>
#include <unordered_map>
#include <vector>

#ifndef WIN32
#include <csignal>
#else
#include <intrin.h>
#endif

namespace insider {

class syntax_error : public error {
public:
  template <typename... Args>
  syntax_error(syntax* stx, std::string_view fmt, Args&&... args)
    : error{"{}: {}", format_location(stx->location()), fmt::format(fmt, std::forward<Args>(args)...)}
  { }

  template <typename... Args>
  syntax_error(source_location const& loc, std::string_view fmt, Args&&... args)
    : error{"{}: {}", format_location(loc), fmt::format(fmt, std::forward<Args>(args)...)}
  { }
};

static std::string
syntax_to_string(context& ctx, syntax* stx) {
  return datum_to_string(ctx, syntax_to_datum(ctx, stx));
}

static std::shared_ptr<variable>
lookup_variable(tracked_ptr<environment> const& env, syntax* id) {
  if (auto binding = lookup(env, id->expression())) {
    if (auto var = std::get_if<std::shared_ptr<variable>>(&*binding))
      return *var;
    else
      return {};
  }

  return {};
}

static std::shared_ptr<variable>
lookup_variable(tracked_ptr<environment> const& env, syntactic_closure* id) {
  if (auto binding = lookup(env, id)) {
    if (auto var = std::get_if<std::shared_ptr<variable>>(&*binding))
      return *var;
    else
      return {};
  }

  return {};
}

static core_form_type*
lookup_core(context& ctx, tracked_ptr<environment> const& env, syntax* id) {
  auto var = lookup_variable(env, id);
  if (!var || !var->global)
    return {};  // Core forms are never defined in a local environment.

  object* form = ctx.get_top_level(*var->global);
  return match<core_form_type>(form);
}

static syntax*
expect_id(context& ctx, syntax* x) {
  if (!is_identifier(x->expression()))
    throw syntax_error{x, "Expected identifier, got {}", syntax_to_string(ctx, x)};

  return x;
}

template <typename T, typename... Args>
std::unique_ptr<expression>
make_expression(Args&&... args) {
  return std::make_unique<expression>(expression{T(std::forward<Args>(args)...)});
}

static transformer*
lookup_transformer(context& ctx, tracked_ptr<environment> const& env, syntax* id) {
  if (auto binding = lookup(env, id->expression())) {
    if (auto tr = std::get_if<transformer*>(&*binding))
      return *tr;
    else
      return {};
  }

  if (auto sc = match<syntactic_closure>(id)) {
    tracked_ptr<environment> const& subenv = syntactic_closure_to_environment(ctx, sc, env);
    return lookup_transformer(ctx, subenv, sc->expression());
  }

  return {};
}

static tracked_ptr<syntax>
call_transformer(context& ctx, transformer* t, tracked_ptr<environment> const& env,
                 tracked_ptr<syntax> const& stx) {
  struct guard {
    context& ctx;

    guard(context& ctx, tracked_ptr<environment> const& env)
      : ctx{ctx}
    {
      assert(!ctx.current_usage_environment);
      ctx.current_usage_environment = env;
    }

    ~guard() {
      ctx.current_usage_environment.reset();
    }
  } g{ctx, env};

  object* result_datum = call(ctx, t->callable(), {syntax_to_datum(ctx, stx.get()), t->environment(), env.get()}).get();
  return track(ctx, datum_to_syntax(ctx, stx->location(), result_datum));
}

// If the head of the given list is bound to a transformer, run the transformer
// on the datum, and repeat.
//
// Causes a garbage collection.
static tracked_ptr<syntax>
expand(context& ctx, tracked_ptr<environment> const& env, tracked_ptr<syntax> stx) {
  simple_action a(ctx, stx, "Expanding macro use");

  bool expanded;
  do {
    expanded = false;

    if (auto lst = syntax_match<pair>(stx.get())) {
      syntax* head = expect<syntax>(car(lst));
      if (is_identifier(head->expression())) {
        if (transformer* t = lookup_transformer(ctx, env, head)) {
          stx = call_transformer(ctx, t, env, stx);
          expanded = true;
        }
      }
    }
  } while (expanded);

  return stx;
}

// Causes a garbage collection.
static object*
eval_transformer(context& ctx, module& m, syntax* datum) {
  simple_action a(ctx, datum, "Evaluating transformer");
  auto proc = compile_expression(ctx, datum, m);
  return call(ctx, proc, {}).get();
}

namespace {
  struct parsing_context {
    context&         ctx;
    insider::module& module;
  };
}

static std::unique_ptr<expression>
parse(parsing_context& pc, tracked_ptr<environment> const&, syntax* stx);

static definition_pair_expression
parse_definition_pair(parsing_context& pc, tracked_ptr<environment> const& env, syntax* stx) {
  simple_action a(pc.ctx, stx, "Parsing let definition pair");

  object* datum = syntax_to_list(pc.ctx, stx);
  if (!datum || datum == pc.ctx.constants->null.get())
    throw syntax_error{stx, "Invalid let syntax: Expected a list, got {}", syntax_to_string(pc.ctx, stx)};

  auto id = expect_id(pc.ctx, expect<syntax>(car(assume<pair>(datum))));

  if (cdr(assume<pair>(datum)) == pc.ctx.constants->null.get())
    throw syntax_error(stx, "Invalid let syntax: No expression for {}", identifier_name(id));

  return {track(pc.ctx, id),
          std::make_shared<variable>(identifier_name(id)), parse(pc, env, expect<syntax>(cadr(assume<pair>(datum))))};
}

namespace {
  struct body_content {
    tracked_ptr<environment>         env;
    std::vector<tracked_ptr<syntax>> forms;
    std::vector<tracked_ptr<syntax>> internal_variable_ids;
  };
}

static core_form_type*
match_core_form(context& ctx, tracked_ptr<environment> env, syntax* stx) {
  while (auto sc = syntax_match<syntactic_closure>(stx)) {
    env = syntactic_closure_to_environment(ctx, sc, env);
    stx = sc->expression();
  }

  if (auto cf = syntax_match<core_form_type>(stx))
    return cf;
  else if (syntax_is<symbol>(stx))
    return lookup_core(ctx, env, stx);
  return {};
}

static std::tuple<syntactic_closure*, tracked_ptr<environment> const&>
collapse_syntactic_closures(context& ctx, syntactic_closure* sc, tracked_ptr<environment> env) {
  env = syntactic_closure_to_environment(ctx, sc, env);

  while (auto inner_sc = syntax_match<syntactic_closure>(sc->expression())) {
    env = syntactic_closure_to_environment(ctx, inner_sc, env);
    sc = inner_sc;
  }

  return {sc, env};
}

// Given
//   (syntactic-closure
//    (syntactic-closure
//     ...
//      (define name expr)
//      - or -
//      (define-syntax name expr)
//      - or -
//      (begin
//       expr ...) ...))
//
// Transform it into
//   (define name (syntactic-closure expr))
//   - or -
//   (define-syntax name (syntactic-closure expr))
//   - or -
//   (begin (syntactic-closure expr) ...)
//
// If the input form doesn't contain a begin or define, return nothing. The
// input syntactic closures are all collapsed into one, and the output syntactic
// closure has no free variables even if the input ones did.
static syntax*
transpose_syntactic_closure(context& ctx, syntactic_closure* sc, tracked_ptr<environment> env) {
  std::tie(sc, env) = collapse_syntactic_closures(ctx, sc, env);

  syntax* stx = sc->expression();
  if (auto p = syntax_to_list(ctx, stx)) {
    if (auto form = match_core_form(ctx, env, expect<syntax>(car(expect<pair>(p))))) {
      if (form == ctx.constants->define.get() || form == ctx.constants->define_syntax.get()) {
        auto name = expect<syntax>(cadr(assume<pair>(p)));
        auto expr = expect<syntax>(caddr(assume<pair>(p)));
        return datum_to_syntax(ctx, stx->location(),
                               make_list(ctx, form, name,
                                         make<syntactic_closure>(ctx, env.get(), expr, ctx.constants->null.get())));
      }
      else if (form == ctx.constants->begin.get()) {
        std::vector<object*> exprs;
        for (object* e : in_list{cdr(expect<pair>(p))})
          exprs.push_back(make<syntactic_closure>(ctx, env.get(), expect<syntax>(e), ctx.constants->null.get()));

        return datum_to_syntax(ctx, stx->location(),
                               cons(ctx, form, make_list_from_vector(ctx, exprs)));
      }
    }
  }

  return {};
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
process_internal_defines(parsing_context& pc, tracked_ptr<environment> const& env, object* data,
                         source_location const& loc) {
  body_content result;
  result.env = make_tracked<environment>(pc.ctx, env.get());

  object* list = syntax_to_list(pc.ctx, data);
  if (!list)
    throw syntax_error{loc, "Expected list of expressions"};

  std::vector<tracked_ptr<syntax>> stack;
  for (object* e : in_list{list})
    stack.push_back(track(pc.ctx, expect<syntax>(e)));
  std::reverse(stack.begin(), stack.end());

  bool seen_expression = false;
  while (!stack.empty()) {
    tracked_ptr<syntax> expr = expand(pc.ctx, result.env, stack.back()); // GC
    stack.pop_back();

    if (auto p = syntax_to_list(pc.ctx, expr.get())) {
      core_form_type* form = match_core_form(pc.ctx, result.env, expect<syntax>(car(expect<pair>(p))));

      if (form == pc.ctx.constants->define_syntax.get()) {
        if (seen_expression)
          throw syntax_error(expr.get(), "define-syntax after a nondefinition");

        auto name = track(pc.ctx, expect_id(pc.ctx, expect<syntax>(cadr(assume<pair>(p)))));
        auto transformer_proc = eval_transformer(pc.ctx, pc.module, expect<syntax>(caddr(assume<pair>(p)))); // GC
        auto transformer = make<insider::transformer>(pc.ctx, result.env.get(), transformer_proc);
        result.env->add(pc.ctx.store, name->expression(), transformer);

        continue;
      }
      else if (form == pc.ctx.constants->define.get()) {
        if (seen_expression)
          throw syntax_error(expr.get(), "define after a nondefinition");

        auto id = expect_id(pc.ctx, expect<syntax>(cadr(assume<pair>(p))));
        result.env->add(pc.ctx.store, id->expression(), std::make_shared<variable>(identifier_name(id)));

        result.forms.push_back(expr);
        result.internal_variable_ids.push_back(track(pc.ctx, id));

        continue;
      }
      else if (form == pc.ctx.constants->begin.get()) {
        std::vector<tracked_ptr<syntax>> subforms;
        for (object* e : in_list{cdr(assume<pair>(p))})
          subforms.push_back(track(pc.ctx, expect<syntax>(e)));

        std::copy(subforms.rbegin(), subforms.rend(), std::back_inserter(stack));
        continue;
      }
    }
    else if (auto sc = syntax_match<syntactic_closure>(expr.get())) {
      if (syntax* subform = transpose_syntactic_closure(pc.ctx, sc, result.env)) {
        stack.push_back(track(pc.ctx, subform));
        continue;
      }
    }

    seen_expression = true;
    result.forms.push_back(expr);
  }

  if (!seen_expression) {
    if (!result.forms.empty())
      throw syntax_error(loc, "No expression after a sequence of internal definitions");
    else
      throw syntax_error(loc, "Empty body");
  }

  return result;
}

static std::vector<std::unique_ptr<expression>>
parse_expression_list(parsing_context& pc, tracked_ptr<environment> const& env,
                      std::vector<tracked_ptr<syntax>> const& exprs) {
  std::vector<std::unique_ptr<expression>> result;
  result.reserve(exprs.size());

  for (tracked_ptr<syntax> const& e : exprs)
    result.push_back(parse(pc, env, e.get()));

  return result;
}

static sequence_expression
parse_body(parsing_context& pc, tracked_ptr<environment> const& env, object* data, source_location const& loc) {
  body_content content = process_internal_defines(pc, env, data, loc); // GC
  if (!content.internal_variable_ids.empty()) {
    std::vector<definition_pair_expression> definitions;
    for (tracked_ptr<syntax> const& id : content.internal_variable_ids) {
      auto void_expr = make_expression<literal_expression>(pc.ctx.constants->void_);
      auto var = lookup_variable(content.env, id.get());
      assert(var);
      definitions.push_back({id, var, std::move(void_expr)});
    }

    sequence_expression result;
    result.expressions.push_back(
      make_expression<let_expression>(std::move(definitions),
                                      sequence_expression{parse_expression_list(pc, content.env, content.forms)})
    );
    return result;
  }
  else
    return sequence_expression{parse_expression_list(pc, content.env, content.forms)};
}

static std::unique_ptr<expression>
parse_let(parsing_context& pc, tracked_ptr<environment> const& env, syntax* stx) {
  simple_action a(pc.ctx, stx, "Parsing let");

  object* datum = syntax_to_list(pc.ctx, stx);
  if (!datum || list_length(datum) < 3)
    throw syntax_error(stx, "Invalid let syntax");

  syntax* bindings_stx = expect<syntax>(cadr(assume<pair>(datum)));
  object* bindings = syntax_to_list(pc.ctx, bindings_stx);
  if (!bindings)
    throw syntax_error(bindings_stx, "Invalid let syntax in binding definitions");

  std::vector<definition_pair_expression> definitions;
  while (bindings != pc.ctx.constants->null.get()) {
    auto binding = expect<syntax>(car(assume<pair>(bindings)));
    if (!syntax_is<pair>(binding))
      throw syntax_error(binding, "Invalid let syntax in binding definitions");

    definitions.push_back(parse_definition_pair(pc, env, binding));
    bindings = cdr(assume<pair>(bindings));
  }

  auto subenv = make_tracked<environment>(pc.ctx, env.get());
  for (definition_pair_expression const& dp : definitions)
    subenv->add(pc.ctx.store, dp.id->expression(), dp.variable);

  return make_expression<let_expression>(std::move(definitions),
                                         parse_body(pc, subenv, cddr(expect<pair>(datum)), stx->location()));
}

static std::unique_ptr<expression>
parse_lambda(parsing_context& pc, tracked_ptr<environment> const& env, syntax* stx) {
  simple_action a(pc.ctx, stx, "Parsing lambda");

  object* datum = syntax_to_list(pc.ctx, stx);
  if (!datum || cdr(assume<pair>(datum)) == pc.ctx.constants->null.get())
    throw syntax_error(stx, "Invalid lambda syntax");

  syntax* param_stx = expect<syntax>(cadr(assume<pair>(datum)));
  object* param_names = param_stx;
  std::vector<std::shared_ptr<variable>> parameters;
  bool has_rest = false;
  auto subenv = make_tracked<environment>(pc.ctx, env.get());
  while (!semisyntax_is<null_type>(param_names)) {
    if (auto param = semisyntax_match<pair>(param_names)) {
      auto id = expect_id(pc.ctx, expect<syntax>(car(param)));
      auto var = std::make_shared<variable>(identifier_name(id));
      parameters.push_back(var);
      subenv->add(pc.ctx.store, id->expression(), std::move(var));

      param_names = cdr(param);
    }
    else if (semisyntax_is<symbol>(param_names)) {
      has_rest = true;
      auto id = semisyntax_assume<symbol>(param_names);
      auto var = std::make_shared<variable>(identifier_name(id));
      parameters.push_back(var);
      subenv->add(pc.ctx.store, id, std::move(var));
      break;
    }
    else
      throw syntax_error{param_stx, "Unexpected value in lambda parameters: {}",
                         datum_to_string(pc.ctx, param_names)};
  }

  return make_expression<lambda_expression>(std::move(parameters), has_rest,
                                            parse_body(pc, subenv, cddr(assume<pair>(datum)), stx->location()),
                                            std::nullopt, std::vector<std::shared_ptr<insider::variable>>{});
}

static std::unique_ptr<expression>
parse_if(parsing_context& pc, tracked_ptr<environment> const& env, syntax* stx) {
  simple_action a(pc.ctx, stx, "Parsing if");

  object* datum = syntax_to_list(pc.ctx, stx);
  if (list_length(datum) != 3 && list_length(datum) != 4)
    throw syntax_error(stx, "Invalid if syntax");
  pair* list = assume<pair>(datum);

  syntax* test_expr = expect<syntax>(cadr(list));
  syntax* then_expr = expect<syntax>(caddr(list));
  syntax* else_expr = nullptr;
  if (cdddr(list) != pc.ctx.constants->null.get())
    else_expr = expect<syntax>(cadddr(list));

  return make_expression<if_expression>(parse(pc, env, test_expr),
                                        parse(pc, env, then_expr),
                                        else_expr ? parse(pc, env, else_expr) : nullptr);
}

static std::unique_ptr<expression>
parse_application(parsing_context& pc, tracked_ptr<environment> const& env, syntax* stx) {
  simple_action a(pc.ctx, stx, "Parsing procedure application");

  auto datum = track(pc.ctx, syntax_to_list(pc.ctx, stx));
  if (!datum)
    throw syntax_error(stx, "Invalid function call syntax");

  std::vector<std::unique_ptr<expression>> arguments;
  auto arg_expr = track(pc.ctx, cdr(assume<pair>(datum.get())));
  while (arg_expr != pc.ctx.constants->null) {
    arguments.push_back(parse(pc, env, expect<syntax>(car(assume<pair>(arg_expr.get())))));
    arg_expr = track(pc.ctx, cdr(assume<pair>(arg_expr.get())));
  }

  return make_expression<application_expression>(parse(pc, env, expect<syntax>(car(assume<pair>(datum.get())))),
                                                 std::move(arguments));
}

static std::unique_ptr<expression>
parse_box(parsing_context& pc, tracked_ptr<environment> const& env, syntax* stx) {
  object* datum = syntax_to_list(pc.ctx, stx);
  if (list_length(datum) != 2)
    throw syntax_error(stx, "Invalid box syntax");

  return make_expression<box_expression>(parse(pc, env, expect<syntax>(cadr(assume<pair>(datum)))));
}

static std::unique_ptr<expression>
parse_unbox(parsing_context& pc, tracked_ptr<environment> const& env, syntax* stx) {
  object* datum = syntax_to_list(pc.ctx, stx);
  if (list_length(datum) != 2)
    throw syntax_error(stx, "Invalid unbox syntax");

  return make_expression<unbox_expression>(parse(pc, env, expect<syntax>(cadr(assume<pair>(datum)))));
}

static std::unique_ptr<expression>
parse_box_set(parsing_context& pc, tracked_ptr<environment> const& env, syntax* stx) {
  object* datum = syntax_to_list(pc.ctx, stx);
  if (list_length(datum) != 3)
    throw syntax_error(stx, "Invalid box-set! syntax");

  return make_expression<box_set_expression>(parse(pc, env, expect<syntax>(cadr(assume<pair>(datum)))),
                                             parse(pc, env, expect<syntax>(caddr(assume<pair>(datum)))));
}

static syntax*
syntax_car(object* stx) {
  return expect<syntax>(car(semisyntax_expect<pair>(stx)));
}

static object*
syntax_cdr(object* stx) {
  return cdr(semisyntax_expect<pair>(stx));
}

static syntax*
syntax_cadr(object* stx) {
  return expect<syntax>(car(semisyntax_expect<pair>(syntax_cdr(stx))));
}

static std::unique_ptr<expression>
parse_sequence(parsing_context& pc, tracked_ptr<environment> const& env, object* stx) {
  std::vector<std::unique_ptr<expression>> exprs;
  for (generic_tracked_ptr datum = track(pc.ctx, stx);
       !semisyntax_is<null_type>(datum.get());
       datum = track(pc.ctx, syntax_cdr(datum.get())))
    exprs.push_back(parse(pc, env, syntax_car(datum.get())));

  return make_expression<sequence_expression>(std::move(exprs));
}

static std::unique_ptr<expression>
parse_reference(tracked_ptr<environment> const& env, syntax* id) {
  auto var = lookup_variable(env, id);

  if (!var)
    throw syntax_error(id, "Identifier {} not bound to a variable", identifier_name(id));

  if (!var->global)
    return make_expression<local_reference_expression>(std::move(var));
  else
    return make_expression<top_level_reference_expression>(*var->global, identifier_name(id));
}

static std::unique_ptr<expression>
parse_define_or_set(parsing_context& pc, tracked_ptr<environment> const& env, syntax* stx,
                    std::string const& form_name) {
  // Defines are processed in two passes: First all the define'd variables are
  // declared within the module or scope and initialised to #void; second, they
  // are assigned their values as if by set!.
  //
  // This function can be therefore be used for both set! and define forms -- in
  // either case, we emit a set! syntax.

  object* datum = syntax_to_list(pc.ctx, stx);
  if (!datum || list_length(datum) != 3)
    throw syntax_error(stx, "Invalid {} syntax", form_name);

  auto name = track(pc.ctx, expect_id(pc.ctx, expect<syntax>(cadr(assume<pair>(datum)))));
  syntax* expr = expect<syntax>(caddr(assume<pair>(datum)));

  auto var = lookup_variable(env, name.get());
  if (!var)
    throw syntax_error(name.get(), "Identifier {} not bound to a variable", identifier_name(name.get()));

  auto initialiser = parse(pc, env, expr);
  if (auto l = std::get_if<lambda_expression>(&initialiser->value))
    l->name = identifier_name(name.get());

  if (!var->global) {
    var->is_set = true;
    return make_expression<local_set_expression>(std::move(var), std::move(initialiser));
  }
  else
    return make_expression<top_level_set_expression>(*var->global, std::move(initialiser));
}

static std::unique_ptr<expression>
parse_syntactic_closure(parsing_context& pc, tracked_ptr<environment> const& env,
                        syntactic_closure* sc) {
  if (syntax_is<symbol>(sc->expression())) {
    if (auto var = lookup_variable(env, sc)) {
      if (!var->global)
        return make_expression<local_reference_expression>(std::move(var));
      else
        return make_expression<top_level_reference_expression>(*var->global, identifier_name(sc));
    }
  }

  auto new_env = syntactic_closure_to_environment(pc.ctx, sc, env);
  return parse(pc, new_env, sc->expression());
}

static std::unique_ptr<expression>
parse_syntax_trap(parsing_context& pc, tracked_ptr<environment> const& env, syntax* stx) {
#ifndef WIN32
  raise(SIGTRAP);
#else
  __debugbreak();
#endif

  object* datum = syntax_to_list(pc.ctx, stx);
  if (!datum)
    throw syntax_error{stx, "Invalid syntax-trap syntax"};

  return parse(pc, env, expect<syntax>(cadr(assume<pair>(datum))));
}

static std::unique_ptr<expression>
parse_syntax_error(parsing_context& pc, syntax* stx) {
  object* datum = syntax_to_list(pc.ctx, stx);
  if (!datum || list_length(datum) < 2)
    throw syntax_error{stx, "Invalid syntax-error syntax, how ironic"};

  std::string result_msg = syntax_expect<string>(expect<syntax>(cadr(assume<pair>(datum))))->value();
  for (object* irritant : in_list{cddr(assume<pair>(datum))})
    result_msg += " " + syntax_to_string(pc.ctx, expect<syntax>(irritant));

  throw error{result_msg};
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

    value_type value;
    source_location loc;

    qq_template(value_type value, syntax* stx)
      : value(std::move(value))
      , loc{stx->location()}
    { }
  };
} // anonymous namespace

static bool
is_qq_form(context& ctx, tracked_ptr<environment> const& env, object* stx) {
  if (auto p = semisyntax_match<pair>(stx))
    if (auto form = match_core_form(ctx, env, expect<syntax>(car(p))))
      return form == ctx.constants->unquote.get()
             || form == ctx.constants->unquote_splicing.get()
             || form == ctx.constants->quasiquote.get();

  return false;
}

static std::unique_ptr<qq_template>
parse_qq_template(context& ctx, tracked_ptr<environment> const& env, syntax* stx, unsigned quote_level) {
  if (auto p = syntax_match<pair>(stx)) {
    unsigned nested_level = quote_level;

    if (auto form = match_core_form(ctx, env, expect<syntax>(car(p)))) {
      if (form == ctx.constants->unquote.get()) {
        if (quote_level == 0)
          return std::make_unique<qq_template>(unquote{track(ctx, syntax_cadr(stx)), false}, syntax_cadr(stx));
        else
          nested_level = quote_level - 1;
      }
      else if (form == ctx.constants->unquote_splicing.get()) {
        if (quote_level == 0)
          return std::make_unique<qq_template>(unquote{track(ctx, syntax_cadr(stx)), true}, syntax_cadr(stx));
        else
          nested_level = quote_level - 1;
      }
      else if (form == ctx.constants->quasiquote.get())
        nested_level = quote_level + 1;
    }

    bool all_literal = true;
    list_pattern result;

    object* elem = stx;
    while (!semisyntax_is<null_type>(elem)) {
      auto current = semisyntax_expect<pair>(elem);
      if ((!semisyntax_is<pair>(syntax_cdr(elem)) && !is<null_type>(cdr(current)))
          || is_qq_form(ctx, env, syntax_cdr(elem)))
        // An improper list or a list of the form (x . ,y), which is the same as
        // (x unquote y) which is a proper list, but we don't want to consider
        // it being proper here.
        break;

      result.elems.push_back(parse_qq_template(ctx, env, syntax_car(elem), nested_level));
      elem = syntax_cdr(elem);

      if (!std::holds_alternative<literal>(result.elems.back()->value))
        all_literal = false;
    }

    if (auto pair = semisyntax_match<insider::pair>(elem)) {
      result.last = cons_pattern{parse_qq_template(ctx, env, expect<syntax>(car(pair)), nested_level),
                                 parse_qq_template(ctx, env, expect<syntax>(cdr(pair)), nested_level)};
      all_literal = all_literal
                    && std::holds_alternative<literal>(result.last->car->value)
                    && std::holds_alternative<literal>(result.last->cdr->value);
    }

    if (all_literal)
      return std::make_unique<qq_template>(literal{track(ctx, stx)}, stx);
    else
      return std::make_unique<qq_template>(std::move(result), stx);
  }
  else if (auto v = syntax_match<vector>(stx)) {
    std::vector<std::unique_ptr<qq_template>> templates;
    templates.reserve(v->size());
    bool all_literal = true;

    for (std::size_t i = 0; i < v->size(); ++i) {
      templates.push_back(parse_qq_template(ctx, env, expect<syntax>(v->ref(i)), quote_level));
      if (!std::holds_alternative<literal>(templates.back()->value))
        all_literal = false;
    }

    if (all_literal)
      return std::make_unique<qq_template>(literal{track(ctx, stx)}, stx);
    else
      return std::make_unique<qq_template>(vector_pattern{std::move(templates)}, stx);
  }
  else
    return std::make_unique<qq_template>(literal{track(ctx, stx)}, stx);
}

static std::unique_ptr<expression>
make_internal_reference(context& ctx, std::string name) {
  std::optional<module::binding_type> binding = ctx.internal_module.find(ctx.intern(name));
  assert(binding);
  assert(std::holds_alternative<module::index_type>(*binding));
  return make_expression<top_level_reference_expression>(std::get<module::index_type>(*binding),
                                                         std::move(name));
}

static bool
is_splice(std::unique_ptr<qq_template> const& tpl) {
  if (auto* expr = std::get_if<unquote>(&tpl->value))
    if (expr->splicing)
      return true;
  return false;
}

static std::unique_ptr<expression>
process_qq_template(parsing_context& pc, tracked_ptr<environment> const& env, std::unique_ptr<qq_template> const& tpl) {
  if (auto* cp = std::get_if<list_pattern>(&tpl->value)) {
    std::unique_ptr<expression> tail;
    if (cp->last) {
      if (is_splice(cp->last->cdr))
        throw syntax_error(tpl->loc, "Invalid use of unquote-splicing");

      if (is_splice(cp->last->car))
        tail = make_expression<application_expression>(make_internal_reference(pc.ctx, "append"),
                                                       process_qq_template(pc, env, cp->last->car),
                                                       process_qq_template(pc, env, cp->last->cdr));
      else
        tail = make_expression<cons_expression>(process_qq_template(pc, env, cp->last->car),
                                                process_qq_template(pc, env, cp->last->cdr));
    }

    for (auto elem = cp->elems.rbegin(); elem != cp->elems.rend(); ++elem) {
      if (tail) {
        if (is_splice(*elem))
          tail = make_expression<application_expression>(make_internal_reference(pc.ctx, "append"),
                                                         process_qq_template(pc, env, *elem),
                                                         std::move(tail));
        else
          tail = make_expression<cons_expression>(process_qq_template(pc, env, *elem),
                                                  std::move(tail));
      } else {
        if (is_splice(*elem))
          tail = process_qq_template(pc, env, *elem);
        else
          tail = make_expression<cons_expression>(process_qq_template(pc, env, *elem),
                                                  make_expression<literal_expression>(pc.ctx.constants->null));
      }
    }

    assert(tail);
    return tail;
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
      auto result = make_expression<application_expression>(make_internal_reference(pc.ctx, "vector-append"));
      application_expression& app = std::get<application_expression>(result->value);

      std::vector<std::unique_ptr<expression>> chunk;
      for (std::unique_ptr<qq_template> const& elem : vp->elems) {
        if (is_splice(elem)) {
          if (!chunk.empty()) {
            app.arguments.push_back(make_expression<make_vector_expression>(std::move(chunk)));
            chunk.clear();
          }

          app.arguments.push_back(make_expression<application_expression>(make_internal_reference(pc.ctx, "list->vector"),
                                                                          process_qq_template(pc, env, elem)));
        }
        else
          chunk.push_back(process_qq_template(pc, env, elem));
      }

      if (!chunk.empty())
        app.arguments.push_back(make_expression<make_vector_expression>(std::move(chunk)));

      return result;
    }
    else {
      std::vector<std::unique_ptr<expression>> elements;
      elements.reserve(vp->elems.size());

      for (std::unique_ptr<qq_template> const& elem : vp->elems)
        elements.push_back(process_qq_template(pc, env, elem));

      return make_expression<make_vector_expression>(std::move(elements));
    }
  }
  else if (auto* expr = std::get_if<unquote>(&tpl->value))
    return parse(pc, env, expr->datum.get());
  else if (auto* lit = std::get_if<literal>(&tpl->value))
    return make_expression<literal_expression>(track(pc.ctx, syntax_to_datum(pc.ctx, lit->value.get())));

  assert(!"Forgot a pattern");
  return {};
}

static std::unique_ptr<expression>
parse_quasiquote(parsing_context& pc, tracked_ptr<environment> const& env, syntax* stx) {
  return process_qq_template(pc, env, parse_qq_template(pc.ctx, env, syntax_cadr(stx), 0));
}

static std::unique_ptr<expression>
parse(parsing_context& pc, tracked_ptr<environment> const& env, syntax* s) {
  syntax* stx = expand(pc.ctx, env, track(pc.ctx, s)).get(); // GC

  if (syntax_is<symbol>(stx))
    return parse_reference(env, stx);
  else if (auto sc = syntax_match<syntactic_closure>(stx))
    return parse_syntactic_closure(pc, env, sc);
  else if (syntax_is<pair>(stx)) {
    auto head = syntax_car(stx);
    if (auto form = match_core_form(pc.ctx, env, head)) {
      if (form == pc.ctx.constants->let.get())
        return parse_let(pc, env, stx);
      else if (form == pc.ctx.constants->set.get())
        return parse_define_or_set(pc, env, stx, "set!");
      else if (form == pc.ctx.constants->lambda.get())
        return parse_lambda(pc, env, stx);
      else if (form == pc.ctx.constants->if_.get())
        return parse_if(pc, env, stx);
      else if (form == pc.ctx.constants->box.get())
        return parse_box(pc, env, stx);
      else if (form == pc.ctx.constants->unbox.get())
        return parse_unbox(pc, env, stx);
      else if (form == pc.ctx.constants->box_set.get())
        return parse_box_set(pc, env, stx);
      else if (form == pc.ctx.constants->begin.get())
        return parse_sequence(pc, env, syntax_cdr(stx));
      else if (form == pc.ctx.constants->define.get())
        return parse_define_or_set(pc, env, stx, "define");
      else if (form == pc.ctx.constants->quote.get())
        return make_expression<literal_expression>(track(pc.ctx, syntax_to_datum(pc.ctx, syntax_cadr(stx))));
      else if (form == pc.ctx.constants->quasiquote.get())
        return parse_quasiquote(pc, env, stx);
      else if (form == pc.ctx.constants->expand_quote.get())
        return make_expression<literal_expression>(expand(pc.ctx, env, track(pc.ctx, syntax_cadr(stx)))); // GC
      else if (form == pc.ctx.constants->begin_for_syntax.get())
        throw syntax_error{stx, "begin-for-syntax not at top level"};
      else if (form == pc.ctx.constants->syntax_trap.get())
        return parse_syntax_trap(pc, env, stx);
      else if (form == pc.ctx.constants->syntax_error.get())
        return parse_syntax_error(pc, stx);
    }

    return parse_application(pc, env, stx);
  }
  else
    return make_expression<literal_expression>(track(pc.ctx, stx->expression()));
}

template <auto F, typename... Args>
void
recurse(expression* s, Args&... args) {
  if (std::holds_alternative<literal_expression>(s->value)
      || std::holds_alternative<local_reference_expression>(s->value)
      || std::holds_alternative<top_level_reference_expression>(s->value)) {
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
  else if (auto* top_level_set = std::get_if<top_level_set_expression>(&s->value)) {
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
    assert(!"Forgot a syntax");
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
        std::make_unique<expression>(local_reference_expression{original_set.target}),
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
        def.expression = std::make_unique<expression>(box_expression{std::move(orig_expr)});
      }
  } else if (auto* lambda = std::get_if<lambda_expression>(&s->value)) {
    for (std::shared_ptr<variable> const& param : lambda->parameters)
      if (param->is_set) {
        box_variable_references(s, param);

        auto set = std::make_unique<expression>(local_set_expression{param, {}});
        auto box = std::make_unique<expression>(box_expression{});
        auto ref = std::make_unique<expression>(local_reference_expression{param});
        std::get<box_expression>(box->value).expression = std::move(ref);
        std::get<local_set_expression>(set->value).expression = std::move(box);

        lambda->body.expressions.insert(lambda->body.expressions.begin(), std::move(set));
      }
  }
}

using variable_set = std::unordered_set<std::shared_ptr<variable>>;

// For each lambda find the list the free variables it uses.
static void
analyse_free_variables(expression* s, variable_set& bound_vars, variable_set& free_vars) {
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
    assert(bound_vars.count(set->target)); // Local set!s are boxed, so this shouldn't happen.
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

std::unique_ptr<expression>
analyse(context& ctx, syntax* stx, module& m) {
  parsing_context pc{ctx, m};
  std::unique_ptr<expression> result = parse(pc, track(ctx, m.environment()), stx);
  box_set_variables(result.get());
  analyse_free_variables(result.get());
  return result;
}

static bool
is_directive(syntax* datum, std::string const& directive) {
  return syntax_is<pair>(datum)
         && syntax_is<symbol>(expect<syntax>(car(syntax_assume<pair>(datum))))
         && syntax_assume<symbol>(expect<syntax>(car(syntax_assume<pair>(datum))))->value() == directive;
}

static module_name
parse_module_name(context& ctx, syntax* stx) {
  module_name result;

  object* datum = syntax_to_list(ctx, stx);
  if (!datum)
    throw syntax_error{stx, "Invalid module name"};

  for (object* elem : in_list{datum}) {
    syntax* e = expect<syntax>(elem);
    if (auto s = syntax_match<symbol>(e))
      result.push_back(s->value());
    else if (auto i = syntax_match<integer>(e))
      result.push_back(std::to_string(i->value()));
    else
      throw syntax_error(e, "Invalid module name");
  }

  return result;
}

static void
perform_begin_for_syntax(context& ctx, module& m, protomodule const& parent_pm, generic_tracked_ptr const& body) {
  simple_action a(ctx, "Analysing begin-for-syntax");
  protomodule pm{parent_pm.name, parent_pm.imports, {},
                 from_scheme<std::vector<tracked_ptr<syntax>>>(ctx, syntax_to_list(ctx, body.get()))};
  auto submodule = instantiate(ctx, pm);
  execute(ctx, *submodule);
  import_all_top_level(ctx, m, *submodule);
}

// Gather syntax and top-level variable definitions, expand top-level macro
// uses. Adds the found top-level syntaxes and variables to the module. Returns
// a list of the expanded top-level commands.
//
// Causes a garbage collection.
static std::vector<tracked_ptr<syntax>>
expand_top_level(context& ctx, module& m, protomodule const& pm) {
  simple_action a(ctx, "Expanding module top-level");

  std::vector<tracked_ptr<syntax>> stack;
  stack.reserve(pm.body.size());
  std::copy(pm.body.rbegin(), pm.body.rend(), std::back_inserter(stack));

  std::vector<tracked_ptr<syntax>> result;
  while (!stack.empty()) {
    tracked_ptr<syntax> stx = expand(ctx, track(ctx, m.environment()), stack.back()); // GC
    stack.pop_back();

    if (auto lst = track(ctx, syntax_to_list(ctx, stx.get()))) {
      if (lst == ctx.constants->null)
        throw syntax_error{stx.get(), "Empty application"};

      pair* p = assume<pair>(lst.get());
      if (auto form = match_core_form(ctx, track(ctx, m.environment()), expect<syntax>(car(p)))) {
        if (form == ctx.constants->define_syntax.get()) {
          auto name = track(ctx, expect_id(ctx, expect<syntax>(cadr(p))));
          auto transformer_proc = eval_transformer(ctx, m, expect<syntax>(caddr(p))); // GC
          auto transformer = make<insider::transformer>(ctx, m.environment(), transformer_proc);
          m.environment()->add(ctx.store, name->expression(), transformer);

          continue;
        }
        else if (form == ctx.constants->define.get()) {
          if (list_length(lst.get()) != 3)
            throw syntax_error(stx.get(), "Invalid define syntax");

          auto name = expect_id(ctx, expect<syntax>(cadr(p)));
          auto index = ctx.add_top_level(ctx.constants->void_.get(), identifier_name(name));
          m.add(name->expression(), index);
        }
        else if (form == ctx.constants->begin.get()) {
          std::vector<tracked_ptr<syntax>> subforms;
          for (object* e : in_list{cdr(p)})
            subforms.push_back(track(ctx, expect<syntax>(e)));

          std::copy(subforms.rbegin(), subforms.rend(), std::back_inserter(stack));
          continue;
        }
        else if (form == ctx.constants->begin_for_syntax.get()) {
          perform_begin_for_syntax(ctx, m, pm, track(ctx, cdr(p)));
          continue;
        }
      }
    }
    else if (auto sc = syntax_match<syntactic_closure>(stx.get())) {
      if (syntax* subform = transpose_syntactic_closure(ctx, sc, track(ctx, m.environment()))) {
        stack.push_back(track(ctx, subform));
        continue;
      }
    }

    result.push_back(stx);
  }

  return result;
}

static import_specifier
parse_import_set(context& ctx, syntax* stx) {
  object* spec = syntax_to_list(ctx, stx);
  if (!spec)
    throw syntax_error(stx, "import: Expected a non-empty list");

  auto p = assume<pair>(spec);

  if (auto head = syntax_match<symbol>(expect<syntax>(car(p)))) {
    if (head->value() == "only") {
      import_specifier::only result;
      result.from = std::make_unique<import_specifier>(parse_import_set(ctx, expect<syntax>(cadr(p))));

      for (object* identifier : in_list{cddr(p)})
        result.identifiers.push_back(syntax_expect<symbol>(expect<syntax>(identifier))->value());

      return import_specifier{std::move(result)};
    }
    else if (head->value() == "except") {
      import_specifier::except result;
      result.from = std::make_unique<import_specifier>(parse_import_set(ctx, expect<syntax>(cadr(p))));

      for (object* identifier : in_list{cddr(p)})
        result.identifiers.push_back(syntax_expect<symbol>(expect<syntax>(identifier))->value());

      return import_specifier{std::move(result)};
    }
    else if (head->value() == "prefix") {
      import_specifier::prefix result;
      result.from = std::make_unique<import_specifier>(parse_import_set(ctx, expect<syntax>(cadr(p))));
      result.prefix_ = syntax_expect<symbol>(expect<syntax>(caddr(p)))->value();

      return import_specifier{std::move(result)};
    }
    else if (head->value() == "rename") {
      import_specifier::rename result;
      result.from = std::make_unique<import_specifier>(parse_import_set(ctx, expect<syntax>(cadr(p))));

      for (object* name_pair_stx : in_list{cddr(p)}) {
        object* name_pair = syntax_to_list(ctx, expect<syntax>(name_pair_stx));
        if (list_length(name_pair) != 2)
          throw syntax_error(expect<syntax>(name_pair_stx), "import: rename: Expected a list of length 2");

        auto np = assume<pair>(name_pair);

        result.renames.push_back(std::tuple{syntax_expect<symbol>(expect<syntax>(car(np)))->value(),
                                            syntax_expect<symbol>(expect<syntax>(cadr(np)))->value()});
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

protomodule
read_main_module(context& ctx, std::vector<tracked_ptr<syntax>> const& contents) {
  protomodule result;

  auto stx = contents.begin();
  while (stx != contents.end()) {
    if (is_directive(stx->get(), "import")) {
      object* directive = syntax_to_list(ctx, cdr(syntax_assume<pair>(stx->get())));
      if (!directive)
        throw syntax_error{stx->get(), "Invalid import directive"};

      for (object* set : in_list{directive})
        result.imports.push_back(parse_import_set(ctx, expect<syntax>(set)));

      ++stx;
    } else
      break;
  }

  result.body.reserve(contents.end() - stx);
  for (; stx != contents.end(); ++stx)
    result.body.push_back(*stx);

  return result;
}

protomodule
read_library(context& ctx, std::vector<tracked_ptr<syntax>> const& contents) {
  if (contents.empty())
    throw error("Empty library body");

  protomodule result;
  auto current = contents.begin();
  if (is_directive(current++->get(), "library"))
    result.name = parse_module_name(ctx, syntax_cadr(contents.front().get()));
  else
    throw error("Missing library declaration");

  while (true) {
    if (is_directive(current->get(), "import")) {
      for (object* set : in_list{syntax_to_list(ctx, syntax_cdr(current->get()))})
        result.imports.push_back(parse_import_set(ctx, assume<syntax>(set)));
      ++current;
      continue;
    }
    else if (is_directive(current->get(), "export")) {
      for (object* name : in_list{syntax_to_list(ctx, syntax_cdr(current->get()))})
        result.exports.push_back(syntax_expect<symbol>(assume<syntax>(name))->value());
      ++current;
      continue;
    }
    else
      break;
  }

  result.body.reserve(contents.end() - current);
  for (; current != contents.end(); ++current)
    result.body.push_back(*current);

  return result;
}

std::optional<module_name>
read_library_name(context& ctx, port* in) {
  try {
    syntax* first_datum = read_syntax(ctx, in);
    if (is_directive(first_datum, "library"))
      return parse_module_name(ctx, syntax_cadr(first_datum));

    return {};
  }
  catch (parse_error const&) {
    // The file probably isn't a library at all. That is not an error.
    return {};
  }
}

sequence_expression
analyse_module(context& ctx, module& m, protomodule const& pm) {
  std::vector<tracked_ptr<syntax>> body = expand_top_level(ctx, m, pm);

  sequence_expression result;
  for (tracked_ptr<syntax> const& datum : body)
    result.expressions.push_back(analyse(ctx, datum.get(), m));

  return result;
}

} // namespace insider
