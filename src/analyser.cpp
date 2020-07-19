#include "analyser.hpp"

#include "compiler.hpp"
#include "io.hpp"
#include "numeric.hpp"
#include "vm.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <csignal>
#include <optional>
#include <set>
#include <unordered_map>
#include <vector>

namespace insider {

static std::optional<environment::value_type>
lookup(ptr<environment> env, generic_ptr const& id) {
  while (env) {
    if (auto binding = environment_lookup(env, id))
      return binding;

    env = environment_parent(env);
  }

  return std::nullopt;
}


static std::shared_ptr<variable>
lookup_variable(ptr<environment> env, generic_ptr const& id) {
  if (auto binding = lookup(env, id)) {
    if (auto var = std::get_if<std::shared_ptr<variable>>(&*binding))
      return *var;
    else
      return {};
  }

  return {};
}

static ptr<core_form_type>
lookup_core(context& ctx, ptr<environment> const& env, generic_ptr const& id) {
  auto var = lookup_variable(env, id);
  if (!var || !var->global)
    return {};  // Core forms are never defined in a local environment.

  generic_ptr form = ctx.get_top_level(*var->global);
  return match<core_form_type>(form);
}

static generic_ptr
expect_id(context& ctx, generic_ptr const& x) {
  if (!is_identifier(x))
    throw error{"Expected identifier, got {}", datum_to_string(ctx, x)};

  return x;
}

template <typename T, typename... Args>
std::unique_ptr<syntax>
make_syntax(Args&&... args) {
  return std::make_unique<syntax>(syntax{T{std::forward<Args>(args)...}});
}

static ptr<environment>
syntactic_closure_to_environment(context& ctx, ptr<syntactic_closure> const& sc, ptr<environment> const& env) {
  auto result = make<environment>(ctx, syntactic_closure_environment(sc));

  for (ptr<symbol> const& free : syntactic_closure_free(sc)) {
    if (auto binding = lookup(env, free))
      result->add(free, *binding);
  }

  return result;
}

static ptr<transformer>
lookup_transformer(context& ctx, ptr<environment> const& env, generic_ptr const& id) {
  if (auto binding = lookup(env, id)) {
    if (auto tr = std::get_if<ptr<transformer>>(&*binding))
      return *tr;
    else
      return {};
  }

  if (auto sc = match<syntactic_closure>(id)) {
    ptr<environment> env = syntactic_closure_to_environment(ctx, sc, env);
    return lookup_transformer(ctx, env, syntactic_closure_expression(sc));
  }

  return {};
}

// If the head of the given list is bound to a transformer, run the transformer
// on the datum, and repeat.
static generic_ptr
expand(context& ctx, ptr<environment> const& env, generic_ptr datum) {
  simple_action a(ctx, datum, "Expanding macro use");

  bool expanded;
  do {
    expanded = false;

    if (auto lst = match<pair>(datum)) {
      generic_ptr head = car(lst);
      if (is_identifier(head)) {
        if (ptr<transformer> t = lookup_transformer(ctx, env, head)) {
          datum = call(ctx, transformer_callable(t), {datum, transformer_environment(t), env});
          expanded = true;
        }
      }
    }
  } while (expanded);

  return datum;
}

static generic_ptr
eval_transformer(context& ctx, module& m, generic_ptr const& datum) {
  simple_action a(ctx, datum, "Evaluating transformer");
  auto proc = compile_expression(ctx, datum, m);
  auto state = make_state(ctx, proc);
  return expect_callable(run(state));
}

namespace {
  struct parsing_context {
    context&         ctx;
    insider::module& module;
  };
}

static std::unique_ptr<syntax>
parse(parsing_context& pc, ptr<environment> const&, generic_ptr const& datum);

static definition_pair_syntax
parse_definition_pair(parsing_context& pc, ptr<environment> const& env, ptr<pair> const& datum) {
  simple_action a(pc.ctx, datum, "Parsing let definition pair");

  auto id = expect_id(pc.ctx, car(datum));

  if (cdr(datum) == pc.ctx.constants->null)
    throw error("Invalid let syntax: No expression for {}", identifier_name(id));
  if (!is<pair>(cdr(datum)))
    throw error("Invalid let syntax in binding definition");

  return {id, std::make_shared<variable>(identifier_name(id)), parse(pc, env, cadr(datum))};
}

namespace {
  struct body_content {
    ptr<environment>         env;
    std::vector<generic_ptr> forms;
    std::vector<generic_ptr> internal_variable_ids;
  };
}

static ptr<core_form_type>
match_core_form(context& ctx, ptr<environment> env, generic_ptr datum) {
  while (auto sc = match<syntactic_closure>(datum)) {
    env = syntactic_closure_to_environment(ctx, sc, env);
    datum = syntactic_closure_expression(sc);
  }

  if (auto cf = match<core_form_type>(datum))
    return cf;
  else if (auto sym = match<symbol>(datum))
    return lookup_core(ctx, env, sym);
  return {};
}

static std::tuple<ptr<syntactic_closure>, ptr<environment>>
collapse_syntactic_closures(context& ctx, ptr<syntactic_closure> sc, ptr<environment> env) {
  env = syntactic_closure_to_environment(ctx, sc, env);

  while (auto inner_sc = match<syntactic_closure>(syntactic_closure_expression(sc))) {
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
static generic_ptr
transpose_syntactic_closure(context& ctx, ptr<syntactic_closure> sc, ptr<environment> env) {
  std::tie(sc, env) = collapse_syntactic_closures(ctx, sc, env);

  generic_ptr expr = syntactic_closure_expression(sc);
  if (auto p = match<pair>(expr))
    if (auto form = match_core_form(ctx, env, car(p))) {
      if (form == ctx.constants->define || form == ctx.constants->define_syntax) {
        auto name = cadr(p);
        auto expr = caddr(p);
        return make_list(ctx, form, name, make<syntactic_closure>(ctx, env, expr, ctx.constants->null));
      }
      else if (form == ctx.constants->begin) {
        std::vector<generic_ptr> exprs;
        for (generic_ptr e : in_list{cdr(p)})
          exprs.push_back(make<syntactic_closure>(ctx, env, e, ctx.constants->null));

        return cons(ctx, form, make_list_from_vector(ctx, exprs));
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
static body_content
process_internal_defines(parsing_context& pc, ptr<environment> const& env, generic_ptr list) {
  body_content result;
  result.env = make<environment>(pc.ctx, env);

  std::vector<generic_ptr> stack;
  for (generic_ptr e : in_list{list})
    stack.push_back(e);
  std::reverse(stack.begin(), stack.end());

  bool seen_expression = false;
  while (!stack.empty()) {
    generic_ptr expr = expand(pc.ctx, result.env, stack.back());
    stack.pop_back();

    if (auto p = match<pair>(expr)) {
      ptr<core_form_type> form = match_core_form(pc.ctx, result.env, car(p));

      if (form == pc.ctx.constants->define_syntax) {
        if (seen_expression)
          throw error("define-syntax after a nondefinition");

        auto name = expect_id(pc.ctx, cadr(p));
        auto transformer_proc = eval_transformer(pc.ctx, pc.module, caddr(p));
        auto transformer = make<insider::transformer>(pc.ctx, result.env, transformer_proc);
        result.env->add(name, transformer);

        continue;
      }
      else if (form == pc.ctx.constants->define) {
        if (seen_expression)
          throw error("define after a nondefinition");

        auto id = expect_id(pc.ctx, cadr(p));
        result.env->add(id, std::make_shared<variable>(identifier_name(id)));

        result.forms.push_back(expr);
        result.internal_variable_ids.push_back(id);

        continue;
      }
      else if (form == pc.ctx.constants->begin) {
        std::vector<generic_ptr> subforms;
        for (generic_ptr e : in_list{cdr(p)})
          subforms.push_back(e);

        std::copy(subforms.rbegin(), subforms.rend(), std::back_inserter(stack));
        continue;
      }
    }
    else if (auto sc = match<syntactic_closure>(expr)) {
      if (generic_ptr subform = transpose_syntactic_closure(pc.ctx, sc, result.env)) {
        stack.push_back(subform);
        continue;
      }
    }

    seen_expression = true;
    result.forms.push_back(expr);
  }

  if (!seen_expression) {
    if (!result.forms.empty())
      throw error("No expression after a sequence of internal definitions");
    else
      throw error("Empty body");
  }

  return result;
}

static std::vector<std::unique_ptr<syntax>>
parse_expression_list(parsing_context& pc, ptr<environment> const& env,
                      std::vector<generic_ptr> const& exprs) {
  std::vector<std::unique_ptr<syntax>> result;
  result.reserve(exprs.size());

  for (generic_ptr const& e : exprs)
    result.push_back(parse(pc, env, e));

  return result;
}

static sequence_syntax
parse_body(parsing_context& pc, ptr<environment> const& env, generic_ptr const& datum) {
  if (!is_list(datum) || datum == pc.ctx.constants->null)
    throw error("Invalid syntax: Expected a list of expressions");

  body_content content = process_internal_defines(pc, env, datum);
  if (!content.internal_variable_ids.empty()) {
    std::vector<definition_pair_syntax> definitions;
    for (generic_ptr const& id : content.internal_variable_ids) {
      auto void_expr = make_syntax<literal_syntax>(pc.ctx.constants->void_);
      auto var = lookup_variable(content.env, id);
      assert(var);
      definitions.push_back({id, var, std::move(void_expr)});
    }

    sequence_syntax result;
    result.expressions.push_back(make_syntax<let_syntax>(std::move(definitions),
                                                         parse_expression_list(pc, content.env, content.forms)));
    return result;
  }
  else
    return {parse_expression_list(pc, content.env, content.forms)};
}

static std::unique_ptr<syntax>
parse_let(parsing_context& pc, ptr<environment> const& env, ptr<pair> const& datum) {
  simple_action a(pc.ctx, datum, "Parsing let");

  if (!is_list(datum) || list_length(datum) < 3)
    throw error("Invalid let syntax");

  auto bindings = cadr(datum);
  if (!is_list(bindings))
    throw error("Invalid let syntax in binding definitions");

  std::vector<definition_pair_syntax> definitions;
  while (bindings != pc.ctx.constants->null) {
    auto binding = car(assume<pair>(bindings));
    if (!is<pair>(binding))
      throw error("Invalid let syntax in binding definitions");

    definitions.push_back(parse_definition_pair(pc, env, assume<pair>(binding)));
    bindings = cdr(assume<pair>(bindings));
  }

  auto subenv = make<environment>(pc.ctx, env);
  for (definition_pair_syntax const& dp : definitions)
    subenv->add(dp.id, dp.variable);

  return make_syntax<let_syntax>(std::move(definitions), parse_body(pc, subenv, cddr(datum)));
}

static std::unique_ptr<syntax>
parse_lambda(parsing_context& pc, ptr<environment> const& env, ptr<pair> const& datum) {
  simple_action a(pc.ctx, datum, "Parsing lambda");

  if (!is_list(cdr(datum)) || cdr(datum) == pc.ctx.constants->null)
    throw error("Invalid lambda syntax");

  generic_ptr param_names = cadr(datum);
  std::vector<std::shared_ptr<variable>> parameters;
  bool has_rest = false;
  auto subenv = make<environment>(pc.ctx, env);
  while (param_names != pc.ctx.constants->null) {
    if (auto param = match<pair>(param_names)) {
      auto id = expect_id(pc.ctx, car(param));
      auto var = std::make_shared<variable>(identifier_name(id));
      parameters.push_back(var);
      subenv->add(id, std::move(var));

      param_names = cdr(param);
    }
    else if (is_identifier(param_names)) {
      has_rest = true;
      auto id = param_names;
      auto var = std::make_shared<variable>(identifier_name(id));
      parameters.push_back(var);
      subenv->add(id, std::move(var));
      break;
    }
    else
      throw error{"Unexpected value in lambda parameters: {}", datum_to_string(pc.ctx, param_names)};
  }

  return make_syntax<lambda_syntax>(std::move(parameters), has_rest,
                                    parse_body(pc, subenv, cddr(datum)),
                                    std::nullopt);
}

static std::unique_ptr<syntax>
parse_if(parsing_context& pc, ptr<environment> const& env, ptr<pair> const& datum) {
  simple_action a(pc.ctx, datum, "Parsing if");

  if (!is_list(cdr(datum)) || (list_length(datum) != 3 && list_length(datum) != 4))
    throw error("Invalid if syntax");

  generic_ptr test_expr = cadr(datum);
  generic_ptr then_expr = caddr(datum);
  generic_ptr else_expr;
  if (cdddr(datum) != pc.ctx.constants->null)
    else_expr = cadddr(datum);

  return make_syntax<if_syntax>(parse(pc, env, test_expr),
                                parse(pc, env, then_expr),
                                else_expr ? parse(pc, env, else_expr) : nullptr);
}

static std::unique_ptr<syntax>
parse_application(parsing_context& pc, ptr<environment> const& env, ptr<pair> const& datum) {
  simple_action a(pc.ctx, datum, "Parsing procedure application");

  if (!is_list(cdr(datum)))
    throw error("Invalid function call syntax");

  std::vector<std::unique_ptr<syntax>> arguments;
  auto arg_expr = cdr(datum);
  while (arg_expr != pc.ctx.constants->null) {
    arguments.push_back(parse(pc, env, car(assume<pair>(arg_expr))));
    arg_expr = cdr(assume<pair>(arg_expr));
  }

  return make_syntax<application_syntax>(parse(pc, env, car(datum)), std::move(arguments));
}

static std::unique_ptr<syntax>
parse_box(parsing_context& pc, ptr<environment> const& env, ptr<pair> const& datum) {
  if (!is_list(datum) || list_length(datum) != 2)
    throw error("Invalid box syntax");

  return make_syntax<box_syntax>(parse(pc, env, cadr(datum)));
}

static std::unique_ptr<syntax>
parse_unbox(parsing_context& pc, ptr<environment> const& env, ptr<pair> const& datum) {
  if (!is_list(datum) || list_length(datum) != 2)
    throw error("Invalid unbox syntax");

  return make_syntax<unbox_syntax>(parse(pc, env, cadr(datum)));
}

static std::unique_ptr<syntax>
parse_box_set(parsing_context& pc, ptr<environment> const& env, ptr<pair> const& datum) {
  if (!is_list(datum) || list_length(datum) != 3)
    throw error("Invalid box-set! syntax");

  return make_syntax<box_set_syntax>(parse(pc, env, cadr(datum)),
                                     parse(pc, env, caddr(datum)));
}

static std::unique_ptr<syntax>
parse_sequence(parsing_context& pc, ptr<environment> const& env, ptr<pair> const& datum) {
  std::vector<std::unique_ptr<syntax>> exprs;
  for (generic_ptr expr : in_list{cdr(datum)})
    exprs.push_back(parse(pc, env, expr));

  return make_syntax<sequence_syntax>(std::move(exprs));
}

static std::unique_ptr<syntax>
parse_reference(ptr<environment> const& env, ptr<symbol> const& id) {
  auto var = lookup_variable(env, id);

  if (!var)
    throw error("Identifier {} not bound to a variable", identifier_name(id));

  if (!var->global)
    return make_syntax<local_reference_syntax>(std::move(var));
  else
    return make_syntax<top_level_reference_syntax>(operand::global(*var->global), identifier_name(id));
}

static std::unique_ptr<syntax>
parse_define_or_set(parsing_context& pc, ptr<environment> const& env, ptr<pair> const& datum,
                    std::string const& form_name) {
  // Defines are processed in two passes: First all the define'd variables are
  // declared within the module or scope and initialised to #void; second, they
  // are assigned their values as if by set!.
  //
  // This function can be therefore be used for both set! and define forms -- in
  // either case, we emit a set! syntax.

  if (!is_list(datum) || list_length(datum) != 3)
    throw error("Invalid {} syntax", form_name);

  generic_ptr name = expect_id(pc.ctx, cadr(datum));
  generic_ptr expr = caddr(datum);

  auto var = lookup_variable(env, name);
  if (!var)
    throw error("Identifier {} not bound to a variable", identifier_name(name));

  auto initialiser = parse(pc, env, expr);
  if (auto l = std::get_if<lambda_syntax>(&initialiser->value))
    l->name = identifier_name(name);

  if (!var->global) {
    var->is_set = true;
    return make_syntax<local_set_syntax>(std::move(var), std::move(initialiser));
  }
  else
    return make_syntax<top_level_set_syntax>(operand::global(*var->global), std::move(initialiser));
}

static std::unique_ptr<syntax>
parse_syntactic_closure(parsing_context& pc, ptr<environment> const& env,
                        ptr<syntactic_closure> const& sc) {
  if (is<symbol>(syntactic_closure_expression(sc))) {
    if (auto var = lookup_variable(env, sc)) {
      if (!var->global)
        return make_syntax<local_reference_syntax>(std::move(var));
      else
        return make_syntax<top_level_reference_syntax>(operand::global(*var->global), identifier_name(sc));
    }
  }

  auto new_env = syntactic_closure_to_environment(pc.ctx, sc, env);
  return parse(pc, new_env, syntactic_closure_expression(sc));
}

static std::unique_ptr<syntax>
parse_syntax_trap(parsing_context& pc, ptr<environment> const& env, ptr<pair> const& datum) {
  raise(SIGTRAP);
  return parse(pc, env, cadr(datum));
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
    generic_ptr value;
  };

  struct unquote {
    generic_ptr datum;
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

    explicit
    qq_template(value_type value)
      : value(std::move(value))
    { }
  };
} // anonymous namespace

static std::unique_ptr<qq_template>
parse_qq_template(context& ctx, ptr<environment> const& env,
                  generic_ptr const& datum, unsigned quote_level) {
  if (auto p = match<pair>(datum)) {
    unsigned nested_level = quote_level;

    if (auto form = match_core_form(ctx, env, car(p))) {
      if (form == ctx.constants->unquote) {
        if (quote_level == 0)
          return std::make_unique<qq_template>(unquote{cadr(p), false});
        else
          nested_level = quote_level - 1;
      }
      else if (form == ctx.constants->unquote_splicing) {
        if (quote_level == 0)
          return std::make_unique<qq_template>(unquote{cadr(p), true});
        else
          nested_level = quote_level - 1;
      }
      else if (form == ctx.constants->quasiquote)
        nested_level = quote_level + 1;
    }

    bool all_literal = true;
    list_pattern result;

    generic_ptr elem = p;
    while (!is<null_type>(elem)) {
      auto current = assume<pair>(elem);
      if (!is<pair>(cdr(current)) && !is<null_type>(cdr(current)))
        break; // Improper list.

      result.elems.push_back(parse_qq_template(ctx, env, car(current), nested_level));
      elem = cdr(current);

      if (!std::holds_alternative<literal>(result.elems.back()->value))
        all_literal = false;
    }

    if (auto pair = match<insider::pair>(elem)) {
      result.last = cons_pattern{parse_qq_template(ctx, env, car(pair), nested_level),
                                 parse_qq_template(ctx, env, cdr(pair), nested_level)};
      all_literal = all_literal
                    && std::holds_alternative<literal>(result.last->car->value)
                    && std::holds_alternative<literal>(result.last->cdr->value);
    }

    if (all_literal)
      return std::make_unique<qq_template>(literal{datum});
    else
      return std::make_unique<qq_template>(std::move(result));
  }
  else if (auto v = match<vector>(datum)) {
    std::vector<std::unique_ptr<qq_template>> templates;
    templates.reserve(v->size());
    bool all_literal = true;

    for (std::size_t i = 0; i < v->size(); ++i) {
      templates.push_back(parse_qq_template(ctx, env, vector_ref(v, i), quote_level));
      if (!std::holds_alternative<literal>(templates.back()->value))
        all_literal = false;
    }

    if (all_literal)
      return std::make_unique<qq_template>(literal{datum});
    else
      return std::make_unique<qq_template>(vector_pattern{std::move(templates)});
  }
  else
    return std::make_unique<qq_template>(literal{datum});
}

static std::unique_ptr<syntax>
make_internal_reference(context& ctx, std::string name) {
  std::optional<module::binding_type> binding = ctx.internal_module.find(ctx.intern(name));
  assert(binding);
  assert(std::holds_alternative<module::index_type>(*binding));
  return make_syntax<top_level_reference_syntax>(operand::global(std::get<module::index_type>(*binding)),
                                                 std::move(name));
}

static bool
is_splice(std::unique_ptr<qq_template> const& tpl) {
  if (auto* expr = std::get_if<unquote>(&tpl->value))
    if (expr->splicing)
      return true;
  return false;
}

static std::unique_ptr<syntax>
process_qq_template(parsing_context& pc, ptr<environment> const& env,
                    std::unique_ptr<qq_template> const& tpl) {
  if (auto* cp = std::get_if<list_pattern>(&tpl->value)) {
    std::unique_ptr<syntax> tail;
    if (cp->last) {
      if (is_splice(cp->last->cdr))
        throw error("Invalid use of unquote-splicing");

      if (is_splice(cp->last->car))
        tail = make_syntax<application_syntax>(make_internal_reference(pc.ctx, "append"),
                                               process_qq_template(pc, env, cp->last->car),
                                               process_qq_template(pc, env, cp->last->cdr));
      else
        tail = make_syntax<cons_syntax>(process_qq_template(pc, env, cp->last->car),
                                        process_qq_template(pc, env, cp->last->cdr));
    }

    for (auto elem = cp->elems.rbegin(); elem != cp->elems.rend(); ++elem) {
      if (tail) {
        if (is_splice(*elem))
          tail = make_syntax<application_syntax>(make_internal_reference(pc.ctx, "append"),
                                                 process_qq_template(pc, env, *elem),
                                                 std::move(tail));
        else
          tail = make_syntax<cons_syntax>(process_qq_template(pc, env, *elem),
                                          std::move(tail));
      } else {
        if (is_splice(*elem))
          tail = process_qq_template(pc, env, *elem);
        else
          tail = make_syntax<cons_syntax>(process_qq_template(pc, env, *elem),
                                          make_syntax<literal_syntax>(pc.ctx.constants->null));
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
      auto result = make_syntax<application_syntax>(make_internal_reference(pc.ctx, "vector-append"));
      application_syntax& app = std::get<application_syntax>(result->value);

      std::vector<std::unique_ptr<syntax>> chunk;
      for (std::unique_ptr<qq_template> const& elem : vp->elems) {
        if (is_splice(elem)) {
          if (!chunk.empty()) {
            app.arguments.push_back(make_syntax<make_vector_syntax>(std::move(chunk)));
            chunk.clear();
          }

          app.arguments.push_back(make_syntax<application_syntax>(make_internal_reference(pc.ctx, "list->vector"),
                                                                  process_qq_template(pc, env, elem)));
        }
        else
          chunk.push_back(process_qq_template(pc, env, elem));
      }

      if (!chunk.empty())
        app.arguments.push_back(make_syntax<make_vector_syntax>(std::move(chunk)));

      return result;
    }
    else {
      std::vector<std::unique_ptr<syntax>> elements;
      elements.reserve(vp->elems.size());

      for (std::unique_ptr<qq_template> const& elem : vp->elems)
        elements.push_back(process_qq_template(pc, env, elem));

      return make_syntax<make_vector_syntax>(std::move(elements));
    }
  }
  else if (auto* expr = std::get_if<unquote>(&tpl->value))
    return parse(pc, env, expr->datum);
  else if (auto* lit = std::get_if<literal>(&tpl->value))
    return make_syntax<literal_syntax>(lit->value);

  assert(!"Forgot a pattern");
  return {};
}

static std::unique_ptr<syntax>
parse_quasiquote(parsing_context& pc, ptr<environment> const& env, ptr<pair> const& datum) {
  return process_qq_template(pc, env, parse_qq_template(pc.ctx, env, cadr(datum), 0));
}

static std::unique_ptr<syntax>
parse(parsing_context& pc, ptr<environment> const& env, generic_ptr const& d) {
  generic_ptr datum = expand(pc.ctx, env, d);

  if (auto s = match<symbol>(datum))
    return parse_reference(env, s);
  else if (auto sc = match<syntactic_closure>(datum))
    return parse_syntactic_closure(pc, env, sc);
  else if (auto p = match<pair>(datum)) {
    auto head = car(p);
    if (auto form = match_core_form(pc.ctx, env, head)) {
      if (form == pc.ctx.constants->let)
        return parse_let(pc, env, p);
      else if (form == pc.ctx.constants->set)
        return parse_define_or_set(pc, env, p, "set!");
      else if (form == pc.ctx.constants->lambda)
        return parse_lambda(pc, env, p);
      else if (form == pc.ctx.constants->if_)
        return parse_if(pc, env, p);
      else if (form == pc.ctx.constants->box)
        return parse_box(pc, env, p);
      else if (form == pc.ctx.constants->unbox)
        return parse_unbox(pc, env, p);
      else if (form == pc.ctx.constants->box_set)
        return parse_box_set(pc, env, p);
      else if (form == pc.ctx.constants->begin)
        return parse_sequence(pc, env, p);
      else if (form == pc.ctx.constants->define)
        return parse_define_or_set(pc, env, p, "define");
      else if (form == pc.ctx.constants->quote)
        return make_syntax<literal_syntax>(cadr(p));
      else if (form == pc.ctx.constants->quasiquote)
        return parse_quasiquote(pc, env, p);
      else if (form == pc.ctx.constants->expand_quote)
        return make_syntax<literal_syntax>(expand(pc.ctx, env, cadr(p)));
      else if (form == pc.ctx.constants->begin_for_syntax)
        throw error{"begin-for-syntax not at top level"};
      else if (form == pc.ctx.constants->syntax_trap)
        return parse_syntax_trap(pc, env, p);
    }

    return parse_application(pc, env, p);
  }
  else
    return make_syntax<literal_syntax>(datum);
}

template <auto F, typename... Args>
void
recurse(syntax* s, Args&... args) {
  if (std::holds_alternative<literal_syntax>(s->value)
      || std::holds_alternative<local_reference_syntax>(s->value)
      || std::holds_alternative<top_level_reference_syntax>(s->value)) {
    // Nothing to recurse into.
  }
  else if (auto* app = std::get_if<application_syntax>(&s->value)) {
    F(app->target.get(), args...);
    for (auto const& arg : app->arguments)
      F(arg.get(), args...);
  }
  else if (auto* let = std::get_if<let_syntax>(&s->value)) {
    for (auto const& def : let->definitions)
      F(def.expression.get(), args...);
    for (auto const& expr : let->body.expressions)
      F(expr.get(), args...);
  }
  else if (auto* local_set = std::get_if<local_set_syntax>(&s->value)) {
    F(local_set->expression.get(), args...);
  }
  else if (auto* top_level_set = std::get_if<top_level_set_syntax>(&s->value)) {
    F(top_level_set->expression.get(), args...);
  }
  else if (auto* lambda = std::get_if<lambda_syntax>(&s->value)) {
    for (auto const& expr : lambda->body.expressions)
      F(expr.get(), args...);
  }
  else if (auto* if_ = std::get_if<if_syntax>(&s->value)) {
    F(if_->test.get(), args...);
    F(if_->consequent.get(), args...);
    if (if_->alternative)
      F(if_->alternative.get(), args...);
  }
  else if (auto* box = std::get_if<box_syntax>(&s->value)) {
    F(box->expression.get(), args...);
  }
  else if (auto* unbox = std::get_if<unbox_syntax>(&s->value)) {
    F(unbox->box_expr.get(), args...);
  }
  else if (auto* box_set = std::get_if<box_set_syntax>(&s->value)) {
    F(box_set->box_expr.get(), args...);
    F(box_set->value_expr.get(), args...);
  }
  else if (auto* cons = std::get_if<cons_syntax>(&s->value)) {
    F(cons->car.get(), args...);
    F(cons->cdr.get(), args...);
  }
  else if (auto* make_vector = std::get_if<make_vector_syntax>(&s->value)) {
    for (std::unique_ptr<syntax> const& e : make_vector->elements)
      F(e.get(), args...);
  }
  else if (auto* sequence = std::get_if<sequence_syntax>(&s->value)) {
    for (std::unique_ptr<syntax> const& e : sequence->expressions)
      F(e.get(), args...);
  }
  else
    assert(!"Forgot a syntax");
}

static void
box_variable_references(syntax* s, std::shared_ptr<variable> const& var) {
  recurse<box_variable_references>(s, var);

  if (auto* ref = std::get_if<local_reference_syntax>(&s->value)) {
    if (ref->variable == var) {
      local_reference_syntax original_ref = *ref;
      s->value = unbox_syntax{std::make_unique<syntax>(original_ref)};
    }
  } else if (auto* set = std::get_if<local_set_syntax>(&s->value))
    if (set->target == var) {
      local_set_syntax original_set = std::move(*set);
      s->value = box_set_syntax{
        std::make_unique<syntax>(local_reference_syntax{original_set.target}),
        std::move(original_set.expression)
      };
    }
}

static void
box_set_variables(syntax* s) {
  recurse<box_set_variables>(s);

  if (auto* let = std::get_if<let_syntax>(&s->value)) {
    for (definition_pair_syntax& def : let->definitions)
      if (def.variable->is_set) {
        box_variable_references(s, def.variable);

        std::unique_ptr<syntax> orig_expr = std::move(def.expression);
        def.expression = std::make_unique<syntax>(box_syntax{std::move(orig_expr)});
      }
  } else if (auto* lambda = std::get_if<lambda_syntax>(&s->value)) {
    for (std::shared_ptr<variable> const& param : lambda->parameters)
      if (param->is_set) {
        box_variable_references(s, param);

        auto set = std::make_unique<syntax>(local_set_syntax{param, {}});
        auto box = std::make_unique<syntax>(box_syntax{});
        auto ref = std::make_unique<syntax>(local_reference_syntax{param});
        std::get<box_syntax>(box->value).expression = std::move(ref);
        std::get<local_set_syntax>(set->value).expression = std::move(box);

        lambda->body.expressions.insert(lambda->body.expressions.begin(), std::move(set));
      }
  }
}

std::unique_ptr<syntax>
analyse(context& ctx, generic_ptr const& datum, module& m) {
  parsing_context pc{ctx, m};
  std::unique_ptr<syntax> result = parse(pc, m.environment(), datum);
  box_set_variables(result.get());
  return result;
}

static bool
is_directive(generic_ptr const& datum, std::string const& directive) {
  return is<pair>(datum)
         && is<symbol>(car(assume<pair>(datum)))
         && assume<symbol>(car(assume<pair>(datum)))->value() == directive;
}

static module_name
parse_module_name(generic_ptr const& datum) {
  module_name result;

  for (generic_ptr elem : in_list{datum})
    if (auto s = match<symbol>(elem))
      result.push_back(s->value());
    else if (auto i = match<integer>(elem))
      result.push_back(std::to_string(i->value()));
    else
      throw error("Invalid module name");;

  return result;
}

static void
perform_begin_for_syntax(context& ctx, module& m, protomodule const& parent_pm, generic_ptr const& body) {
  simple_action a(ctx, "Analysing begin-for-syntax");
  protomodule pm{parent_pm.name, parent_pm.imports, {}, list_to_std_vector(body)};
  auto submodule = instantiate(ctx, pm);
  execute(ctx, *submodule);
  import_all_top_level(ctx, m, *submodule);
}

// Gather syntax and top-level variable definitions, expand top-level macro
// uses. Adds the found top-level syntaxes and variables to the module. Returns
// a list of the expanded top-level commands.
static std::vector<generic_ptr>
expand_top_level(context& ctx, module& m, protomodule const& pm) {
  simple_action a(ctx, "Expanding module top-level");

  std::vector<generic_ptr> stack;
  stack.reserve(pm.body.size());
  std::copy(pm.body.rbegin(), pm.body.rend(), std::back_inserter(stack));

  std::vector<generic_ptr> result;
  while (!stack.empty()) {
    generic_ptr datum = expand(ctx, m.environment(), stack.back());
    stack.pop_back();

    if (auto p = match<pair>(datum)) {
      if (auto form = match_core_form(ctx, m.environment(), car(p))) {
        if (form == ctx.constants->define_syntax) {
          auto name = expect_id(ctx, cadr(p));
          auto transformer_proc = eval_transformer(ctx, m, caddr(p));
          auto transformer = make<insider::transformer>(ctx, m.environment(), transformer_proc);
          m.environment()->add(name, transformer);

          continue;
        }
        else if (form == ctx.constants->define) {
          if (!is_list(p) || list_length(p) != 3)
            throw error("Invalid define syntax");

          auto name = expect_id(ctx, cadr(p));
          auto index = ctx.add_top_level(ctx.constants->void_, identifier_name(name));
          m.add(name, index);
        }
        else if (form == ctx.constants->begin) {
          std::vector<generic_ptr> subforms;
          for (generic_ptr e : in_list{cdr(p)})
            subforms.push_back(e);

          std::copy(subforms.rbegin(), subforms.rend(), std::back_inserter(stack));
          continue;
        }
        else if (form == ctx.constants->begin_for_syntax) {
          perform_begin_for_syntax(ctx, m, pm, cdr(p));
          continue;
        }
      }
    }
    else if (auto sc = match<syntactic_closure>(datum)) {
      if (generic_ptr subform = transpose_syntactic_closure(ctx, sc, m.environment())) {
        stack.push_back(subform);
        continue;
      }
    }

    result.push_back(datum);
  }

  return result;
}

static import_specifier
parse_import_set(context& ctx, generic_ptr const& spec) {
  if (is<null_type>(spec) || !is_list(spec))
    throw error("import: Expected a non-empty list");

  auto p = assume<pair>(spec);

  if (auto head = match<symbol>(car(p))) {
    if (head->value() == "only") {
      import_specifier::only result;
      result.from = std::make_unique<import_specifier>(parse_import_set(ctx, cadr(p)));

      for (generic_ptr identifier : in_list{cddr(p)})
        result.identifiers.push_back(expect<symbol>(identifier)->value());

      return import_specifier{std::move(result)};
    }
    else if (head->value() == "except") {
      import_specifier::except result;
      result.from = std::make_unique<import_specifier>(parse_import_set(ctx, cadr(p)));

      for (generic_ptr identifier : in_list{cddr(p)})
        result.identifiers.push_back(expect<symbol>(identifier)->value());

      return import_specifier{std::move(result)};
    }
    else if (head->value() == "prefix") {
      import_specifier::prefix result;
      result.from = std::make_unique<import_specifier>(parse_import_set(ctx, cadr(p)));
      result.prefix = expect<symbol>(caddr(p))->value();

      return import_specifier{std::move(result)};
    }
    else if (head->value() == "rename") {
      import_specifier::rename result;
      result.from = std::make_unique<import_specifier>(parse_import_set(ctx, cadr(p)));

      for (generic_ptr name_pair : in_list{cddr(p)}) {
        if (list_length(name_pair) != 2)
          throw error("import: rename: Expected a list of length 2");

        auto np = assume<pair>(name_pair);

        result.renames.push_back(std::tuple{expect<symbol>(car(np))->value(),
                                            expect<symbol>(cadr(np))->value()});
      }

      return import_specifier{std::move(result)};
    }
    else
      return import_specifier{parse_module_name(p)};
  }
  else
    return import_specifier{parse_module_name(p)};

  assert(!"Unreachable");
  throw std::logic_error{"Unreachable"};
}

protomodule
read_main_module(context& ctx, std::vector<generic_ptr> const& data) {
  protomodule result;

  auto datum = data.begin();
  while (datum != data.end()) {
    if (is_directive(*datum, "import")) {
      for (generic_ptr set : in_list{cdr(assume<pair>(*datum))})
        result.imports.push_back(parse_import_set(ctx, set));

      ++datum;
    } else
      break;
  }

  result.body.reserve(data.end() - datum);
  for (; datum != data.end(); ++datum)
    result.body.push_back(*datum);

  return result;
}

protomodule
read_library(context& ctx, std::vector<generic_ptr> const& data) {
  if (data.empty())
    throw error("Empty library body");

  protomodule result;
  auto current = data.begin();
  if (is_directive(*current++, "library"))
    result.name = parse_module_name(cadr(expect<pair>(data.front())));
  else
    throw error("Missing library declaration");

  while (true) {
    if (is_directive(*current, "import")) {
      for (generic_ptr set : in_list{cdr(assume<pair>(*current))})
        result.imports.push_back(parse_import_set(ctx, set));
      ++current;
      continue;
    }
    else if (is_directive(*current, "export")) {
      for (generic_ptr name : in_list{cdr(assume<pair>(*current))})
        result.exports.push_back(expect<symbol>(name)->value());
      ++current;
      continue;
    }
    else
      break;
  }

  result.body.reserve(data.end() - current);
  for (; current != data.end(); ++current)
    result.body.push_back(*current);

  return result;
}

std::optional<module_name>
read_library_name(context& ctx, ptr<port> const& in) {
  try {
    generic_ptr first_datum = read(ctx, in);
    if (is_directive(first_datum, "library"))
      return parse_module_name(cadr(assume<pair>(first_datum)));

    return {};
  }
  catch (parse_error const&) {
    // The file probably isn't a library at all. That is not an error.
    return {};
  }
}

sequence_syntax
analyse_module(context& ctx, module& m, protomodule const& pm) {
  std::vector<generic_ptr> body = expand_top_level(ctx, m, pm);

  sequence_syntax result;
  for (generic_ptr const& datum : body)
    result.expressions.push_back(analyse(ctx, datum, m));

  return result;
}

} // namespace insider
