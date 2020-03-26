#include "analyser.hpp"

#include "compiler.hpp"
#include "vm.hpp"

#include <fmt/format.h>

#include <optional>
#include <set>
#include <unordered_map>
#include <vector>

namespace scm {

static std::shared_ptr<variable>
lookup(std::shared_ptr<environment> env, std::string const& name) {
  while (env) {
    if (auto it = env->bindings.find(name); it != env->bindings.end())
      return it->second;

    env = env->parent;
  }

  return {};
}

static ptr<transformer>
lookup_transformer(context& ctx, std::shared_ptr<environment> const& env, std::string const& name) {
  if (auto var = lookup(env, name)) {
    if (var->transformer)
      return var->transformer;
    else if (var->global && ctx.find_tag(*var->global) == special_top_level_tag::syntax)
      return expect<transformer>(ctx.get_top_level(*var->global));
  }

  return {};
}

static ptr<core_form_type>
lookup_core(context& ctx, std::shared_ptr<environment> const& env, std::string const& name) {
  auto var = lookup(env, name);
  if (!var || !var->global)
    return {};  // Core forms are never defined in a local environment.

  generic_ptr form = ctx.get_top_level(*var->global);
  return match<core_form_type>(form);
}

template <typename T, typename... Args>
std::unique_ptr<syntax>
make_syntax(Args&&... args) {
  return std::make_unique<syntax>(syntax{T{std::forward<Args>(args)...}});
}

// If the head of the given list is bound to a transformer, run the transformer
// on the datum, and repeat.
static generic_ptr
expand(context& ctx, std::shared_ptr<environment> const& env, generic_ptr datum) {
  while (auto lst = match<pair>(datum)) {
    if (auto head = match<symbol>(car(lst))) {
      if (ptr<transformer> t = lookup_transformer(ctx, env, head->value())) {
        auto trans_env = make<environment_holder>(ctx, t->environment());
        auto usage_env = make<environment_holder>(ctx, env);
        datum = call(ctx, transformer_procedure(t), {datum, trans_env, usage_env});
        continue;
      }
    }

    break;
  }

  return datum;
}

static ptr<procedure>
eval_transformer(context& ctx, module& m, generic_ptr const& datum) {
  auto proc = compile_expression(ctx, datum, m);
  auto state = make_state(ctx, proc);
  return expect<procedure>(run(state));
}

static std::shared_ptr<environment>
syntactic_closure_to_environment(ptr<syntactic_closure> const& sc, std::shared_ptr<environment> const& env) {
  auto result = std::make_shared<environment>(sc->environment);

  for (ptr<symbol> const& free : syntactic_closure_free(sc))
    if (auto var = lookup(env, free->value()))
      result->bindings.emplace(free->value(), var);

  return result;
}

namespace {
  struct parsing_context {
    context&     ctx;
    scm::module& module;
  };
}

static std::unique_ptr<syntax>
parse(parsing_context& pc, std::shared_ptr<environment> const&, generic_ptr const& datum);

static definition_pair_syntax
parse_definition_pair(parsing_context& pc, std::shared_ptr<environment> const& env, ptr<pair> const& datum) {
  if (!is<symbol>(car(datum)))
    throw std::runtime_error{"Invalid let syntax: Binding not a symbol"};

  auto name = assume<symbol>(car(datum));

  if (cdr(datum) == pc.ctx.constants.null)
    throw std::runtime_error{fmt::format("Invalid let syntax: No expression for {}",
                                         name->value())};
  if (!is<pair>(cdr(datum)))
    throw std::runtime_error{"Invalid let syntax in binding definition"};

  return {std::make_shared<variable>(name->value()), parse(pc, env, cadr(datum))};
}

namespace {
  struct body_content {
    std::shared_ptr<environment> env;
    std::vector<generic_ptr>     forms;
    std::vector<std::string>     internal_variable_names;
  };
}

// Process the beginning of a body by adding new transformer and variable
// definitions to the environment and expanding the heads of each form in the
// list. Also checks that the list is followed by at least one expression and
// that internal definitions are not interleaved with expessions. Internal
// variable definitions are bound in the returned environment to #void values;
// internal transformer definitions are bound to the transformer.
static body_content
process_internal_defines(parsing_context& pc, std::shared_ptr<environment> const& env, generic_ptr list) {
  body_content result;
  result.env = std::make_shared<environment>(env);

  bool seen_expression = false;
  for (generic_ptr e : in_list{list}) {
    generic_ptr expr = expand(pc.ctx, result.env, e);
    if (auto p = match<pair>(expr)) {
      if (auto head = match<symbol>(car(p))) {
        auto form = lookup_core(pc.ctx, env, head->value());
        if (form == pc.ctx.constants.define_syntax) {
          if (seen_expression)
            throw std::runtime_error{"define-syntax after a nondefinition"};

          auto name = expect<symbol>(cadr(p));
          auto transformer_proc = eval_transformer(pc.ctx, pc.module, caddr(p));
          auto transformer = make<scm::transformer>(pc.ctx, env, transformer_proc);
          bool inserted = result.env->bindings.emplace(name->value(),
                                                       std::make_shared<variable>(name->value(), transformer)).second;

          if (!inserted)
            throw std::runtime_error{fmt::format("Cannot redefine {} as a syntax", name->value())};

          continue;
        }
        else if (form == pc.ctx.constants.define) {
          if (seen_expression)
            throw std::runtime_error{"define after a nondefinition"};

          auto name = expect<symbol>(cadr(p));
          bool inserted = result.env->bindings.emplace(name->value(),
                                                       std::make_shared<variable>(name->value())).second;

          if (!inserted)
            throw std::runtime_error{fmt::format("Cannot redefine {}", name->value())};

          result.forms.push_back(expr);
          result.internal_variable_names.push_back(name->value());

          continue;
        }
      }
    }

    seen_expression = true;
    result.forms.push_back(expr);
  }

  if (!seen_expression) {
    if (!result.forms.empty())
      throw std::runtime_error{"No expression after a sequence of internal definitions"};
    else
      throw std::runtime_error{"Empty body"};
  }

  return result;
}

static std::vector<std::unique_ptr<syntax>>
parse_expression_list(parsing_context& pc, std::shared_ptr<environment> const& env,
                      std::vector<generic_ptr> const& exprs) {
  std::vector<std::unique_ptr<syntax>> result;
  result.reserve(exprs.size());

  for (generic_ptr const& e : exprs)
    result.push_back(parse(pc, env, e));

  return result;
}

static body_syntax
parse_body(parsing_context& pc, std::shared_ptr<environment> const& env, generic_ptr const& datum) {
  if (!is_list(datum) || datum == pc.ctx.constants.null)
    throw std::runtime_error{"Invalid syntax: Expected a list of expressions"};

  body_content content = process_internal_defines(pc, env, datum);
  if (!content.internal_variable_names.empty()) {
    std::vector<definition_pair_syntax> definitions;
    for (std::string const& name : content.internal_variable_names) {
      auto void_expr = make_syntax<literal_syntax>(pc.ctx.constants.void_);
      auto var = content.env->bindings.find(name);
      assert(var != env->bindings.end());
      definitions.push_back({var->second, std::move(void_expr)});
    }

    body_syntax result;
    result.expressions.push_back(make_syntax<let_syntax>(std::move(definitions),
                                                         parse_expression_list(pc, content.env, content.forms)));
    return result;
  }
  else
    return {parse_expression_list(pc, content.env, content.forms)};
}

static std::unique_ptr<syntax>
parse_let(parsing_context& pc, std::shared_ptr<environment> const& env, ptr<pair> const& datum) {
  if (!is_list(datum) || list_length(datum) < 3)
    throw std::runtime_error{"Invalid let syntax"};

  auto bindings = cadr(datum);
  if (!is_list(bindings))
    throw std::runtime_error{"Invalid let syntax in binding definitions"};

  std::vector<definition_pair_syntax> definitions;
  while (bindings != pc.ctx.constants.null) {
    auto binding = car(assume<pair>(bindings));
    if (!is<pair>(binding))
      throw std::runtime_error{"Invalid let syntax in binding definitions"};

    definitions.push_back(parse_definition_pair(pc, env, assume<pair>(binding)));
    bindings = cdr(assume<pair>(bindings));
  }

  auto subenv = std::make_shared<environment>(env);
  for (definition_pair_syntax const& dp : definitions)
    subenv->bindings.emplace(dp.variable->name, dp.variable);

  return make_syntax<let_syntax>(std::move(definitions), parse_body(pc, subenv, cddr(datum)));
}

static std::shared_ptr<variable>
lookup_expression(std::shared_ptr<environment> env, generic_ptr datum) {
  while (auto sc = match<syntactic_closure>(datum)) {
    env = syntactic_closure_to_environment(sc, env);
    datum = syntactic_closure_expression(sc);
  }

  return lookup(env, expect<symbol>(datum, "Expected symbol")->value());
}

static std::unique_ptr<syntax>
parse_set(parsing_context& pc, std::shared_ptr<environment> const& env, ptr<pair> const& datum) {
  if (!is_list(datum) || list_length(datum) != 3)
    throw std::runtime_error{"Invalid set! syntax"};

  auto var = lookup_expression(env, cadr(datum));

  if (!var)
    throw std::runtime_error{fmt::format("Unbound symbol {}", var->name)};

  if (!var->global) {
    var->is_set = true;
    return make_syntax<local_set_syntax>(std::move(var), parse(pc, env, caddr(datum)));
  }
  else
    return make_syntax<top_level_set_syntax>(operand::global(*var->global),
                                             parse(pc, env, caddr(datum)));
}

static std::unique_ptr<syntax>
parse_lambda(parsing_context& pc, std::shared_ptr<environment> const& env, ptr<pair> const& datum) {
  if (!is_list(cdr(datum)) || cdr(datum) == pc.ctx.constants.null)
    throw std::runtime_error{"Invalid lambda syntax"};

  generic_ptr param_names = cadr(datum);
  if (!is_list(param_names))
    throw std::runtime_error{"Unimplemented"};

  std::vector<std::shared_ptr<variable>> parameters;
  auto subenv = std::make_shared<environment>(env);
  while (param_names != pc.ctx.constants.null) {
    auto param = assume<pair>(param_names);
    auto var = std::make_shared<variable>(
      expect<symbol>(car(param), "Invalid lambda syntax: Expected symbol in parameter list")->value()
    );
    parameters.push_back(var);
    subenv->bindings.emplace(var->name, var);

    param_names = cdr(param);
  }

  return make_syntax<lambda_syntax>(std::move(parameters), parse_body(pc, subenv, cddr(datum)));
}

static std::unique_ptr<syntax>
parse_if(parsing_context& pc, std::shared_ptr<environment> const& env, ptr<pair> const& datum) {
  if (!is_list(cdr(datum)) || (list_length(datum) != 3 && list_length(datum) != 4))
    throw std::runtime_error{"Invalid if syntax"};

  generic_ptr test_expr = cadr(datum);
  generic_ptr then_expr = caddr(datum);
  generic_ptr else_expr;
  if (cdddr(datum) != pc.ctx.constants.null)
    else_expr = cadddr(datum);

  return make_syntax<if_syntax>(parse(pc, env, test_expr),
                                parse(pc, env, then_expr),
                                else_expr ? parse(pc, env, else_expr) : nullptr);
}

static std::unique_ptr<syntax>
parse_application(parsing_context& pc, std::shared_ptr<environment> const& env, ptr<pair> const& datum) {
  if (!is_list(cdr(datum)))
    throw std::runtime_error{"Invalid function call syntax"};

  std::vector<std::unique_ptr<syntax>> arguments;
  auto arg_expr = cdr(datum);
  while (arg_expr != pc.ctx.constants.null) {
    arguments.push_back(parse(pc, env, car(assume<pair>(arg_expr))));
    arg_expr = cdr(assume<pair>(arg_expr));
  }

  return make_syntax<application_syntax>(parse(pc, env, car(datum)), std::move(arguments));
}

static std::unique_ptr<syntax>
parse_box(parsing_context& pc, std::shared_ptr<environment> const& env, ptr<pair> const& datum) {
  if (!is_list(datum) || list_length(datum) != 2)
    throw std::runtime_error{"Invalid box syntax"};

  return make_syntax<box_syntax>(parse(pc, env, cadr(datum)));
}

static std::unique_ptr<syntax>
parse_unbox(parsing_context& pc, std::shared_ptr<environment> const& env, ptr<pair> const& datum) {
  if (!is_list(datum) || list_length(datum) != 2)
    throw std::runtime_error{"Invalid unbox syntax"};

  return make_syntax<unbox_syntax>(parse(pc, env, cadr(datum)));
}

static std::unique_ptr<syntax>
parse_box_set(parsing_context& pc, std::shared_ptr<environment> const& env, ptr<pair> const& datum) {
  if (!is_list(datum) || list_length(datum) != 3)
    throw std::runtime_error{"Invalid box-set! syntax"};

  return make_syntax<box_set_syntax>(parse(pc, env, cadr(datum)),
                                     parse(pc, env, caddr(datum)));
}

static std::unique_ptr<syntax>
parse_reference(std::shared_ptr<environment> const& env, ptr<symbol> const& datum) {
  auto var = lookup(env, datum->value());

  if (!var)
    throw std::runtime_error{fmt::format("Unbound symbol {}", datum->value())};

  if (!var->global)
    return make_syntax<local_reference_syntax>(std::move(var));
  else
    return make_syntax<top_level_reference_syntax>(operand::global(*var->global), datum->value());
}

static std::unique_ptr<syntax>
parse_define(parsing_context& pc, std::shared_ptr<environment> const& env, ptr<pair> const& datum) {
  // Defines are processed in two passes: First all the define'd variables are
  // declared within the module or scope and initialised to #void; second, they
  // are assigned their values as if by set!. This is the second pass, so we
  // expect the variable to be declared already, and all we have to emit is a
  // set!.

  if (!is_list(datum) || list_length(datum) != 3)
    throw std::runtime_error{"Invalid define syntax"};

  ptr<symbol> name = expect<symbol>(cadr(datum), "Invalid define syntax");
  generic_ptr expr = caddr(datum);

  auto var = lookup(env, name->value());
  assert(var);

  if (!var->global) {
    var->is_set = true;
    return make_syntax<local_set_syntax>(std::move(var), parse(pc, env, expr));
  }
  else
    return make_syntax<top_level_set_syntax>(operand::global(*var->global), parse(pc, env, expr));
}

static std::unique_ptr<syntax>
parse_syntactic_closure(parsing_context& pc, std::shared_ptr<environment> const& env,
                        ptr<syntactic_closure> const& sc) {
  auto new_env = syntactic_closure_to_environment(sc, env);
  return parse(pc, new_env, syntactic_closure_expression(sc));
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
parse_qq_template(context& ctx, std::shared_ptr<environment> const& env,
                  generic_ptr const& datum, unsigned quote_level) {
  if (auto p = match<pair>(datum)) {
    unsigned nested_level = quote_level;

    if (auto s = match<symbol>(car(p))) {
      auto form = lookup_core(ctx, env, s->value());
      if (form == ctx.constants.unquote) {
        if (quote_level == 0)
          return std::make_unique<qq_template>(unquote{cadr(p), false});
        else
          nested_level = quote_level - 1;
      }
      else if (form == ctx.constants.unquote_splicing) {
        if (quote_level == 0)
          return std::make_unique<qq_template>(unquote{cadr(p), true});
        else
          nested_level = quote_level - 1;
      }
      else if (form == ctx.constants.quasiquote)
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

    if (auto pair = match<scm::pair>(elem)) {
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
  std::optional<module::index_type> index = ctx.internal_module.find(name);
  assert(index);
  return make_syntax<top_level_reference_syntax>(operand::global(*index), std::move(name));
}

static bool
is_splice(std::unique_ptr<qq_template> const& tpl) {
  if (auto* expr = std::get_if<unquote>(&tpl->value))
    if (expr->splicing)
      return true;
  return false;
}

static std::unique_ptr<syntax>
process_qq_template(parsing_context& pc, std::shared_ptr<environment> const& env,
                    std::unique_ptr<qq_template> const& tpl) {
  if (auto* cp = std::get_if<list_pattern>(&tpl->value)) {
    std::unique_ptr<syntax> tail;
    if (cp->last) {
      if (is_splice(cp->last->cdr))
        throw std::runtime_error{"Invalid use of unquote-splicing"};

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
                                          make_syntax<literal_syntax>(pc.ctx.constants.null));
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
parse_quasiquote(parsing_context& pc, std::shared_ptr<environment> const& env, ptr<pair> const& datum) {
  return process_qq_template(pc, env, parse_qq_template(pc.ctx, env, cadr(datum), 0));
}

static std::unique_ptr<syntax>
parse(parsing_context& pc, std::shared_ptr<environment> const& env, generic_ptr const& d) {
  generic_ptr datum = expand(pc.ctx, env, d);

  if (is<integer>(datum) || is<boolean>(datum) || is<void_type>(datum) || is<string>(datum))
    return make_syntax<literal_syntax>(datum);
  else if (auto s = match<symbol>(datum))
    return parse_reference(env, s);
  else if (auto sc = match<syntactic_closure>(datum))
    return parse_syntactic_closure(pc, env, sc);
  else if (auto p = match<pair>(datum)) {
    auto head = car(p);
    if (auto head_symbol = match<symbol>(head)) {
      auto form = lookup_core(pc.ctx, env, head_symbol->value());
      if (form == pc.ctx.constants.let)
        return parse_let(pc, env, p);
      else if (form == pc.ctx.constants.set)
        return parse_set(pc, env, p);
      else if (form == pc.ctx.constants.lambda)
        return parse_lambda(pc, env, p);
      else if (form == pc.ctx.constants.if_)
        return parse_if(pc, env, p);
      else if (form == pc.ctx.constants.box)
        return parse_box(pc, env, p);
      else if (form == pc.ctx.constants.unbox)
        return parse_unbox(pc, env, p);
      else if (form == pc.ctx.constants.box_set)
        return parse_box_set(pc, env, p);
      else if (form == pc.ctx.constants.define)
        return parse_define(pc, env, p);
      else if (form == pc.ctx.constants.quote)
        return make_syntax<literal_syntax>(cadr(p));
      else if (form == pc.ctx.constants.quasiquote)
        return parse_quasiquote(pc, env, p);
    }

    return parse_application(pc, env, p);
  }
  else
    throw std::runtime_error{"Unimplemented"};
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

        auto set = std::make_unique<syntax>(local_set_syntax{param});
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
      throw std::runtime_error{fmt::format("Invalid module name")};

  return result;
}

// Gather syntax and top-level variable definitions, expand top-level macro
// uses. Adds the found top-level syntaxes and variables to the module. Returns
// a list of the expanded top-level commands.
static std::vector<generic_ptr>
expand_top_level(context& ctx, module& m, std::vector<generic_ptr> const& data) {
  // First, scan the module for variable and syntax definitions.

  std::vector<generic_ptr> first_pass;

  for (generic_ptr const& d : data) {
    generic_ptr datum = expand(ctx, m.environment(), d);

    if (auto p = match<pair>(datum)) {
      if (auto head = match<symbol>(car(p))) {
        auto form = lookup_core(ctx, m.environment(), head->value());

        if (form == ctx.constants.define_syntax) {
          auto name = expect<symbol>(cadr(p));
          auto transformer_proc = expect<procedure>(eval_transformer(ctx, m, caddr(p)));
          auto transformer = make<scm::transformer>(ctx, m.environment(), transformer_proc);

          auto index = ctx.add_top_level(transformer);
          ctx.tag_top_level(index, special_top_level_tag::syntax);
          m.add(name->value(), index);

          continue;
        }
        else if (form == ctx.constants.define) {
          if (!is_list(p) || list_length(p) != 3 || !is<symbol>(cadr(p)))
            throw std::runtime_error{"Invalid define syntax"};

          auto name = assume<symbol>(cadr(p));
          auto index = ctx.add_top_level(ctx.constants.void_);
          m.add(name->value(), index);
        }
      }
    }

    first_pass.push_back(datum);
  }

  // Second, expand all macro uses and gather the result.
  std::vector<generic_ptr> result;

  for (generic_ptr const& datum : first_pass) {
    if (auto p = match<pair>(datum))
      if (auto head = match<symbol>(car(p)))
        if (lookup_core(ctx, m.environment(), head->value()) == ctx.constants.define_syntax)
          continue;

    result.push_back(datum);
  }

  return result;
}

protomodule
read_main_module(std::vector<generic_ptr> const& data) {
  protomodule result;

  auto datum = data.begin();
  while (datum != data.end()) {
    if (is_directive(*datum, "import")) {
      result.imports.push_back(parse_module_name(cadr(assume<pair>(*datum))));
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
read_library(std::vector<generic_ptr> const& data) {
  if (data.empty())
    throw std::runtime_error{"Empty library body"};

  protomodule result;
  auto current = data.begin();
  if (is_directive(*current++, "library"))
    result.name = parse_module_name(cadr(expect<pair>(data.front())));
  else
    throw std::runtime_error{"Missing library declaration"};

  while (true) {
    if (is_directive(*current, "import")) {
      result.imports.push_back(parse_module_name(cadr(assume<pair>(*current))));
      ++current;
      continue;
    }
    else if (is_directive(*current, "export")) {
      result.exports.push_back(expect<symbol>(cadr(assume<pair>(*current)))->value());
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

body_syntax
analyse_module(context& ctx, module& m, std::vector<generic_ptr> const& data) {
  std::vector<generic_ptr> body = expand_top_level(ctx, m, data);

  body_syntax result;
  for (generic_ptr const& datum : body)
    result.expressions.push_back(analyse(ctx, datum, m));

  return result;
}

} // namespace scm
