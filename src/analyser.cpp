#include "analyser.hpp"

#include <fmt/format.h>

#include <set>
#include <vector>

namespace game::scm {

template <typename T, typename... Args>
std::unique_ptr<syntax>
make_syntax(syntax* parent, Args&&... args) {
  return std::make_unique<syntax>(syntax{parent, T{{std::forward<Args>(args)}...}});
}

static std::unique_ptr<syntax>
parse(context& ctx, syntax* parent, generic_ptr const& datum);

static definition_pair_syntax
parse_definition_pair(context& ctx, syntax* parent, ptr<pair> const& datum) {
  if (!is<symbol>(datum->car()))
    throw std::runtime_error{"Invalid #$let syntax: Binding not a symbol"};

  auto name = assume<symbol>(datum->car());

  if (datum->cdr() == ctx.constants->null)
    throw std::runtime_error{fmt::format("Invalid #$let syntax: No expression for {}",
                                         name->value())};
  if (!is<pair>(datum->cdr()))
    throw std::runtime_error{"Invalid #$let syntax in binding definition"};

  return {name, parse(ctx, parent, assume<pair>(datum->cdr())->car())};
}

static body_syntax
parse_body(context& ctx, syntax* parent, generic_ptr const& datum) {
  if (!is_list(datum) || datum == ctx.constants->null)
    throw std::runtime_error{"Invalid syntax: Expected a list of expressions"};

  generic_ptr expr = datum;
  std::vector<std::unique_ptr<syntax>> result;
  while (expr != ctx.constants->null) {
    auto e = assume<pair>(expr);
    result.push_back(parse(ctx, parent, e->car()));
    expr = e->cdr();
  }

  return body_syntax{std::move(result)};
}

static std::unique_ptr<syntax>
parse_let(context& ctx, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(datum) || list_length(datum) < 3)
    throw std::runtime_error{"Invalid #$let syntax"};

  auto bindings = cadr(datum);
  if (!is_list(bindings))
    throw std::runtime_error{"Invalid #$let syntax in binding definitions"};

  auto result = make_syntax<let_syntax>(parent);
  auto& let = std::get<let_syntax>(result->value);

  while (bindings != ctx.constants->null) {
    auto binding = assume<pair>(bindings)->car();
    if (!is<pair>(binding))
      throw std::runtime_error{"Invalid #$let syntax in binding definitions"};

    let.definitions.push_back(parse_definition_pair(ctx, result.get(), assume<pair>(binding)));
    bindings = assume<pair>(bindings)->cdr();
  }

  let.body = parse_body(ctx, result.get(), cddr(datum));
  return result;
}

static std::unique_ptr<syntax>
parse_set(context& ctx, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(datum) || list_length(datum) != 3)
    throw std::runtime_error{"Invalid #$set! syntax"};

  auto result = make_syntax<set_syntax>(parent, expect<symbol>(cadr(datum), "Invalid #$set! syntax"));
  std::get<set_syntax>(result->value).expression = parse(ctx, result.get(), caddr(datum));
  return result;
}

static std::unique_ptr<syntax>
parse_lambda(context& ctx, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(datum->cdr()) || datum->cdr() == ctx.constants->null)
    throw std::runtime_error{"Invalid lambda syntax"};

  generic_ptr param_names = cadr(datum);
  if (!is_list(param_names))
    throw std::runtime_error{"Unimplemented"};

  auto result = make_syntax<lambda_syntax>(parent);
  auto& lambda = std::get<lambda_syntax>(result->value);

  std::vector<ptr<symbol>> params;
  while (param_names != ctx.constants->null) {
    auto param = assume<pair>(param_names);
    lambda.parameters.push_back(expect<symbol>(param->car(),
                                               "Invalid lambda syntax: Expected symbol in parameter list"));
    param_names = param->cdr();
  }

  lambda.body = parse_body(ctx, result.get(), cddr(datum));
  return result;
}

static std::unique_ptr<syntax>
parse_if(context& ctx, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(datum->cdr()) || (list_length(datum) != 3 && list_length(datum) != 4))
    throw std::runtime_error{"Invalid if syntax"};

  generic_ptr test_expr = cadr(datum);
  generic_ptr then_expr = caddr(datum);
  generic_ptr else_expr;
  if (cdddr(datum) != ctx.constants->null)
    else_expr = cadddr(datum);

  auto result = make_syntax<if_syntax>(parent);
  auto& if_ = std::get<if_syntax>(result->value);

  if_.test = parse(ctx, result.get(), test_expr);
  if_.consequent = parse(ctx, result.get(), then_expr);
  if (else_expr)
    if_.alternative = parse(ctx, result.get(), else_expr);

  return result;
}

static std::unique_ptr<syntax>
parse_application(context& ctx, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(datum->cdr()))
    throw std::runtime_error{"Invalid function call syntax"};

  auto result = make_syntax<application_syntax>(parent);
  auto& app = std::get<application_syntax>(result->value);

  app.target = parse(ctx, result.get(), datum->car());

  auto arg_expr = datum->cdr();
  while (arg_expr != ctx.constants->null) {
    app.arguments.push_back(parse(ctx, result.get(), assume<pair>(arg_expr)->car()));
    arg_expr = assume<pair>(arg_expr)->cdr();
  }

  return result;
}

static std::unique_ptr<syntax>
parse_box(context& ctx, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(datum) || list_length(datum) != 2)
    throw std::runtime_error{"Invalid #$box syntax"};

  auto result = make_syntax<box_syntax>(parent);
  std::get<box_syntax>(result->value).expression = parse(ctx, result.get(), cadr(datum));
  return result;
}

static std::unique_ptr<syntax>
parse_unbox(context& ctx, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(datum) || list_length(datum) != 2)
    throw std::runtime_error{"Invalid #$unbox syntax"};

  auto result = make_syntax<unbox_syntax>(parent);
  std::get<unbox_syntax>(result->value).box_expr = parse(ctx, result.get(), cadr(datum));
  return result;
}

static std::unique_ptr<syntax>
parse_box_set(context& ctx, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(datum) || list_length(datum) != 3)
    throw std::runtime_error{"Invalid #$box-set! syntax"};

  auto result = make_syntax<box_set_syntax>(parent);
  auto& box_set = std::get<box_set_syntax>(result->value);

  box_set.box_expr = parse(ctx, result.get(), cadr(datum));
  box_set.value_expr = parse(ctx, result.get(), caddr(datum));

  return result;
}

static std::unique_ptr<syntax>
parse(context& ctx, syntax* parent, generic_ptr const& datum) {
  if (is<integer>(datum) || is<boolean>(datum) || is<void_type>(datum))
    return make_syntax<literal_syntax>(parent, datum);
  else if (auto s = match<symbol>(datum))
    return make_syntax<reference_syntax>(parent, s);
  else if (auto p = match<pair>(datum)) {
    auto head = p->car();
    if (auto head_symbol = match<symbol>(head)) {
      if (head_symbol->value() == "#$let")
        return parse_let(ctx, parent, p);
      else if (head_symbol->value() == "#$set!")
        return parse_set(ctx, parent, p);
      else if (head_symbol->value() == "#$lambda")
        return parse_lambda(ctx, parent, p);
      else if (head_symbol->value() == "#$if")
        return parse_if(ctx, parent, p);
      else if (head_symbol->value() == "#$box")
        return parse_box(ctx, parent, p);
      else if (head_symbol->value() == "#$unbox")
        return parse_unbox(ctx, parent, p);
      else if (head_symbol->value() == "#$box-set!")
        return parse_box_set(ctx, parent, p);
    }

    return parse_application(ctx, parent, p);
  }
  else
    throw std::runtime_error{"Unimplemented"};
}

namespace {
  struct variable_declaration {
    syntax*     declarator;
    ptr<symbol> name;
  };

  static bool
  operator < (variable_declaration const& lhs, variable_declaration const& rhs) {
    return lhs.declarator < rhs.declarator
           || (lhs.declarator == rhs.declarator && lhs.name->value() < rhs.name->value());
  }
}

static bool
declares_variable(let_syntax const& let, ptr<symbol> const& name) {
  for (auto const& def : let.definitions)
    if (def.name == name)
      return true;

  return false;
}

static bool
declares_variable(lambda_syntax const& lambda, ptr<symbol> const& name) {
  for (ptr<symbol> const& def : lambda.parameters)
    if (def == name)
      return true;

  return false;
}

static syntax*
find_declarator(syntax* s) {
  set_syntax const& set = std::get<set_syntax>(s->value);
  syntax* current = s->parent;
  while (current) {
    if (auto* let = std::get_if<let_syntax>(&current->value)) {
      if (declares_variable(*let, set.target))
        return current;
    } else if (auto* lambda = std::get_if<lambda_syntax>(&current->value)) {
      if (declares_variable(*lambda, set.target))
        return current;
    }

    current = current->parent;
  }

  return nullptr;
}

template <auto F, typename... Args>
void
recurse(syntax* s, Args&... args) {
  if (std::holds_alternative<literal_syntax>(s->value) || std::holds_alternative<reference_syntax>(s->value)) {
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
  else if (auto* set = std::get_if<set_syntax>(&s->value)) {
    F(set->expression.get(), args...);
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
  else
    assert(!"Forgot a syntax");
}

static void
find_set_variables(syntax* s, std::set<variable_declaration>& result) {
  if (auto* set = std::get_if<set_syntax>(&s->value)) {
    if (syntax* declarator = find_declarator(s))
      result.insert({declarator, set->target});
  }

  recurse<find_set_variables>(s, result);
}

static std::set<variable_declaration>
find_set_variables(syntax* s) {
  std::set<variable_declaration> result;
  find_set_variables(s, result);
  return result;
}

static void
box_variable_references(syntax* s, ptr<symbol> const& name) {
  recurse<box_variable_references>(s, name);

  if (auto* ref = std::get_if<reference_syntax>(&s->value)) {
    if (ref->symbol == name) {
      reference_syntax original_ref = *ref;
      s->value = unbox_syntax{std::make_unique<syntax>(s, original_ref)};
    }
  } else if (auto* set = std::get_if<set_syntax>(&s->value))
    if (set->target == name) {
      set_syntax original_set = std::move(*set);
      s->value = box_set_syntax{
        std::make_unique<syntax>(s, reference_syntax{original_set.target}),
        std::move(original_set.expression)
      };
    }
}

static void
box_set_variables(syntax* s, std::set<variable_declaration> const& set_vars) {
  recurse<box_set_variables>(s, set_vars);

  if (auto* let = std::get_if<let_syntax>(&s->value)) {
    for (definition_pair_syntax& def : let->definitions)
      if (set_vars.count({s, def.name})) {
        box_variable_references(s, def.name);

        std::unique_ptr<syntax> orig_expr = std::move(def.expression);
        syntax* e = orig_expr.get();
        def.expression = std::make_unique<syntax>(s, box_syntax{std::move(orig_expr)});
        e->parent = def.expression.get();
      }
  } else if (auto* lambda = std::get_if<lambda_syntax>(&s->value)) {
    for (ptr<symbol> const& param : lambda->parameters)
      if (set_vars.count({s, param})) {
        box_variable_references(s, param);

        auto set = std::make_unique<syntax>(s, set_syntax{param});
        auto box = std::make_unique<syntax>(set.get(), box_syntax{});
        auto ref = std::make_unique<syntax>(box.get(), reference_syntax{param});
        std::get<box_syntax>(box->value).expression = std::move(ref);
        std::get<set_syntax>(set->value).expression = std::move(box);

        lambda->body.expressions.insert(lambda->body.expressions.begin(), std::move(set));
      }
  }
}

static void
box_set_variables(syntax* s) {
  box_set_variables(s, find_set_variables(s));
}

std::unique_ptr<syntax>
analyse(context& ctx, generic_ptr const& datum) {
  std::unique_ptr<syntax> result = parse(ctx, nullptr, datum);
  box_set_variables(result.get());
  return result;
}

} // namespace game::scm
