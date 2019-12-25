#include "syntax.hpp"

#include <fmt/format.h>

namespace game::scm {

template <typename T, typename... Args>
std::unique_ptr<syntax>
make_syntax(Args&&... args) {
  return std::make_unique<syntax>(syntax{T{{std::forward<Args>(args)}...}});
}

static definition_pair_syntax
parse_definition_pair(context& ctx, ptr<pair> const& datum, std::string const& form_name) {
  if (!is<symbol>(datum->car()))
    throw std::runtime_error{fmt::format("Invalid {} syntax: Binding not a symbol", form_name)};

  auto name = assume<symbol>(datum->car());

  if (datum->cdr() == ctx.constants->null)
    throw std::runtime_error{fmt::format("Invalid {} syntax: No expression for {}",
                                         form_name, name->value())};
  if (!is<pair>(datum->cdr()))
    throw std::runtime_error{fmt::format("Invalid {} syntax in binding definition", form_name)};

  return {name, parse(ctx, assume<pair>(datum->cdr())->car())};
}

static body_syntax
parse_body(context& ctx, generic_ptr const& datum) {
  if (!is_list(datum) || datum == ctx.constants->null)
    throw std::runtime_error{"Invalid syntax: Expected a list of expressions"};

  generic_ptr expr = datum;
  std::vector<std::unique_ptr<syntax>> result;
  while (expr != ctx.constants->null) {
    auto e = assume<pair>(expr);
    result.push_back(parse(ctx, e->car()));
    expr = e->cdr();
  }

  return body_syntax{std::move(result)};
}

static std::unique_ptr<syntax>
parse_let(context& ctx, ptr<pair> const& datum) {
  auto head = assume<symbol>(datum->car());
  std::string form = head->value() == "#$let" ? "let" : "letrec";

  if (!is_list(datum) || list_length(datum) < 3)
    throw std::runtime_error{fmt::format("Invalid {} syntax", form)};

  auto bindings = assume<pair>(datum->cdr())->car();
  if (!is_list(bindings))
    throw std::runtime_error{fmt::format("Invalid {} syntax in binding definitions", form)};

  std::vector<definition_pair_syntax> definition_pairs;
  while (bindings != ctx.constants->null) {
    auto binding = assume<pair>(bindings)->car();
    if (!is<pair>(binding))
      throw std::runtime_error{fmt::format("Invalid {} syntax in binding definitions", form)};

    definition_pairs.push_back(parse_definition_pair(ctx, assume<pair>(binding), form));
    bindings = assume<pair>(bindings)->cdr();
  }

  auto body = assume<pair>(datum->cdr())->cdr();
  if (head->value() == "#$let")
    return make_syntax<let_syntax>(std::move(definition_pairs), parse_body(ctx, body));
  else
    return make_syntax<letrec_syntax>(std::move(definition_pairs), parse_body(ctx, body));
}

static std::unique_ptr<syntax>
parse_lambda(context& ctx, ptr<pair> const& datum) {
  if (!is_list(datum->cdr()) || datum->cdr() == ctx.constants->null)
    throw std::runtime_error{"Invalid lambda syntax"};

  generic_ptr param_names = assume<pair>(datum->cdr())->car();
  if (!is_list(param_names))
    throw std::runtime_error{"Unimplemented"};

  std::vector<ptr<symbol>> params;
  while (param_names != ctx.constants->null) {
    auto param = assume<pair>(param_names);
    if (!is<symbol>(param->car()))
      throw std::runtime_error{"Invalid lambda syntax: Expected symbol in parameter list"};

    params.push_back(assume<symbol>(param->car()));
    param_names = param->cdr();
  }

  generic_ptr body = assume<pair>(datum->cdr())->cdr();
  return make_syntax<lambda_syntax>(std::move(params), parse_body(ctx, body));
}

static std::unique_ptr<syntax>
parse_if(context& ctx, ptr<pair> const& datum) {
  if (!is_list(datum->cdr()) || datum->cdr() == ctx.constants->null
      || assume<pair>(datum->cdr())->cdr() == ctx.constants->null)
    throw std::runtime_error{"Invalid if syntax"};

  generic_ptr test_expr = assume<pair>(datum->cdr())->car();
  generic_ptr then_expr = assume<pair>(assume<pair>(datum->cdr())->cdr())->car();
  generic_ptr else_expr;
  if (assume<pair>(assume<pair>(datum->cdr())->cdr())->cdr() != ctx.constants->null) {
    else_expr = assume<pair>(assume<pair>(assume<pair>(datum->cdr())->cdr())->cdr())->car();

    if (assume<pair>(assume<pair>(assume<pair>(datum->cdr())->cdr())->cdr())->cdr() != ctx.constants->null)
      throw std::runtime_error{"Invalid if syntax"};
  }

  return make_syntax<if_syntax>(parse(ctx, test_expr), parse(ctx, then_expr),
                                else_expr ? parse(ctx, else_expr) : nullptr);
}

static std::unique_ptr<syntax>
parse_application(context& ctx, ptr<pair> const& datum) {
  if (!is_list(datum->cdr()))
    throw std::runtime_error{"Invalid function call syntax"};

  auto target = parse(ctx, datum->car());

  std::vector<std::unique_ptr<syntax>> arguments;
  auto arg_expr = datum->cdr();
  while (arg_expr != ctx.constants->null) {
    arguments.push_back(parse(ctx, assume<pair>(arg_expr)->car()));
    arg_expr = assume<pair>(arg_expr)->cdr();
  }

  return make_syntax<application_syntax>(std::move(target), std::move(arguments));
}

std::unique_ptr<syntax>
parse(context& ctx, generic_ptr const& datum) {
  if (is<integer>(datum) || is<boolean>(datum))
    return make_syntax<literal_syntax>(datum);
  else if (auto s = match<symbol>(datum))
    return make_syntax<reference_syntax>(s);
  else if (auto p = match<pair>(datum)) {
    auto head = p->car();
    if (auto head_symbol = match<symbol>(head)) {
      if (head_symbol->value() == "#$let" || head_symbol->value() == "#$letrec")
        return parse_let(ctx, p);
      else if (head_symbol->value() == "#$lambda")
        return parse_lambda(ctx, p);
      else if (head_symbol->value() == "#$if")
        return parse_if(ctx, p);
    }

    return parse_application(ctx, p);
  }
  else
    throw std::runtime_error{"Unimplemented"};
}

} // namespace game::scm
