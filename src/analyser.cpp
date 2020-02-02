#include "analyser.hpp"

#include <fmt/format.h>

#include <set>
#include <unordered_map>
#include <vector>

namespace scm {

namespace {
  class environment {
  public:
    class scope_handle {
    public:
      explicit
      scope_handle(environment* env) : env_{env} { }

      scope_handle(scope_handle const&) = delete;
      void operator = (scope_handle const&) = delete;

      ~scope_handle() { env_->pop_scope(); }

    private:
      environment* env_;
    };

    scope_handle
    push_scope() { scopes_.emplace_back(); return scope_handle{this}; }

    void
    pop_scope() { scopes_.pop_back(); }

    void
    add(std::shared_ptr<variable> var) {
      scopes_.back().emplace(var->name, std::move(var));
    }

    std::shared_ptr<variable>
    lookup(std::string const& name) {
      for (auto s = scopes_.rbegin(); s != scopes_.rend(); ++s)
        if (auto it = s->find(name); it != s->end())
          return it->second;

      return {};
    }

  private:
    using scope = std::unordered_map<std::string, std::shared_ptr<variable>>;
    std::vector<scope> scopes_;
  };

  struct parsing_context {
    context&         ctx;
    ptr<scm::module> module;
    environment      env;
  };
} // anonymous namespace

template <typename T, typename... Args>
std::unique_ptr<syntax>
make_syntax(syntax* parent, Args&&... args) {
  return std::make_unique<syntax>(syntax{parent, T{{std::forward<Args>(args)}...}});
}

static std::unique_ptr<syntax>
parse(parsing_context& pc, syntax* parent, generic_ptr const& datum);

static definition_pair_syntax
parse_definition_pair(parsing_context& pc, syntax* parent, ptr<pair> const& datum) {
  if (!is<symbol>(car(datum)))
    throw std::runtime_error{"Invalid #$let syntax: Binding not a symbol"};

  auto name = assume<symbol>(car(datum));

  if (cdr(datum) == pc.ctx.constants.null)
    throw std::runtime_error{fmt::format("Invalid #$let syntax: No expression for {}",
                                         name->value())};
  if (!is<pair>(cdr(datum)))
    throw std::runtime_error{"Invalid #$let syntax in binding definition"};

  return {std::make_shared<variable>(name->value()), parse(pc, parent, cadr(datum))};
}

static std::vector<ptr<symbol>>
gather_defines(context& ctx, generic_ptr list) {
  std::vector<ptr<symbol>> result;

  while (list != ctx.constants.null) {
    if (auto elem = match<pair>(car(assume<pair>(list))))
      if (is_list(ctx, elem))
        if (auto head = match<symbol>(car(elem)))
          if (head->value() == "#$define") {
            if (list_length(ctx, elem) != 3 || !is<symbol>(cadr(elem)))
              throw std::runtime_error{"Invalid #$define syntax"};

            result.push_back(assume<symbol>(cadr(elem)));

            list = assume<pair>(cdr(assume<pair>(list)));
            continue;
          }

    break;
  }

  if (list == ctx.constants.null) {
    if (!result.empty())
      throw std::runtime_error{"No expression after a sequence of internal definitions"};
    else
      throw std::runtime_error{"Empty body"};
  }

  while (list != ctx.constants.null) {
    if (auto elem = match<pair>(car(assume<pair>(list))))
      if (is_list(ctx, elem))
        if (auto head = match<symbol>(car(elem)))
          if (head->value() == "#$define")
            throw std::runtime_error{"Internal define after an expression"};

    list = cdr(assume<pair>(list));
  }

  return result;
}

static std::vector<std::unique_ptr<syntax>>
parse_expression_list(parsing_context& pc, syntax* parent, generic_ptr expr) {
  std::vector<std::unique_ptr<syntax>> result;
  while (expr != pc.ctx.constants.null) {
    auto e = assume<pair>(expr);
    result.push_back(parse(pc, parent, car(e)));
    expr = cdr(e);
  }

  return result;
}

static body_syntax
parse_body(parsing_context& pc, syntax* parent, generic_ptr const& datum) {
  if (!is_list(pc.ctx, datum) || datum == pc.ctx.constants.null)
    throw std::runtime_error{"Invalid syntax: Expected a list of expressions"};

  std::vector<ptr<symbol>> internal_defines = gather_defines(pc.ctx, assume<pair>(datum));
  if (!internal_defines.empty()) {
    environment::scope_handle sh = pc.env.push_scope();

    auto let_s = make_syntax<let_syntax>(parent);
    auto& let = std::get<let_syntax>(let_s->value);

    for (ptr<symbol> const& s : internal_defines) {
      auto void_expr = make_syntax<literal_syntax>(let_s.get(), pc.ctx.constants.void_);
      auto var = std::make_shared<variable>(s->value());
      pc.env.add(var);
      let.definitions.push_back({std::move(var), std::move(void_expr)});
    }

    let.body = {parse_expression_list(pc, let_s.get(), datum)};

    body_syntax result;
    result.expressions.push_back(std::move(let_s));
    return result;
  }
  else
    return {parse_expression_list(pc, parent, datum)};
}

static std::unique_ptr<syntax>
parse_let(parsing_context& pc, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(pc.ctx, datum) || list_length(pc.ctx, datum) < 3)
    throw std::runtime_error{"Invalid #$let syntax"};

  auto bindings = cadr(datum);
  if (!is_list(pc.ctx, bindings))
    throw std::runtime_error{"Invalid #$let syntax in binding definitions"};

  auto result = make_syntax<let_syntax>(parent);
  auto& let = std::get<let_syntax>(result->value);

  while (bindings != pc.ctx.constants.null) {
    auto binding = car(assume<pair>(bindings));
    if (!is<pair>(binding))
      throw std::runtime_error{"Invalid #$let syntax in binding definitions"};

    let.definitions.push_back(parse_definition_pair(pc, result.get(), assume<pair>(binding)));
    bindings = cdr(assume<pair>(bindings));
  }

  auto sh = pc.env.push_scope();
  for (definition_pair_syntax const& dp : let.definitions)
    pc.env.add(dp.variable);

  let.body = parse_body(pc, result.get(), cddr(datum));
  return result;
}

static std::unique_ptr<syntax>
parse_set(parsing_context& pc, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(pc.ctx, datum) || list_length(pc.ctx, datum) != 3)
    throw std::runtime_error{"Invalid #$set! syntax"};

  auto name = expect<symbol>(cadr(datum), "Invalid #$set! syntax");
  if (auto local_var = pc.env.lookup(name->value())) {
    local_var->is_set = true;

    auto result = make_syntax<local_set_syntax>(parent, std::move(local_var));
    std::get<local_set_syntax>(result->value).expression = parse(pc, result.get(), caddr(datum));
    return result;
  }
  else if (auto top_level_var = pc.module->find(name->value())) {
    auto result = make_syntax<top_level_set_syntax>(parent, operand::global(*top_level_var));
    std::get<top_level_set_syntax>(result->value).expression = parse(pc, result.get(), caddr(datum));
    return result;
  }
  else
    throw std::runtime_error{fmt::format("Unbound symbol {}", name->value())};
}

static std::unique_ptr<syntax>
parse_lambda(parsing_context& pc, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(pc.ctx, cdr(datum)) || cdr(datum) == pc.ctx.constants.null)
    throw std::runtime_error{"Invalid lambda syntax"};

  generic_ptr param_names = cadr(datum);
  if (!is_list(pc.ctx, param_names))
    throw std::runtime_error{"Unimplemented"};

  auto result = make_syntax<lambda_syntax>(parent);
  auto& lambda = std::get<lambda_syntax>(result->value);

  auto sh = pc.env.push_scope();
  std::vector<ptr<symbol>> params;
  while (param_names != pc.ctx.constants.null) {
    auto param = assume<pair>(param_names);
    auto var = std::make_shared<variable>(
      expect<symbol>(car(param), "Invalid lambda syntax: Expected symbol in parameter list")->value()
    );
    lambda.parameters.push_back(var);
    pc.env.add(std::move(var));

    param_names = cdr(param);
  }

  lambda.body = parse_body(pc, result.get(), cddr(datum));
  return result;
}

static std::unique_ptr<syntax>
parse_if(parsing_context& pc, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(pc.ctx, cdr(datum)) || (list_length(pc.ctx, datum) != 3 && list_length(pc.ctx, datum) != 4))
    throw std::runtime_error{"Invalid if syntax"};

  generic_ptr test_expr = cadr(datum);
  generic_ptr then_expr = caddr(datum);
  generic_ptr else_expr;
  if (cdddr(datum) != pc.ctx.constants.null)
    else_expr = cadddr(datum);

  auto result = make_syntax<if_syntax>(parent);
  auto& if_ = std::get<if_syntax>(result->value);

  if_.test = parse(pc, result.get(), test_expr);
  if_.consequent = parse(pc, result.get(), then_expr);
  if (else_expr)
    if_.alternative = parse(pc, result.get(), else_expr);

  return result;
}

static std::unique_ptr<syntax>
parse_application(parsing_context& pc, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(pc.ctx, cdr(datum)))
    throw std::runtime_error{"Invalid function call syntax"};

  auto result = make_syntax<application_syntax>(parent);
  auto& app = std::get<application_syntax>(result->value);

  app.target = parse(pc, result.get(), car(datum));

  auto arg_expr = cdr(datum);
  while (arg_expr != pc.ctx.constants.null) {
    app.arguments.push_back(parse(pc, result.get(), car(assume<pair>(arg_expr))));
    arg_expr = cdr(assume<pair>(arg_expr));
  }

  return result;
}

static std::unique_ptr<syntax>
parse_box(parsing_context& pc, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(pc.ctx, datum) || list_length(pc.ctx, datum) != 2)
    throw std::runtime_error{"Invalid #$box syntax"};

  auto result = make_syntax<box_syntax>(parent);
  std::get<box_syntax>(result->value).expression = parse(pc, result.get(), cadr(datum));
  return result;
}

static std::unique_ptr<syntax>
parse_unbox(parsing_context& pc, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(pc.ctx, datum) || list_length(pc.ctx, datum) != 2)
    throw std::runtime_error{"Invalid #$unbox syntax"};

  auto result = make_syntax<unbox_syntax>(parent);
  std::get<unbox_syntax>(result->value).box_expr = parse(pc, result.get(), cadr(datum));
  return result;
}

static std::unique_ptr<syntax>
parse_box_set(parsing_context& pc, syntax* parent, ptr<pair> const& datum) {
  if (!is_list(pc.ctx, datum) || list_length(pc.ctx, datum) != 3)
    throw std::runtime_error{"Invalid #$box-set! syntax"};

  auto result = make_syntax<box_set_syntax>(parent);
  auto& box_set = std::get<box_set_syntax>(result->value);

  box_set.box_expr = parse(pc, result.get(), cadr(datum));
  box_set.value_expr = parse(pc, result.get(), caddr(datum));

  return result;
}

static std::unique_ptr<syntax>
parse_reference(parsing_context& pc, syntax* parent, ptr<symbol> const& datum) {
  if (auto local_var = pc.env.lookup(datum->value()))
    return make_syntax<local_reference_syntax>(parent, std::move(local_var));
  else if (auto top_level_var = pc.module->find(datum->value()))
    return make_syntax<top_level_reference_syntax>(parent, operand::global(*top_level_var), datum->value());
  else
    throw std::runtime_error{fmt::format("Unbound symbol {}", datum->value())};
}

static std::unique_ptr<syntax>
parse_define(parsing_context& pc, syntax* parent, ptr<pair> const& datum) {
  // Defines are processed in two passes: First all the define'd variables are
  // declared within the module or scope and initialised to #void; second, they
  // are assigned their values as if by set!. This is the second pass, so we
  // expect the variable to be declared already, and all we have to emit is a
  // set!.

  if (!is_list(pc.ctx, datum) || list_length(pc.ctx, datum) != 3)
    throw std::runtime_error{"Invalid #$define syntax"};

  ptr<symbol> name = expect<symbol>(cadr(datum), "Invalid #$define syntax");
  generic_ptr expr = caddr(datum);

  if (auto local_var = pc.env.lookup(name->value())) {
    local_var->is_set = true;
    auto result = make_syntax<local_set_syntax>(parent, std::move(local_var));
    std::get<local_set_syntax>(result->value).expression = parse(pc, result.get(), expr);
    return result;
  }
  else {
    std::optional<operand::representation_type> dest = pc.module->find(name->value());
    assert(dest);

    auto result = make_syntax<top_level_set_syntax>(parent, operand::global(*dest));
    std::get<top_level_set_syntax>(result->value).expression = parse(pc, result.get(), expr);
    return result;
  }
}

static std::unique_ptr<syntax>
parse(parsing_context& pc, syntax* parent, generic_ptr const& datum) {
  if (is<integer>(datum) || is<boolean>(datum) || is<void_type>(datum))
    return make_syntax<literal_syntax>(parent, datum);
  else if (auto s = match<symbol>(datum))
    return parse_reference(pc, parent, s);
  else if (auto p = match<pair>(datum)) {
    auto head = car(p);
    if (auto head_symbol = match<symbol>(head)) {
      if (head_symbol->value() == "#$let")
        return parse_let(pc, parent, p);
      else if (head_symbol->value() == "#$set!")
        return parse_set(pc, parent, p);
      else if (head_symbol->value() == "#$lambda")
        return parse_lambda(pc, parent, p);
      else if (head_symbol->value() == "#$if")
        return parse_if(pc, parent, p);
      else if (head_symbol->value() == "#$box")
        return parse_box(pc, parent, p);
      else if (head_symbol->value() == "#$unbox")
        return parse_unbox(pc, parent, p);
      else if (head_symbol->value() == "#$box-set!")
        return parse_box_set(pc, parent, p);
      else if (head_symbol->value() == "#$define")
        return parse_define(pc, parent, p);
    }

    return parse_application(pc, parent, p);
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
  else
    assert(!"Forgot a syntax");
}

static void
box_variable_references(syntax* s, std::shared_ptr<variable> const& var) {
  recurse<box_variable_references>(s, var);

  if (auto* ref = std::get_if<local_reference_syntax>(&s->value)) {
    if (ref->variable == var) {
      local_reference_syntax original_ref = *ref;
      s->value = unbox_syntax{std::make_unique<syntax>(s, original_ref)};
    }
  } else if (auto* set = std::get_if<local_set_syntax>(&s->value))
    if (set->target == var) {
      local_set_syntax original_set = std::move(*set);
      s->value = box_set_syntax{
        std::make_unique<syntax>(s, local_reference_syntax{original_set.target}),
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
        syntax* e = orig_expr.get();
        def.expression = std::make_unique<syntax>(s, box_syntax{std::move(orig_expr)});
        e->parent = def.expression.get();
      }
  } else if (auto* lambda = std::get_if<lambda_syntax>(&s->value)) {
    for (std::shared_ptr<variable> const& param : lambda->parameters)
      if (param->is_set) {
        box_variable_references(s, param);

        auto set = std::make_unique<syntax>(s, local_set_syntax{param});
        auto box = std::make_unique<syntax>(set.get(), box_syntax{});
        auto ref = std::make_unique<syntax>(box.get(), local_reference_syntax{param});
        std::get<box_syntax>(box->value).expression = std::move(ref);
        std::get<local_set_syntax>(set->value).expression = std::move(box);

        lambda->body.expressions.insert(lambda->body.expressions.begin(), std::move(set));
      }
  }
}

std::unique_ptr<syntax>
analyse(context& ctx, generic_ptr const& datum, ptr<module> const& m) {
  parsing_context pc{ctx, m};
  std::unique_ptr<syntax> result = parse(pc, nullptr, datum);
  box_set_variables(result.get());
  return result;
}

static bool
is_import(generic_ptr const& datum) {
  return is<pair>(datum)
         && is<symbol>(car(assume<pair>(datum)))
         && assume<symbol>(car(assume<pair>(datum)))->value() == "import";
}

static void
perform_import(context& ctx, ptr<module> const& m, ptr<pair> const& datum) {
  if (!is_list(ctx, datum))
    throw std::runtime_error{"Invalid import syntax"};

  ptr<pair> spec = assume<pair>(cadr(datum));
  if (!is_list(ctx, spec)
      || expect<symbol>(car(spec))->value() != "insider"
      || expect<symbol>(cadr(spec))->value() != "internal")
    throw std::runtime_error{"Unimplemented"};

  import_all(m, ctx.constants.internal);
}

static std::vector<generic_ptr>::const_iterator
perform_imports(context& ctx, ptr<module> const& m, std::vector<generic_ptr> const& data) {
  auto it = data.begin();
  while (it != data.end() && is_import(*it)) {
    perform_import(ctx, m, assume<pair>(*it));
    ++it;
  }

  return it;
}

static void
add_top_level_definitions(context& ctx, ptr<module> const& m, std::vector<generic_ptr> const& data) {
  for (generic_ptr const& datum : data)
    if (auto p = match<pair>(datum))
      if (auto head = match<symbol>(car(p)); head->value() == "#$define") {
        if (!is_list(ctx, p) || list_length(ctx, p) != 3 || !is<symbol>(cadr(p)))
          throw std::runtime_error{"Invalid #$define syntax"};

        m->add(assume<symbol>(cadr(p))->value(), ctx.add_top_level(ctx.constants.void_));
      }
}

uncompiled_module
analyse_module(context& ctx, std::vector<generic_ptr> const& data) {
  uncompiled_module result;
  result.module = make<module>(ctx);

  auto body = perform_imports(ctx, result.module, data);

  add_top_level_definitions(ctx, result.module, data);

  for (; body != data.end(); ++body)
    result.body.expressions.push_back(analyse(ctx, *body, result.module));

  return result;
}

} // namespace scm
