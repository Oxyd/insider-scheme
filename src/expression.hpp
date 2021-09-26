#ifndef INSIDER_EXPRESSION_HPP
#define INSIDER_EXPRESSION_HPP

#include "bytecode.hpp"
#include "free_store.hpp"
#include "module_name.hpp"
#include "source_file_origin.hpp"

#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace insider {

class syntax;
class transformer;

// The binding between a name and its value. For top-level values, this directly
// contains the index of the value. Otherwise, it's just an object representing
// the binding itself and the compiler will use these to translate them to local
// registers.
struct variable {
  std::string            name;
  bool                   is_set = false;
  std::optional<operand> global;

  explicit
  variable(std::string n) : name{std::move(n)} { }

  variable(std::string n, operand index)
    : name{std::move(n)}
    , global{index}
  { }
};

struct expression;

struct literal_expression {
  tracked_ptr<> value;

  explicit
  literal_expression(tracked_ptr<> const& value) : value{value} { }
};

struct local_reference_expression {
  std::shared_ptr<insider::variable> variable;

  explicit
  local_reference_expression(std::shared_ptr<insider::variable> var)
    : variable{std::move(var)}
  { }
};

struct top_level_reference_expression {
  operand location;
  std::string name;

  top_level_reference_expression(operand location, std::string name)
    : location{location}
    , name{std::move(name)}
  { }
};

struct application_expression {
  std::unique_ptr<expression> target;
  std::vector<std::unique_ptr<expression>> arguments;

  application_expression(std::unique_ptr<expression> t,
                         std::vector<std::unique_ptr<expression>> args)
    : target{std::move(t)}
    , arguments{std::move(args)}
  { }

  template <typename... Ts>
  application_expression(std::unique_ptr<expression> t, Ts&&... ts)
    : target{std::move(t)}
  {
    arguments.reserve(sizeof...(Ts));
    (arguments.push_back(std::move(ts)), ...);
  }
};

struct sequence_expression {
  std::vector<std::unique_ptr<expression>> expressions;

  sequence_expression() = default;

  explicit
  sequence_expression(std::vector<std::unique_ptr<expression>> exprs)
    : expressions{std::move(exprs)}
  { }
};

struct definition_pair_expression {
  tracked_ptr<syntax>                  id;
  std::shared_ptr<insider::variable>   variable;
  std::unique_ptr<insider::expression> expression;

  definition_pair_expression(tracked_ptr<syntax> id, std::shared_ptr<insider::variable> var,
                             std::unique_ptr<insider::expression> expr)
    : id{id}
    , variable{std::move(var)}
    , expression{std::move(expr)}
  { }
};

struct let_expression {
  std::vector<definition_pair_expression> definitions;
  sequence_expression body;

  let_expression(std::vector<definition_pair_expression> defs, sequence_expression body)
    : definitions{std::move(defs)}
    , body{std::move(body)}
  { }
};

struct local_set_expression {
  std::shared_ptr<variable>            target;
  std::unique_ptr<insider::expression> expression;

  local_set_expression(std::shared_ptr<variable> target, std::unique_ptr<insider::expression> expr)
    : target{std::move(target)}
    , expression{std::move(expr)}
  { }
};

struct top_level_set_expression {
  operand location;
  std::unique_ptr<insider::expression> expression;

  top_level_set_expression(operand location, std::unique_ptr<insider::expression> expr)
    : location{location}
    , expression{std::move(expr)}
  { }
};

struct lambda_expression {
  std::vector<std::shared_ptr<variable>> parameters;
  bool has_rest;
  sequence_expression body;
  std::optional<std::string> name;
  std::vector<std::shared_ptr<variable>> free_variables;

  lambda_expression(std::vector<std::shared_ptr<variable>> parameters,
                    bool has_rest,
                    sequence_expression body,
                    std::optional<std::string> name,
                    std::vector<std::shared_ptr<variable>> free_variables)
    : parameters{std::move(parameters)}
    , has_rest{has_rest}
    , body{std::move(body)}
    , name{std::move(name)}
    , free_variables{std::move(free_variables)}
  { }
};

struct if_expression {
  std::unique_ptr<expression> test;
  std::unique_ptr<expression> consequent;
  std::unique_ptr<expression> alternative;

  if_expression(std::unique_ptr<expression> test,
                std::unique_ptr<expression> consequent,
                std::unique_ptr<expression> alternative)
    : test{std::move(test)}
    , consequent{std::move(consequent)}
    , alternative{std::move(alternative)}
  { }
};

struct box_expression {
  std::unique_ptr<insider::expression> expression;

  box_expression() = default;

  explicit
  box_expression(std::unique_ptr<insider::expression> expression)
    : expression{std::move(expression)}
  { }
};

struct unbox_expression {
  std::unique_ptr<expression> box_expr;

  explicit
  unbox_expression(std::unique_ptr<expression> box_expr)
    : box_expr{std::move(box_expr)}
  { }
};

struct box_set_expression {
  std::unique_ptr<expression> box_expr;
  std::unique_ptr<expression> value_expr;

  box_set_expression(std::unique_ptr<expression> box_expr, std::unique_ptr<expression> value_expr)
    : box_expr{std::move(box_expr)}
    , value_expr{std::move(value_expr)}
  { }
};

struct cons_expression {
  std::unique_ptr<expression> car;
  std::unique_ptr<expression> cdr;

  cons_expression(std::unique_ptr<expression> car, std::unique_ptr<expression> cdr)
    : car{std::move(car)}
    , cdr{std::move(cdr)}
  { }
};

struct make_vector_expression {
  std::vector<std::unique_ptr<expression>> elements;

  explicit
  make_vector_expression(std::vector<std::unique_ptr<expression>> elements)
    : elements{std::move(elements)}
  { }
};

struct expression {
  using value_type = std::variant<
    literal_expression,
    local_reference_expression,
    top_level_reference_expression,
    application_expression,
    let_expression,
    local_set_expression,
    top_level_set_expression,
    lambda_expression,
    if_expression,
    box_expression,
    unbox_expression,
    box_set_expression,
    cons_expression,
    make_vector_expression,
    sequence_expression
  >;

  value_type value;

  explicit
  expression(value_type value)
    : value{std::move(value)}
  { }
};

struct import_specifier {
  struct only {
    std::unique_ptr<import_specifier> from;
    std::vector<std::string>          identifiers;

    only() = default;

    only(std::unique_ptr<import_specifier> from, std::vector<std::string> identifiers)
      : from{std::move(from)}
      , identifiers{std::move(identifiers)}
    { }
  };

  struct except {
    std::unique_ptr<import_specifier> from;
    std::vector<std::string>          identifiers;

    except() = default;

    except(std::unique_ptr<import_specifier> from, std::vector<std::string> identifiers)
      : from{std::move(from)}
      , identifiers{std::move(identifiers)}
    { }
  };

  struct prefix {
    std::unique_ptr<import_specifier> from;
    std::string                       prefix_;

    prefix() = default;

    prefix(std::unique_ptr<import_specifier> from, std::string prefix)
      : from{std::move(from)}
      , prefix_{std::move(prefix)}
    { }
  };

  struct rename {
    std::unique_ptr<import_specifier> from;
    std::vector<std::tuple<std::string, std::string>> renames;

    rename() = default;

    rename(std::unique_ptr<import_specifier> from, std::vector<std::tuple<std::string, std::string>> renames)
      : from{std::move(from)}
      , renames{std::move(renames)}
    { }
  };

  using value_type = std::variant<
    module_name,
    only,
    except,
    prefix,
    rename
  >;

  value_type value;

  template <typename T>
  explicit
  import_specifier(T value) : value{std::move(value)} { }

  import_specifier(import_specifier const&);
  import_specifier(import_specifier&&) = default;

  import_specifier&
  operator = (import_specifier const&);

  import_specifier&
  operator = (import_specifier&&) = default;
};

// Metainformation about a module -- its name, list of imports and exports, plus
// its body as a list of unparsed data.
struct protomodule {
  std::optional<module_name>       name;
  std::vector<import_specifier>    imports;
  std::vector<std::string>         exports;
  std::vector<tracked_ptr<syntax>> body;
  source_file_origin               origin;

  explicit
  protomodule(source_file_origin origin)
    : origin{std::move(origin)}
  { }

  protomodule(std::optional<module_name> name, std::vector<import_specifier> imports,
              std::vector<std::string> exports, std::vector<tracked_ptr<syntax>> body,
              source_file_origin origin)
    : name{std::move(name)}
    , imports{std::move(imports)}
    , exports{std::move(exports)}
    , body{std::move(body)}
    , origin{std::move(origin)}
  { }
};

} // namespace insider

#endif
