#ifndef INSIDER_SYNTAX_HPP
#define INSIDER_SYNTAX_HPP

#include "bytecode.hpp"
#include "free_store.hpp"

#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace insider {

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

struct syntax;

struct literal_syntax {
  generic_ptr value;

  explicit
  literal_syntax(generic_ptr const& value) : value{value} { }
};

struct local_reference_syntax {
  std::shared_ptr<insider::variable> variable;

  explicit
  local_reference_syntax(std::shared_ptr<insider::variable> var)
    : variable{std::move(var)}
  { }
};

struct top_level_reference_syntax {
  operand location;
  std::string name;

  top_level_reference_syntax(operand location, std::string name)
    : location{location}
    , name{std::move(name)}
  { }
};

struct application_syntax {
  std::unique_ptr<syntax> target;
  std::vector<std::unique_ptr<syntax>> arguments;

  application_syntax(std::unique_ptr<syntax> t,
                     std::vector<std::unique_ptr<syntax>> args)
    : target{std::move(t)}
    , arguments{std::move(args)}
  { }

  template <typename... Ts>
  application_syntax(std::unique_ptr<syntax> t, Ts&&... ts)
    : target{std::move(t)}
  {
    arguments.reserve(sizeof...(Ts));
    (arguments.push_back(std::move(ts)), ...);
  }
};

struct sequence_syntax {
  std::vector<std::unique_ptr<syntax>> expressions;

  sequence_syntax() = default;

  explicit
  sequence_syntax(std::vector<std::unique_ptr<syntax>> exprs)
    : expressions{std::move(exprs)}
  { }
};

struct definition_pair_syntax {
  generic_ptr                        id;
  std::shared_ptr<insider::variable> variable;
  std::unique_ptr<syntax>            expression;

  definition_pair_syntax(generic_ptr id, std::shared_ptr<insider::variable> var,
                         std::unique_ptr<syntax> expr)
    : id{id}
    , variable{std::move(var)}
    , expression{std::move(expr)}
  { }
};

struct let_syntax {
  std::vector<definition_pair_syntax> definitions;
  sequence_syntax body;

  let_syntax(std::vector<definition_pair_syntax> defs, sequence_syntax body)
    : definitions{std::move(defs)}
    , body{std::move(body)}
  { }
};

struct local_set_syntax {
  std::shared_ptr<variable> target;
  std::unique_ptr<syntax> expression;

  local_set_syntax(std::shared_ptr<variable> target, std::unique_ptr<syntax> expr)
    : target{std::move(target)}
    , expression{std::move(expr)}
  { }
};

struct top_level_set_syntax {
  operand location;
  std::unique_ptr<syntax> expression;

  top_level_set_syntax(operand location, std::unique_ptr<syntax> expr)
    : location{location}
    , expression{std::move(expr)}
  { }
};

struct lambda_syntax {
  std::vector<std::shared_ptr<variable>> parameters;
  bool has_rest;
  sequence_syntax body;
  std::optional<std::string> name;
  std::vector<std::shared_ptr<variable>> free_variables;

  lambda_syntax(std::vector<std::shared_ptr<variable>> parameters,
                bool has_rest,
                sequence_syntax body,
                std::optional<std::string> name,
                std::vector<std::shared_ptr<variable>> free_variables)
    : parameters{std::move(parameters)}
    , has_rest{has_rest}
    , body{std::move(body)}
    , name{std::move(name)}
    , free_variables{std::move(free_variables)}
  { }
};

struct if_syntax {
  std::unique_ptr<syntax> test;
  std::unique_ptr<syntax> consequent;
  std::unique_ptr<syntax> alternative;

  if_syntax(std::unique_ptr<syntax> test, std::unique_ptr<syntax> consequent, std::unique_ptr<syntax> alternative)
    : test{std::move(test)}
    , consequent{std::move(consequent)}
    , alternative{std::move(alternative)}
  { }
};

struct box_syntax {
  std::unique_ptr<syntax> expression;

  box_syntax() = default;

  explicit
  box_syntax(std::unique_ptr<syntax> expression)
    : expression{std::move(expression)}
  { }
};

struct unbox_syntax {
  std::unique_ptr<syntax> box_expr;

  explicit
  unbox_syntax(std::unique_ptr<syntax> box_expr)
    : box_expr{std::move(box_expr)}
  { }
};

struct box_set_syntax {
  std::unique_ptr<syntax> box_expr;
  std::unique_ptr<syntax> value_expr;

  box_set_syntax(std::unique_ptr<syntax> box_expr, std::unique_ptr<syntax> value_expr)
    : box_expr{std::move(box_expr)}
    , value_expr{std::move(value_expr)}
  { }
};

struct cons_syntax {
  std::unique_ptr<syntax> car;
  std::unique_ptr<syntax> cdr;

  cons_syntax(std::unique_ptr<syntax> car, std::unique_ptr<syntax> cdr)
    : car{std::move(car)}
    , cdr{std::move(cdr)}
  { }
};

struct make_vector_syntax {
  std::vector<std::unique_ptr<syntax>> elements;

  explicit
  make_vector_syntax(std::vector<std::unique_ptr<syntax>> elements)
    : elements{std::move(elements)}
  { }
};

struct syntax {
  using value_type = std::variant<
    literal_syntax,
    local_reference_syntax,
    top_level_reference_syntax,
    application_syntax,
    let_syntax,
    local_set_syntax,
    top_level_set_syntax,
    lambda_syntax,
    if_syntax,
    box_syntax,
    unbox_syntax,
    box_set_syntax,
    cons_syntax,
    make_vector_syntax,
    sequence_syntax
  >;

  value_type value;

  explicit
  syntax(value_type value)
    : value{std::move(value)}
  { }
};

using module_name = std::vector<std::string>;

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
  std::optional<module_name>    name;
  std::vector<import_specifier> imports;
  std::vector<std::string>      exports;
  std::vector<generic_ptr>      body;

  protomodule() = default;

  protomodule(std::optional<module_name> name, std::vector<import_specifier> imports,
              std::vector<std::string> exports, std::vector<generic_ptr> body)
    : name{std::move(name)}
    , imports{std::move(imports)}
    , exports{std::move(exports)}
    , body{std::move(body)}
  { }
};

} // namespace insider

#endif
