#ifndef SCHEME_SYNTAX_HPP
#define SCHEME_SYNTAX_HPP

#include "bytecode.hpp"
#include "free_store.hpp"

#include <memory>
#include <string>
#include <variant>
#include <vector>

namespace insider {

class transformer;

// The binding between a name and its value. For top-level values, this directly
// contains the index of the value. For non-top-level syntax transformers, it
// directly contains the transformer. Otherwise, it's just an object
// representing the binding itself and the compiler will use these to translate
// them to local registers.
struct variable {
  std::string                                 name;
  bool                                        is_set = false;
  std::optional<operand::representation_type> global;

  explicit
  variable(std::string n) : name{std::move(n)} { }

  variable(std::string n, operand::representation_type index)
    : name{std::move(n)}
    , global{index}
  { }
};

struct syntax;

struct literal_syntax {
  generic_ptr value;
};

struct local_reference_syntax {
  std::shared_ptr<insider::variable> variable;
};

struct top_level_reference_syntax {
  operand location;
  std::string name;
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
};

struct definition_pair_syntax {
  std::shared_ptr<insider::variable> variable;
  std::unique_ptr<syntax> expression;
};

struct let_syntax {
  std::vector<definition_pair_syntax> definitions;
  sequence_syntax body;
};

struct local_set_syntax {
  std::shared_ptr<variable> target;
  std::unique_ptr<syntax> expression;
};

struct top_level_set_syntax {
  operand location;
  std::unique_ptr<syntax> expression;
};

struct lambda_syntax {
  std::vector<std::shared_ptr<variable>> parameters;
  sequence_syntax body;
};

struct if_syntax {
  std::unique_ptr<syntax> test;
  std::unique_ptr<syntax> consequent;
  std::unique_ptr<syntax> alternative;
};

struct box_syntax {
  std::unique_ptr<syntax> expression;
};

struct unbox_syntax {
  std::unique_ptr<syntax> box_expr;
};

struct box_set_syntax {
  std::unique_ptr<syntax> box_expr;
  std::unique_ptr<syntax> value_expr;
};

struct cons_syntax {
  std::unique_ptr<syntax> car;
  std::unique_ptr<syntax> cdr;
};

struct make_vector_syntax {
  std::vector<std::unique_ptr<syntax>> elements;
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
  };

  struct except {
    std::unique_ptr<import_specifier> from;
    std::vector<std::string>          identifiers;
  };

  struct prefix {
    std::unique_ptr<import_specifier> from;
    std::string                       prefix;
  };

  struct rename {
    std::unique_ptr<import_specifier> from;
    std::vector<std::tuple<std::string, std::string>> renames;
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
};

// Metainformation about a module -- its name, list of imports and exports, plus
// its body as a list of unparsed data.
struct protomodule {
  std::optional<module_name>    name;
  std::vector<import_specifier> imports;
  std::vector<std::string>      exports;
  std::vector<generic_ptr>      body;
};

} // namespace insider

#endif
