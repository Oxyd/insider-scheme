#ifndef SCHEME_ANALYSER_HPP
#define SCHEME_ANALYSER_HPP

#include "scheme.hpp"

#include <memory>
#include <variant>

namespace game::scm {

// The analyser expects a Scheme datum (or a list of data) that represents a
// program, and turns it into an internal representation, defined here.

// Names are translated to variable objects during analysis so that if two parts
// of the code refer to the same variable, their syntaxes contain pointers to
// the same variable object.
struct variable {
  std::string name;
  bool is_set = false;

  explicit
  variable(std::string n) : name{std::move(n)} { }
};

struct syntax;

struct literal_syntax {
  generic_ptr value;
};

struct local_reference_syntax {
  std::shared_ptr<scm::variable> variable;
};

struct top_level_reference_syntax {
  operand location;
  std::string name;
};

struct application_syntax {
  std::unique_ptr<syntax> target;
  std::vector<std::unique_ptr<syntax>> arguments;
};

struct body_syntax {
  std::vector<std::unique_ptr<syntax>> expressions;
};

struct definition_pair_syntax {
  std::shared_ptr<scm::variable> variable;
  std::unique_ptr<syntax> expression;
};

struct let_syntax {
  std::vector<definition_pair_syntax> definitions;
  body_syntax body;
};

struct set_syntax {
  std::shared_ptr<variable> target;
  std::unique_ptr<syntax> expression;
};

struct lambda_syntax {
  std::vector<std::shared_ptr<variable>> parameters;
  body_syntax body;
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

struct syntax {
  using value_type = std::variant<
    literal_syntax,
    local_reference_syntax,
    top_level_reference_syntax,
    application_syntax,
    let_syntax,
    set_syntax,
    lambda_syntax,
    if_syntax,
    box_syntax,
    unbox_syntax,
    box_set_syntax
  >;

  syntax* parent = nullptr;
  value_type value;

  syntax(syntax* p, value_type value)
    : parent{p}
    , value{std::move(value)}
  { }
};

// Analyse a datum within a given module. The module provides the top-level
// bindings visible to S-expression.
std::unique_ptr<syntax>
analyse(context&, generic_ptr const& datum, ptr<module> const&);

} // namespace game::scm

#endif
