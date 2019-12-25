#ifndef SCHEME_SYNTAX_HPP
#define SCHEME_SYNTAX_HPP

#include "scheme.hpp"

#include <memory>
#include <variant>

namespace game::scm {

// The parser expects a Scheme datum (or a list of data) that represents a
// program, and turns it into an internal representation, defined here.

struct syntax;

struct literal_syntax {
  generic_ptr value;
};

struct reference_syntax {
  ptr<scm::symbol> symbol;
};

struct application_syntax {
  std::unique_ptr<syntax> target;
  std::vector<std::unique_ptr<syntax>> arguments;
};

struct body_syntax {
  std::vector<std::unique_ptr<syntax>> expressions;
};

struct definition_pair_syntax {
  ptr<symbol> name;
  std::unique_ptr<syntax> expression;
};

struct let_syntax {
  std::vector<definition_pair_syntax> definitions;
  body_syntax body;
};

struct letrec_syntax {
  std::vector<definition_pair_syntax> definitions;
  body_syntax body;
};

struct lambda_syntax {
  std::vector<ptr<symbol>> parameters;
  body_syntax body;
};

struct if_syntax {
  std::unique_ptr<syntax> test;
  std::unique_ptr<syntax> consequent;
  std::unique_ptr<syntax> alternative;
};

struct syntax {
  using value_type = std::variant<
    literal_syntax,
    reference_syntax,
    application_syntax,
    let_syntax,
    letrec_syntax,
    lambda_syntax,
    if_syntax
  >;

  value_type value;
};

std::unique_ptr<syntax>
parse(context&, generic_ptr const& datum);

} // namespace game::scm

#endif
