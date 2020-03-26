#ifndef SCHEME_ANALYSER_HPP
#define SCHEME_ANALYSER_HPP

#include "scheme.hpp"

#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>

namespace scm {

// The analyser expects a Scheme datum (or a list of data) that represents a
// program, and turns it into an internal representation, defined here.

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
    make_vector_syntax
  >;

  value_type value;

  explicit
  syntax(value_type value)
    : value{std::move(value)}
  { }
};

// Analyse a datum within a given module. The module provides the top-level
// bindings visible to S-expression. The module is modified by adding a new
// top-level binding if the datum is a top-level definition.
std::unique_ptr<syntax>
analyse(context&, generic_ptr const& datum, module&);

// Interpret the given list of data as a main (program) module.
protomodule
read_main_module(std::vector<generic_ptr> const& data);

// Interpret the given list of data as a library.
protomodule
read_library(std::vector<generic_ptr> const& data);

// Analyse a list of data that forms a module body.
body_syntax
analyse_module(context&, module&, std::vector<generic_ptr> const& data);

} // namespace scm

#endif
