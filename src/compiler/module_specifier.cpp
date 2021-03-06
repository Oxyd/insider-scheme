#include "compiler/module_specifier.hpp"

namespace insider {

import_specifier::import_specifier(import_specifier const& other) {
  if (auto const* n = std::get_if<module_name>(&other.value))
    value = *n;
  else if (auto const* o = std::get_if<only>(&other.value))
    value = only{std::make_unique<import_specifier>(*o->from), o->identifiers};
  else if (auto const* e = std::get_if<except>(&other.value))
    value = except{std::make_unique<import_specifier>(*e->from), e->identifiers};
  else if (auto const* p = std::get_if<prefix>(&other.value))
    value = prefix{std::make_unique<import_specifier>(*p->from), p->prefix_};
  else if (auto const* r = std::get_if<rename>(&other.value))
    value = rename{std::make_unique<import_specifier>(*r->from), r->renames};
}

import_specifier&
import_specifier::operator = (import_specifier const& other) {
  if (this != &other) {
    this->~import_specifier();
    new (this) import_specifier(other);
  }

  return *this;
}

} // namespace insider
