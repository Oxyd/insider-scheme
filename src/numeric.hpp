#ifndef SCHEME_NUMERIC_HPP
#define SCHEME_NUMERIC_HPP

#include "free_store.hpp"

#include <cstdint>
#include <vector>

namespace scm {

class boolean;
class context;
class module;

// A signed, fixed size integer.
class integer : public object {
public:
  using storage_type = std::uint64_t;
  using value_type = std::int64_t;

  integer() = default;
  integer(std::uint64_t value) : value_{value} { }
  integer(std::int64_t value) : value_{static_cast<std::uint64_t>(value)} { }
  integer(int value) : value_{static_cast<std::uint64_t>(value)} { }

  std::int64_t
  value() const { return static_cast<std::int64_t>(value_); }

  std::size_t
  hash() const override { return static_cast<std::size_t>(value_); }

  bool
  eqv(generic_ptr const& other) const override;

private:
  storage_type value_ = 0;
};

ptr<integer>
add(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
add(context&, std::vector<generic_ptr> const&);

ptr<integer>
subtract(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
subtract(context&, std::vector<generic_ptr> const&);

ptr<integer>
multiply(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
multiply(context&, std::vector<generic_ptr> const&);

ptr<integer>
divide(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
divide(context&, std::vector<generic_ptr> const&);

ptr<boolean>
arith_equal(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
arith_equal(context&, std::vector<generic_ptr> const&);

ptr<boolean>
less(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
less(context&, std::vector<generic_ptr> const&);

ptr<boolean>
greater(context&, ptr<integer> const&, ptr<integer> const&);
generic_ptr
greater(context&, std::vector<generic_ptr> const&);

void
export_numeric(context&, module&);

} // namespace scm

#endif
