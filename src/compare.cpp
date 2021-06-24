#include "compare.hpp"

#include "basic_types.hpp"
#include "context.hpp"
#include "numeric.hpp"
#include "object_conversions.hpp"

namespace insider {

std::size_t
hash(ptr<> x) {
  if (auto i = match<integer>(x))
    return integer_hash(*i);
  else
    return object_type(x).hash(x);
}

bool
eqv(context& ctx, ptr<> x, ptr<> y) {
  if (x == y)
    return true;

  if (!is_object_ptr(x) || !is_object_ptr(y))
    return false; // Either both are fixnums and not the same, or they're different types.

  if (is_number(x) && is_number(y) && is_exact(x) == is_exact(y))
    return arith_equal(ctx, x, y) == ctx.constants->t.get();

  if (object_type_index(x) != object_type_index(y))
    return false;

  if (auto lhs = match<character>(x))
    return lhs->value() == assume<character>(x)->value();

  return false;
}

bool
equal(context& ctx, ptr<> x, ptr<> y) {
  // XXX: This will break on infinite data structures.

  struct record {
    ptr<> left;
    ptr<> right;
  };

  std::vector<record> stack{{x, y}};
  while (!stack.empty()) {
    record top = stack.back();
    stack.pop_back();

    if (!eqv(ctx, top.left, top.right)) {
      if (is<pair>(top.left) && is<pair>(top.right)) {
        auto l = assume<pair>(top.left);
        auto r = assume<pair>(top.right);

        stack.push_back({cdr(l), cdr(r)});
        stack.push_back({car(l), car(r)});
      }
      else if (is<vector>(top.left) && is<vector>(top.right)) {
        auto l = assume<vector>(top.left);
        auto r = assume<vector>(top.right);

        if (l->size() != r->size())
          return false;

        for (std::size_t i = l->size(); i > 0; --i)
          stack.push_back({l->ref(i - 1), r->ref(i - 1)});
      }
      else if (is<string>(top.left) && is<string>(top.right)) {
        if (assume<string>(top.left)->value() != assume<string>(top.right)->value())
          return false;
      }
      else
        return false;
    }
  }

  return true;
}

} // namespace insider
