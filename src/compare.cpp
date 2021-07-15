#include "compare.hpp"

#include "basic_types.hpp"
#include "context.hpp"
#include "numeric.hpp"
#include "object_conversions.hpp"

#include <algorithm>
#include <unordered_map>
#include <vector>

namespace insider {

std::size_t
hash(ptr<> x) {
  if (auto i = match<integer>(x))
    return integer_hash(*i);
  else
    return object_hash(x);
}

std::size_t
hasheqv(ptr<> x) {
  if (auto i = match<integer>(x))
    return integer_hash(*i);
  else if (auto bi = match<big_integer>(x))
    return bi->hash();
  else if (auto f = match<fraction>(x))
    return f->hash();
  else if (auto fp = match<floating_point>(x))
    return fp->hash();
  else if (auto c = match<character>(x))
    return c->hash();
  else if (auto s = match<string>(x))
    return s->hash();
  else
    return object_hash(x);
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

  if (is<string>(x) && is<string>(y))
    return assume<string>(x)->value() == assume<string>(y)->value();

  return false;
}

namespace {
  class disjoint_set {
  public:
    // Returns true when the two sets were disjoint before this operation.
    bool
    merge(ptr<> x, ptr<> y) {
      std::size_t x_root = find_root(x);
      std::size_t y_root = find_root(y);

      if (x_root == y_root)
        return false;
      else {
        merge(x_root, y_root);
        return true;
      }
    }

  private:
    struct node {
      ptr<>       value;
      std::size_t index;
      std::size_t parent_index;
      std::size_t size = 1;
    };

    static constexpr std::size_t no_parent = static_cast<std::size_t>(-1);
    std::vector<node> nodes_;
    std::unordered_map<ptr<>, std::size_t> value_map_;

    std::size_t
    get_node_index(ptr<> value) {
      if (auto it = value_map_.find(value); it != value_map_.end())
        return it->second;
      else {
        nodes_.push_back({value, nodes_.size(), no_parent});
        value_map_.emplace(value, nodes_.back().index);
        return nodes_.back().index;
      }
    }

    std::size_t
    find_root(std::size_t node_index) {
      std::size_t root_index = find_root_index(node_index);
      compress_path(node_index, root_index);
      return root_index;
    }

    std::size_t
    find_root(ptr<> value) {
      return find_root(get_node_index(value));
    }

    std::size_t
    find_root_index(std::size_t start_index) {
      std::size_t root_index = start_index;
      while (nodes_[root_index].parent_index != no_parent)
        root_index = nodes_[root_index].parent_index;

      return root_index;
    }

    void
    compress_path(std::size_t start_index, std::size_t root_index) {
      while (start_index != root_index) {
        node& n = nodes_[start_index];
        std::size_t parent = n.parent_index;
        n.parent_index = root_index;
        start_index = parent;
      }
    }

    void
    merge(std::size_t x_index, std::size_t y_index) {
      node& x = nodes_[x_index];
      node& y = nodes_[y_index];

      assert(x.parent_index == no_parent);
      assert(y.parent_index == no_parent);

      if (x.size > y.size) {
        y.parent_index = x.index;
        x.size += y.size;
      } else {
        x.parent_index = y.index;
        y.size += x.size;
      }
    }
  };
}

bool
equal(context& ctx, ptr<> x, ptr<> y) {
  struct record {
    ptr<> left;
    ptr<> right;
  };
  std::vector<record> stack{{x, y}};
  disjoint_set assumed_equivalence_classes;

  auto check_pair_members = [&] (ptr<pair> l, ptr<pair> r) {
    stack.push_back({cdr(l), cdr(r)});
    stack.push_back({car(l), car(r)});
    return true;
  };

  auto check_vector_members = [&] (ptr<vector> l, ptr<vector> r) {
    if (l->size() != r->size())
      return false;

    for (std::size_t i = 0; i < l->size(); ++i)
      stack.push_back({l->ref(i), r->ref(i)});

    return true;
  };

  auto check_members = [&] (record current) {
    if (eqv(ctx, current.left, current.right))
      return true;

    bool made_new_assumption = assumed_equivalence_classes.merge(current.left, current.right);
    if (!made_new_assumption)
      return true;

    if (is<pair>(current.left) && is<pair>(current.right))
      return check_pair_members(assume<pair>(current.left),
                                assume<pair>(current.right));

    if (is<vector>(current.left) && is<vector>(current.right))
      return check_vector_members(assume<vector>(current.left),
                                  assume<vector>(current.right));

    return false;
  };

  while (!stack.empty()) {
    record current = stack.back();
    stack.pop_back();

    bool possibly_equal = check_members(current);
    if (!possibly_equal)
      return false;
  }

  return true;
}

} // namespace insider
