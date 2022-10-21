#ifndef INSIDER_UTIL_DEPTH_FIRST_SEARCH_HPP
#define INSIDER_UTIL_DEPTH_FIRST_SEARCH_HPP

#include <vector>

namespace insider {

namespace detail {
  enum class direction { in, out };
}

template <typename Node>
class dfs_stack {
public:
  struct record {
    Node              node;
    detail::direction dir;
  };

  void
  push_back(Node n) {
    stack_.emplace_back(n, detail::direction::in);
  }

  record&
  back() { return stack_.back(); }

  void
  pop_back() { stack_.pop_back(); }

  bool
  empty() const { return stack_.empty(); }

private:
  std::vector<record> stack_;
};

template <typename Node, typename Visitor>
void
depth_first_search(Node n, Visitor&& v) {
  dfs_stack<Node> stack;
  stack.push_back(n);

  while (!stack.empty()) {
    auto& current = stack.back();
    switch (current.dir) {
    case detail::direction::in:
      current.dir = detail::direction::out;
      v.enter(current.node, stack);
      break;

    case detail::direction::out:
      v.leave(current.node);
      stack.pop_back();
      break;
    }
  }
}

template <typename Visitor, typename Node>
Visitor
depth_first_search(Node n) {
  Visitor v;
  depth_first_search(n, v);
  return v;
}

} // namespace insider

#endif
