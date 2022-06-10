#include "util/depth_first_search.hpp"

#include <gtest/gtest.h>

#include <memory>
#include <vector>
#include <numeric>
#include <ranges>

using namespace insider;

struct depth_first_search_fixture : testing::Test {
  enum class color {
    white, grey, black
  };

  enum class operation { constant, plus, minus, times };

  struct node {
    int value;
    enum color color = color::white;
    operation op = operation::constant;
    std::vector<std::unique_ptr<node>> children;

    node(int v, auto&&... c)
      : value{v}
    {
      (children.emplace_back(std::move(c)), ...);
    }

    node(operation op, auto&&... c)
      : op{op}
    {
      (children.emplace_back(std::move(c)), ...);
    }
  };

  std::unique_ptr<node>
  make(int value, auto&&... children) {
    return std::make_unique<node>(value, std::move(children)...);
  }

  std::unique_ptr<node>
  make(operation op, auto&&... children) {
    return std::make_unique<node>(op, std::move(children)...);
  }
};

TEST_F(depth_first_search_fixture, visitor_is_called_on_root_node) {
  struct visitor : dfs_visitor {
    unsigned count = 0;

    void
    enter(node*, dfs_stack<node*>&) {
      ++count;
    }
  } v;

  auto tree = make(1, make(2), make(3));
  depth_first_search(tree.get(), v);
  EXPECT_EQ(v.count, 1);
}

TEST_F(depth_first_search_fixture, visitor_is_called_on_all_nodes) {
  struct visitor : dfs_visitor {
    unsigned count = 0;

    void
    enter(node* n, dfs_stack<node*>& s) {
      ++count;

      for (auto& c : n->children)
        s.push_back(c.get());
    }
  } v;

  auto tree = make(1, make(2), make(3));
  depth_first_search(tree.get(), v);
  EXPECT_EQ(v.count, 3);
}

TEST_F(depth_first_search_fixture, leave_is_called_when_going_up) {
  struct visitor : dfs_visitor {
    unsigned left_nodes = 0;

    void
    enter(node* n, dfs_stack<node*>& s) {
      n->color = color::grey;

      for (auto& c : n->children)
        s.push_back(c.get());
    }

    void
    leave(node* n) {
      EXPECT_EQ(n->color, color::grey);
      n->color = color::black;
      ++left_nodes;
    }
  } v;

  auto tree = make(1, make(2, make(3)), make(4, make(5), make(6)));
  depth_first_search(tree.get(), v);
  EXPECT_EQ(v.left_nodes, 6);
}

TEST_F(depth_first_search_fixture, search_without_explicit_visitor) {
  struct visitor : dfs_visitor {
    void
    enter(node* n, dfs_stack<node*>& s) {
      n->color = color::black;

      for (auto& c : n->children)
        s.push_back(c.get());
    }
  };

  auto tree = make(1);
  depth_first_search<visitor>(tree.get());
  EXPECT_EQ(tree->color, color::black);
}

TEST_F(depth_first_search_fixture, tagged_nodes) {
  struct visitor : dfs_visitor {
    struct tagged_node {
      node* n;
      std::optional<int> tag;

      tagged_node(node* n) : n{n} { }
    };

    void
    enter(tagged_node& n, dfs_stack<tagged_node>& s) {
      n.tag = n.n->value;
      for (auto& c : n.n->children)
        s.push_back(c.get());
    }

    void
    leave(tagged_node& n) {
      ASSERT_TRUE(n.tag);
      EXPECT_EQ(*n.tag, n.n->value);
    }
  };

  auto tree = make(1, make(2, make(3)), make(4));
  depth_first_search<visitor>(visitor::tagged_node{tree.get()});
}

TEST_F(depth_first_search_fixture, expression_evaluator) {
  struct visitor : dfs_visitor {
    struct tagged_node {
      node*       n;
      std::size_t num_children = 0;
      operation   op;

      tagged_node(node* n) : n{n} { }
    };

    std::vector<int> results;

    void
    enter(tagged_node& n, dfs_stack<tagged_node>& s) {
      n.num_children = n.n->children.size();
      n.op = n.n->op;
      for (auto& c : n.n->children | std::views::reverse)
        s.push_back(c.get());
    }

    void
    leave(tagged_node& n) {
      switch (n.op) {
      case operation::constant:
        results.push_back(n.n->value);
        break;

      case operation::plus: {
        int r = std::accumulate(results.end() - n.num_children, results.end(),
                                0, std::plus<int>{});
        results.resize(results.size() - n.num_children);
        results.push_back(r);
        break;
      }

      case operation::minus: {
        int r = std::accumulate(
          results.end() - n.num_children + 1, results.end(),
          *(results.end() - n.num_children),
          std::minus<int>{}
        );
        results.resize(results.size() - n.num_children);
        results.push_back(r);
        break;
      }

      case operation::times: {
        int r = std::accumulate(results.end() - n.num_children, results.end(),
                                1, std::multiplies<int>{});
        results.resize(results.size() - n.num_children);
        results.push_back(r);
        break;
      }
      }
    }
  };

  auto tree = make(operation::plus,
                   make(2), make(operation::times,
                                 make(3), make(6),
                                 make(operation::minus,
                                      make(9), make(7))));
  auto v = depth_first_search<visitor>(visitor::tagged_node{tree.get()});
  ASSERT_EQ(v.results.size(), 1);
  EXPECT_EQ(v.results.front(), 2 + 3 * 6 * (9 - 7));
}