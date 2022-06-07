#include "util/depth_first_search.hpp"

#include <gtest/gtest.h>

#include <memory>
#include <vector>

using namespace insider;

struct depth_first_search_fixture : testing::Test {
  enum class color {
    white, grey, black
  };

  struct node {
    int value;
    enum color color = color::white;
    std::vector<std::unique_ptr<node>> children;

    node(int v, auto&&... c)
      : value{v}
    {
      (children.emplace_back(std::move(c)), ...);
    }
  };

  std::unique_ptr<node>
  make(int value, auto&&... children) {
    return std::make_unique<node>(value, std::move(children)...);
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