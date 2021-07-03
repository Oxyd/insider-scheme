#include "scheme_fixture.hpp"

using namespace insider;

struct gc : scheme_fixture { };

namespace {
  struct aaa : leaf_object<aaa> {
    static constexpr char const* scheme_name = "aaa";

    bool* alive;

    explicit
    aaa(bool* alive) : alive{alive} { *alive = true; }

    aaa(aaa&& other) : alive{other.alive} { other.alive = nullptr; }

    ~aaa() {
      if (alive)
        *alive = false;
    }

    std::size_t
    hash() const { return 0; }
  };
}

TEST_F(gc, collect_direct_garbage) {
  bool one{}, two{}, three{};
  tracked_ptr<aaa> a = make_tracked<aaa>(ctx, &one);
  tracked_ptr<aaa> b = make_tracked<aaa>(ctx, &two);
  tracked_ptr<aaa> c = make_tracked<aaa>(ctx, &three);

  ctx.store.collect_garbage(true);

  EXPECT_TRUE(one);
  EXPECT_TRUE(two);
  EXPECT_TRUE(three);

  a.reset();
  ctx.store.collect_garbage(true);

  EXPECT_FALSE(one);
  EXPECT_TRUE(two);
  EXPECT_TRUE(three);

  b.reset();
  c.reset();
  ctx.store.collect_garbage(true);

  EXPECT_FALSE(one);
  EXPECT_FALSE(two);
  EXPECT_FALSE(three);
}

struct ccc : composite_object<ccc> {
  static constexpr char const* scheme_name = "ccc";

  bool* alive;
  ptr<> ref;

  ccc(bool* alive, object* ref)
    : alive{alive}
    , ref{ref}
  {
    *alive = true;
  }

  ~ccc() {
    *alive = false;
  }

  void
  visit_members(member_visitor const& f) {
    f(ref);
  }

  std::size_t
  hash() const { return 0; }
};

struct bbb : composite_object<bbb> {
  static constexpr char const* scheme_name = "bbb";

  bool* alive;

  explicit
  bbb(bool* alive) : alive{alive} { *alive = true; }

  bbb(bbb&& other)
    : alive{other.alive}
    , child_{other.child_}
  {
    other.alive = nullptr;
  }

  ~bbb() {
    if (alive)
      *alive = false;
  }

  void
  visit_members(member_visitor const& f) {
    f(child_);
  }

  ptr<bbb>
  child() const { return child_; }

  void
  set_child(free_store& fs, ptr<bbb> new_child) {
    child_ = new_child;
    fs.notify_arc(this, new_child);
  }

  std::size_t
  hash() const { return 0; }

private:
  ptr<bbb> child_ = nullptr;
};

TEST_F(gc, collect_indirect_garbage) {
  bool one{}, two{}, three{}, four{};
  tracked_ptr<bbb> root = make_tracked<bbb>(ctx, &one);
  root->set_child(ctx.store, make<bbb>(ctx, &two));
  root->child()->set_child(ctx.store, make<bbb>(ctx, &three));
  root->child()->child()->set_child(ctx.store, make<bbb>(ctx, &four));

  ctx.store.collect_garbage(true);
  EXPECT_TRUE(one);
  EXPECT_TRUE(two);
  EXPECT_TRUE(three);
  EXPECT_TRUE(four);

  root->child()->set_child(ctx.store, nullptr);
  ctx.store.collect_garbage(true);
  EXPECT_TRUE(one);
  EXPECT_TRUE(two);
  EXPECT_FALSE(three);
  EXPECT_FALSE(four);

  root.reset();
  ctx.store.collect_garbage(true);
  EXPECT_FALSE(one);
  EXPECT_FALSE(two);
  EXPECT_FALSE(three);
  EXPECT_FALSE(four);
}

TEST_F(gc, collect_circles) {
  bool one{}, two{};
  tracked_ptr<bbb> a = make_tracked<bbb>(ctx, &one);
  a->set_child(ctx.store, make<bbb>(ctx, &two));
  a->child()->set_child(ctx.store, a.get());

  ctx.store.collect_garbage(true);
  EXPECT_TRUE(one);
  EXPECT_TRUE(two);

  a.reset();
  ctx.store.collect_garbage(true);
  EXPECT_FALSE(one);
  EXPECT_FALSE(two);
}

TEST_F(gc, weak_ptr) {
  bool one{};
  tracked_ptr<aaa> a = make_tracked<aaa>(ctx, &one);
  weak_ptr<aaa> w = a;

  ctx.store.collect_garbage(true);
  EXPECT_TRUE(one);
  EXPECT_TRUE(w);

  a.reset();
  ctx.store.collect_garbage(true);
  EXPECT_FALSE(one);
  EXPECT_FALSE(w);

  tracked_ptr<aaa> b = w.lock();
  EXPECT_FALSE(b);
}
