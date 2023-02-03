#include "scheme_fixture.hpp"

#include "memory/root_provider.hpp"
#include "memory/tracker.hpp"
#include "runtime/string.hpp"
#include "runtime/symbol.hpp"

using namespace insider;

struct gc : scheme_fixture { };

namespace {
  struct aaa : leaf_object<aaa> {
    static constexpr char const* scheme_name = "aaa";

    bool* alive;

    explicit
    aaa(bool* alive) : alive{alive} { if (alive) *alive = true; }

    aaa(aaa&& other) : alive{other.alive} { other.alive = nullptr; }

    ~aaa() {
      if (alive)
        *alive = false;
    }
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

  ccc(bool* alive, ptr<> ref)
    : alive{alive}
    , ref{ref}
  {
    if (alive)
      *alive = true;
  }

  ~ccc() {
    if (alive)
      *alive = false;
  }

  void
  visit_members(member_visitor const& f) const {
    f(ref);
  }
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
  visit_members(member_visitor const& f) const {
    f(child_);
  }

  ptr<bbb>
  child() const { return child_; }

  void
  set_child(free_store& fs, ptr<bbb> new_child) {
    child_ = new_child;
    fs.notify_arc(this, new_child);
  }

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

struct ddd : composite_object<ddd> {
  static constexpr char const* scheme_name = "insider::ddd";

  explicit
  ddd(ptr<aaa> a) : child{a} { }

  ddd(ddd&& other) : child{other.child} { }

  void
  visit_members(member_visitor const& f) const {
    f.weak(child);
  }

  ptr<aaa> child;
};

TEST_F(gc, weak_references) {
  bool one{};
  tracked_ptr<ddd> d = make_tracked<ddd>(ctx, make<aaa>(ctx, &one));
  EXPECT_TRUE(one);

  ctx.store.collect_garbage(true);
  EXPECT_FALSE(one);
  EXPECT_EQ(d->child, ptr<>{});
}

TEST_F(gc, weak_references_across_generations) {
  tracked_ptr<ddd> d = make_tracked<ddd>(ctx, ptr<aaa>{});
  ctx.store.collect_garbage(true);
  ctx.store.collect_garbage(true);
  EXPECT_EQ(object_generation(d.get()), generation::mature);

  bool one{};
  d->child = make<aaa>(ctx, &one);
  ctx.store.notify_arc(d.get(), d->child);
  EXPECT_TRUE(one);

  ctx.store.collect_garbage(false);
  EXPECT_FALSE(one);
  EXPECT_EQ(d->child, ptr<>{});
}

namespace {
  struct simple_provider : root_provider {
    ptr<aaa> a;

    simple_provider(free_store& fs, ptr<aaa> a)
      : root_provider{fs}
      , a{a}
    { }

    void
    visit_roots(member_visitor const& f) override {
      f(a);
    }
  };
} // anonymous namespace

struct root_provider_fixture : scheme_fixture {
  bool one{};
  ptr<aaa> a = make<aaa>(ctx, &one);

  bool two{};
  ptr<aaa> b = make<aaa>(ctx, &two);

  root_provider_fixture() {
    EXPECT_TRUE(one);
    EXPECT_TRUE(two);
  }
};

TEST_F(root_provider_fixture, provides_roots) {
  {
    simple_provider p{ctx.store, a};
    ctx.store.collect_garbage(true);
    EXPECT_TRUE(one);
    EXPECT_FALSE(two);
  }

  ctx.store.collect_garbage(true);
  EXPECT_FALSE(one);
}

TEST_F(root_provider_fixture, copy) {
  {
    simple_provider p{ctx.store, a};
    {
      simple_provider copy{p};
      ctx.store.collect_garbage(true);
      EXPECT_TRUE(one);
    }
    ctx.store.collect_garbage(true);
    EXPECT_TRUE(one);
  }

  ctx.store.collect_garbage(true);
  EXPECT_FALSE(one);
}

TEST_F(root_provider_fixture, copy_assign) {
  simple_provider p{ctx.store, a};
  {
    simple_provider q{ctx.store, b};
    ctx.store.collect_garbage(true);
    EXPECT_TRUE(one);
    EXPECT_TRUE(two);

    p = q;

    // Now both provide b, so no one provides a.
    ctx.store.collect_garbage(true);
    EXPECT_FALSE(one);
    EXPECT_TRUE(two);
  }

  ctx.store.collect_garbage(true);
  EXPECT_TRUE(two);
}

TEST_F(root_provider_fixture, move) {
  simple_provider p{ctx.store, a};
  {
    simple_provider q = std::move(p);
    ctx.store.collect_garbage(true);
    EXPECT_TRUE(one);
    EXPECT_FALSE(two);
  }

  ctx.store.collect_garbage(true);
  EXPECT_FALSE(one);
}

TEST_F(root_provider_fixture, move_assign) {
  simple_provider p{ctx.store, a};
  {
    simple_provider q{ctx.store, b};
    ctx.store.collect_garbage(true);
    EXPECT_TRUE(one);
    EXPECT_TRUE(two);

    p = std::move(q);

    // Now both provide b, so no one provides a.
    ctx.store.collect_garbage(true);
    EXPECT_FALSE(one);
    EXPECT_TRUE(two);
  }

  ctx.store.collect_garbage(true);
  EXPECT_TRUE(two);
}

TEST_F(gc, distinct_objects_have_distinct_hashes) {
  auto a = make<pair>(ctx, ctx.constants->null, ctx.constants->null);
  auto b = make<pair>(ctx, ctx.constants->null, ctx.constants->null);
  EXPECT_NE(object_hash(a), object_hash(b));
}

TEST_F(gc, objects_retain_hash_values_through_gc) {
  auto a = make_tracked<symbol>(ctx, "a");
  auto b = make_tracked<symbol>(ctx, "b");

  void* old_a_addr = a.get().value();
  void* old_b_addr = b.get().value();
  word_type old_a_hash = object_hash(a.get());
  word_type old_b_hash = object_hash(b.get());

  ctx.store.collect_garbage(true);

  void* new_a_addr = a.get().value();
  void* new_b_addr = b.get().value();

  EXPECT_NE(old_a_addr, new_a_addr);
  EXPECT_NE(old_b_addr, new_b_addr);
  EXPECT_EQ(object_hash(a.get()), old_a_hash);
  EXPECT_EQ(object_hash(b.get()), old_b_hash);
}
