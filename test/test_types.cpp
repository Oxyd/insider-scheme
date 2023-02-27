#include "scheme_fixture.hpp"

#include "runtime/basic_types.hpp"
#include "runtime/symbol.hpp"
#include "util/define_procedure.hpp"

using namespace insider;

struct types : scheme_fixture { };

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
  };
}

TEST_F(types, intern) {
  root_ptr<symbol> a_1{ctx.store, ctx.intern("a")};
  root_ptr<symbol> b_1{ctx.store, ctx.intern("b")};
  root_ptr<symbol> a_2{ctx.store, ctx.intern("a")};

  EXPECT_TRUE(a_1);
  EXPECT_TRUE(b_1);
  EXPECT_TRUE(a_2);

  EXPECT_EQ(a_1.get(), a_2.get());
  EXPECT_NE(a_1.get(), b_1.get());

  b_1.get().reset();
  ctx.store.collect_garbage(true);

  ptr<symbol> b_2 = ctx.intern("b");
  ptr<symbol> b_3 = ctx.intern("b");
  EXPECT_TRUE(b_2);
  EXPECT_TRUE(b_3);
  EXPECT_EQ(b_2, b_3);
}

TEST_F(types, vector) {
  root_ptr<vector> v1 = make_root<vector>(ctx, 3, ctx.constants->void_);
  v1->set(ctx.store, 0, integer_to_ptr(integer{1}));
  v1->set(ctx.store, 1, integer_to_ptr(integer{2}));
  v1->set(ctx.store, 2, integer_to_ptr(integer{3}));

  EXPECT_EQ(v1->size(), 3u);

  EXPECT_EQ(expect<integer>(v1->ref(0)).value(), 1);
  EXPECT_EQ(expect<integer>(v1->ref(1)).value(), 2);
  EXPECT_EQ(expect<integer>(v1->ref(2)).value(), 3);

  EXPECT_THROW(v1->ref(3), std::runtime_error);
  EXPECT_THROW(v1->set(ctx.store, 4, integer_to_ptr(integer{4})), std::runtime_error);

  root_ptr<vector> v2 = make_root<vector>(ctx, 2, ctx.constants->void_);
  bool one{}, two{};
  v2->set(ctx.store, 0, make<aaa>(ctx, &one));
  v2->set(ctx.store, 1, make<aaa>(ctx, &two));

  EXPECT_TRUE(one);
  EXPECT_TRUE(two);

  v2.get().reset();
  ctx.store.collect_garbage(true);

  EXPECT_FALSE(one);
  EXPECT_FALSE(two);
}

static ptr<opaque_value<int>>
make_opaque_seven(context& ctx) {
  return make<opaque_value<int>>(ctx, 7);
}

TEST_F(types, opaque_value) {
  define_procedure<make_opaque_seven>(ctx, "make-value", ctx.internal_module());
  auto result = eval("(make-value)");
  EXPECT_EQ(expect<opaque_value<int>>(result)->value, 7);
}

TEST_F(types, bytevector_has_correct_size_after_creation) {
  ptr<bytevector> bv = make<bytevector>(ctx, 3);
  EXPECT_EQ(bv->size(), 3);
}

TEST_F(types, bytevector_ref_returns_stored_element) {
  ptr<bytevector> bv = make<bytevector>(ctx, 3);
  bv->set(0, 12);
  bv->set(1, 24);
  bv->set(2, 48);

  EXPECT_EQ(bv->ref(0), 12);
  EXPECT_EQ(bv->ref(1), 24);
  EXPECT_EQ(bv->ref(2), 48);
}
