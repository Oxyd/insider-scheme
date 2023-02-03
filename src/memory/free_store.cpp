#include "free_store.hpp"
#include "memory/member_visitor.hpp"
#include "memory/root_list.hpp"
#include "object.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <cassert>

#undef small

namespace insider {

enum class color : word_type {
  white = 0,
  grey = 1,
  black = 2,
};

static color
object_color(word_type header) {
  return static_cast<color>((header & color_bits) >> color_shift);
}

static color
object_color(ptr<> o) { return object_color(o.header()->flags); }

static color
object_color(object_header* h) {
  return object_color(h->flags);
}

static void
set_object_color(word_type& header, color c) {
  header = (header & ~color_bits) | (static_cast<word_type>(c) << color_shift);
}

static void
set_object_color(object_header* h, color c) {
  set_object_color(h->flags, c);
}

static void
set_object_color(ptr<> o, color c) {
  o.header()->flags = (o.header()->flags & ~color_bits)
                      | (static_cast<word_type>(c) << color_shift);
}

static void
deallocate(object_header* o) {
  object_type(o).destroy(o);
  delete [] reinterpret_cast<std::byte*>(o);
}

free_store::~free_store() {
  for (object_header* o : all_objects_)
    deallocate(o);
}

namespace {
  template <typename F>
  struct visitor : member_visitor {
    F& f;

    visitor(F& f) : f{f} { }

    void
    operator () (ptr<> const& ptr) const override {
      f(ptr, false);
    }

    void
    weak(ptr<> const& ptr) const override {
      f(ptr, true);
    }
  };
}

template <typename F>
static void
visit_members(object_header* o, F&& f) {
  object_type(o).visit_members(o, visitor<F>{f});
}

template <typename F>
static void
visit_roots(root_list const& roots, F&& f) {
  roots.visit_roots(visitor<F>{f});
}

void
detail::update_ptr(ptr<> const& p, ptr<> new_value) {
  p.value_ = new_value.value_;
}

static void
verify([[maybe_unused]] auto const& g) {
#ifndef NDEBUG
  g.small.for_all([&] (object_header* o) {
    assert(object_generation(o) == g.generation_number);
  });

  for (std::size_t i = 0; i < g.large.object_count(); ++i)
    assert(
      object_generation(
        reinterpret_cast<object_header*>(g.large.get(i))
      ) == g.generation_number
    );
#endif
}

static void
mark(root_list const& roots) {
  object_list work_list;

  auto visit = [&] (ptr<> object, bool) {
    if (object && is_object_ptr(object)
        && object_color(object) == color::white) {
      work_list.push_back(object.header());
      set_object_color(object, color::grey);
    }
  };

  visit_roots(roots, visit);

  while (!work_list.empty()) {
    object_header* current = work_list.back();
    work_list.pop_back();

    visit_members(current, visit);
    set_object_color(current, color::black);
  }
}

static void
sweep(object_list& objects) {
  for (auto o = objects.begin(); o != objects.end(); ) {
    if (object_color(*o) == color::white) {
      deallocate(*o);
      o = objects.erase(o);
    } else {
      assert(object_color(*o) == color::black); // No gray objects here
      set_object_color(*o, color::white);
      ++o;
    }
  }
}

void
free_store::collect_garbage(bool) {
  mark(roots_);
  sweep(all_objects_);
}

} // namespace insider
