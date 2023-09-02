#include "free_store.hpp"
#include "memory/allocator.hpp"
#include "object.hpp"

#include <fmt/format.h>

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <iterator>

#undef small

namespace insider {

static color
object_color(object_header const* h) {
  return header_color(*h);
}

static color
object_color(ptr<> o) { return object_color(o.header()); }

static void
set_object_color(object_header* h, color c) {
  set_header_color(*h, c);
}

static void
set_object_color(ptr<> o, color c) {
  set_object_color(o.header(), c);
}

static word_type
object_age(object_header const* h) { return header_age(*h); }

static void
set_object_age(object_header* h, word_type a) { set_header_age(*h, a); }

static void
increment_object_age(object_header* h) {
  assert(object_age(h) < mature_age);
  set_object_age(h, object_age(h) + 1);
}

[[maybe_unused]]
static bool
object_remembered(object_header const* h) { return header_remembered(*h); }

static void
mark_remembered(object_header* h) { set_header_remembered(*h, true); }

static void
unmark_remembered(object_header* h) { set_header_remembered(*h, false); }

static void
set_remembered(object_header* h, bool remembered) {
  set_header_remembered(*h, remembered);
}

static void
deallocate(object_header* o, allocator& a) {
  object_type(o).destroy(o);
  a.deallocate(reinterpret_cast<std::byte*>(o), storage_size(o));
}

void
weak_box::set(free_store& store, ptr<> value) {
  value_ = value;
  store.notify_arc(this, value);
}

free_store::~free_store() {
  for (object_header* o : mature_)
    deallocate(o, alloc_);
  for (object_header* o : nursery_)
    deallocate(o, alloc_);
  for (ptr<weak_box> b : weak_boxes_)
    deallocate(b.header(), alloc_);
}

template <typename F>
static void
visit_members(object_header const* o, F&& f) {
  object_type(o).visit_members(o, f);
}

template <typename F>
static void
visit_roots(root_list const& roots, F&& f) {
  roots.visit_roots(f);
}

namespace {
  struct work_list {
    object_list objects;
    word_type   max_age;

    explicit
    work_list(word_type max_age)
      : max_age{max_age}
    { }

    void
    visit(ptr<> object) {
      if (object && is_object_ptr(object)
          && object_color(object) == color::white
          && object_age(object) <= max_age) {
        objects.push_back(object.header());
        set_object_color(object, color::grey);
      }
    }

    object_header*
    pop() {
      object_header* current = objects.back();
      objects.pop_back();
      return current;
    }

    bool
    empty() const { return objects.empty(); }
  };
}

static void
mark_work_list(work_list& list) {
  while (!list.empty()) {
    object_header* current = list.pop();
    assert(object_color(current) != color::white);

    if (object_color(current) == color::grey) {
      visit_members(current, [&] (ptr<> object) { list.visit(object); });
      set_object_color(current, color::black);
    }
  }
}

static void
mark_roots(root_list const& roots, work_list& list) {
  visit_roots(roots, [&] (ptr<> object) { list.visit(object); });
}

static void
mark_remembered_set(object_list const& remembered, work_list& list) {
  for (object_header* r : remembered)
    visit_members(r, [&] (ptr<> object) { list.visit(object); });
}

static void
mark_all(root_list const& roots) {
  work_list list{mature_age};
  mark_roots(roots, list);
  mark_work_list(list);
}

static void
mark_nursery(root_list const& roots, object_list const& remembered) {
  work_list list{mature_age - 1};
  mark_roots(roots, list);
  mark_remembered_set(remembered, list);
  mark_work_list(list);
}

namespace {
  struct sweep_result {
    std::size_t total_deallocated_size;
    std::size_t nursery_deallocated_size;
  };
}

static sweep_result
sweep_weak_boxes(std::vector<ptr<weak_box>>& boxes, word_type max_age,
                 allocator& alloc) {
  std::size_t deallocated_size = 0;
  std::size_t nursery_deallocated_size = 0;
  std::vector<ptr<weak_box>> survivors;

  for (ptr<weak_box> b : boxes) {
    if (object_age(b) <= max_age && object_color(b) == color::white) {
      std::size_t size = storage_size(b.header());
      deallocated_size += size;
      if (object_age(b) < mature_age)
        nursery_deallocated_size += size;

      deallocate(b.header(), alloc);
    } else {
      if (b->get()
          && object_age(b->get()) <= max_age
          && object_color(b->get()) == color::white)
        b->reset();

      set_object_color(b, color::white);
      survivors.push_back(b);
    }
  }

  boxes = std::move(survivors);
  return {deallocated_size, nursery_deallocated_size};
}

static sweep_result
sweep_all_weak_boxes(std::vector<ptr<weak_box>>& boxes, allocator& alloc) {
  return sweep_weak_boxes(boxes, mature_age, alloc);
}

static std::size_t
sweep_nursery_weak_boxes(std::vector<ptr<weak_box>>& boxes, allocator& alloc) {
  return sweep_weak_boxes(boxes, mature_age - 1, alloc).total_deallocated_size;
}

static bool
has_arcs_to_nursery(object_header const* o) {
  bool result = false;
  visit_members(o, [&] (ptr<> x) {
    if (x && is_object_ptr(x) && object_age(x) < mature_age)
      result = true;
  });
  return result;
}

static std::size_t
sweep_mature(object_list& mature, object_list& remembered, allocator& alloc) {
  std::size_t deallocated_size = 0;
  object_list survivors;

  remembered.clear();

  for (object_header* o : mature)
    if (object_color(o) == color::white) {
      deallocated_size += storage_size(o);
      deallocate(o, alloc);
    } else {
      assert(object_color(o) == color::black); // No gray objects here
      set_object_color(o, color::white);
      survivors.push_back(o);

      bool remember = has_arcs_to_nursery(o);
      set_remembered(o, remember);
      if (remember)
        remembered.push_back(o);
    }

  mature = std::move(survivors);
  return deallocated_size;
}

static std::size_t
sweep_nursery(object_list& nursery, allocator& alloc) {
  std::size_t deallocated_size = 0;
  object_list survivors;

  for (object_header* o : nursery)
    if (object_color(o) == color::white) {
      deallocated_size += storage_size(o);
      deallocate(o, alloc);
    } else {
      assert(object_color(o) == color::black); // No gray objects here
      set_object_color(o, color::white);
      survivors.push_back(o);
    }

  nursery = std::move(survivors);
  return deallocated_size;
}

static sweep_result
sweep_all(object_list& mature, object_list& nursery, object_list& remembered,
          allocator& alloc) {
  std::size_t mature_deallocated = sweep_mature(mature, remembered, alloc);
  std::size_t nursery_deallocated = sweep_nursery(nursery, alloc);
  return {mature_deallocated + nursery_deallocated,
          nursery_deallocated};
}

static void
filter_remembered_set(object_list& remembered) {
  object_list new_remembered;

  for (object_header* o : remembered)
    if (has_arcs_to_nursery(o))
      new_remembered.push_back(o);
    else
      unmark_remembered(o);

  remembered = std::move(new_remembered);
}

static void
promote_objects(object_list const& to_promote, object_list& mature,
                object_list& remembered) {
  for (object_header* o : to_promote) {
    if (header_needs_scan_on_promote(*o) && has_arcs_to_nursery(o)) {
      assert(!object_remembered(o));
      mark_remembered(o);
      remembered.push_back(o);
    }

    set_header_needs_scan_on_promote(*o, false);
    mature.push_back(o);
  }
}

static sweep_result
sweep_and_promote_nursery(object_list& nursery, object_list& mature,
                          object_list& remembered,
                          allocator& alloc) {
  std::size_t nursery_size_reduction = 0;
  std::size_t total_size_reduction = 0;
  object_list new_nursery;
  object_list to_promote;

  for (object_header* o : nursery) {
    if (object_color(o) == color::white) {
      std::size_t size = storage_size(o);
      nursery_size_reduction += size;
      total_size_reduction += size;
      deallocate(o, alloc);
    } else {
      assert(object_color(o) == color::black); // No gray objects here
      set_object_color(o, color::white);

      increment_object_age(o);

      if (object_age(o) < mature_age)
        new_nursery.push_back(o);
      else {
        nursery_size_reduction += storage_size(o);
        to_promote.push_back(o);
      }
    }
  }

  filter_remembered_set(remembered);
  promote_objects(to_promote, mature, remembered);

  nursery = std::move(new_nursery);
  return {total_size_reduction, nursery_size_reduction};
}

void
free_store::collect_garbage(bool force_major) {
  if (force_major || alloc_size_ > threshold_alloc_size_)
    collect_major();
  else
    collect_minor();
  
  threshold_alloc_size_ = std::max(min_alloc_size, 3 * alloc_size_ / 2); 
  want_collection_ = false;
}

void
free_store::collect_major() {
  mark_all(roots_);

  auto [swept_total_weak, swept_nursery_weak]
    = sweep_all_weak_boxes(weak_boxes_, alloc_);

  auto [swept_total, swept_nursery]
    = sweep_all(mature_, nursery_, remembered_set_, alloc_);

  std::size_t deallocated_total = swept_total_weak + swept_total;
  std::size_t deallocated_nursery = swept_nursery_weak + swept_nursery;

  assert(deallocated_total <= alloc_size_);
  assert(deallocated_nursery <= nursery_alloc_size_);

  alloc_size_ -= deallocated_total;
  nursery_alloc_size_ -= deallocated_nursery;
}

void
free_store::collect_minor() {
  mark_nursery(roots_, remembered_set_);

  std::size_t weak_swept = sweep_nursery_weak_boxes(weak_boxes_, alloc_);
  auto [all_swept, nursery_swept]
    = sweep_and_promote_nursery(nursery_, mature_, remembered_set_, alloc_);

  std::size_t nursery_swept_total = weak_swept + nursery_swept;
  std::size_t all_swept_total = weak_swept + all_swept;

  assert(all_swept_total <= alloc_size_);
  assert(nursery_swept_total <= nursery_alloc_size_);

  alloc_size_ -= all_swept_total;
  nursery_alloc_size_ -= nursery_swept_total;
}

#ifndef NDEBUG
void
free_store::check_consistency() {
  std::size_t real_nursery_size = 0;
  std::size_t real_total_size = 0;

  for (object_header* o : nursery_) {
    real_nursery_size += storage_size(o);
    real_total_size += storage_size(o);
    assert(object_age(o) < mature_age);
    assert(object_color(o) == color::white);
  }

  for (object_header* o : mature_) {
    real_total_size += storage_size(o);
    assert(object_age(o) == mature_age);
    assert(object_color(o) == color::white);
  }

  for (ptr<weak_box> b : weak_boxes_) {
    real_total_size += storage_size(b.header());
    if (object_age(b) < mature_age)
      real_nursery_size += storage_size(b.header());

    assert(object_color(b) == color::white);
  }

  assert(alloc_size_ == real_total_size);
  assert(nursery_alloc_size_ == real_nursery_size);

  for (object_header* o : remembered_set_)
    assert(object_remembered(o));
}
#endif

} // namespace insider
