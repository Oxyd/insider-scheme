#ifndef INSIDER_MEMORY_ALLOCATOR_HPP
#define INSIDER_MEMORY_ALLOCATOR_HPP

#include "object.hpp"

#include <cstddef>
#include <memory>
#include <new>
#include <tuple>
#include <vector>

namespace insider {

class fixed_size_allocator {
public:
  explicit
  fixed_size_allocator(std::size_t object_size)
    : object_size_{object_size}
  { }

  std::byte*
  allocate() {
    if (!next_free_)
      allocate_page();

    std::byte* storage = next_free_;
    next_free_ = *reinterpret_cast<std::byte**>(next_free_);
    return storage;
  }

  void
  deallocate(std::byte* x) { push_to_free_list(x); }

private:
  std::size_t object_size_;
  std::size_t const page_size = (4096 / object_size_) * object_size_;

  struct deallocator {
    void
    operator () (std::byte* x) {
      ::operator delete [] (x, std::align_val_t{object_alignment});
    }
  };

  using page = std::unique_ptr<std::byte[], deallocator>;

  std::vector<page> pages_;
  std::byte*        next_free_ = nullptr;

  void
  allocate_page() {
    void* storage
      = ::operator new [] (page_size, std::align_val_t{object_alignment});
    page p{static_cast<std::byte*>(storage)};

    for (std::byte* current = p.get();
         current < p.get() + page_size;
         current += object_size_)
      push_to_free_list(current);

    pages_.emplace_back(std::move(p));
  }

  void
  push_to_free_list(std::byte* storage) {
    new (storage) std::byte*{next_free_};
    next_free_ = storage;
  }
};

class allocator {
public:
  std::byte*
  allocate(std::size_t size) {
    if (size >= min_common_size && size <= max_common_size)
      return common_size_allocators_[(size / object_alignment) - 2].allocate();
    else
      return new std::byte[size];
  }

  void
  deallocate(std::byte* x, std::size_t size) {
    if (size >= min_common_size && size <= max_common_size)
      common_size_allocators_[(size / object_alignment) - 2].deallocate(x);
    else
      delete [] x;
  }

private:
  // max_common_size was chosen so that a syntax object can still be a
  // common-sized object.

  static constexpr std::size_t min_common_size = 2 * object_alignment;
  static constexpr std::size_t max_common_size = 13 * object_alignment;

  // Common-size object have sizes that are all multiple of object_alignment.
  // This means we can conveniently store the allocators for those sizes in an
  // array such that objects of size S are handled by
  // common_size_allocator_[(S / object_alignment) - 2].

  static constexpr std::size_t num_common_size_allocators
    = 1 + (max_common_size - min_common_size) / object_alignment;
  using common_size_allocator_array
    = std::array<fixed_size_allocator, num_common_size_allocators>;

  common_size_allocator_array common_size_allocators_{
    fixed_size_allocator{2 * object_alignment},
    fixed_size_allocator{3 * object_alignment},
    fixed_size_allocator{4 * object_alignment},
    fixed_size_allocator{5 * object_alignment},
    fixed_size_allocator{6 * object_alignment},
    fixed_size_allocator{7 * object_alignment},
    fixed_size_allocator{8 * object_alignment},
    fixed_size_allocator{9 * object_alignment},
    fixed_size_allocator{10 * object_alignment},
    fixed_size_allocator{11 * object_alignment},
    fixed_size_allocator{12 * object_alignment},
    fixed_size_allocator{13 * object_alignment}
  };
};

} // namespace insider

#endif
