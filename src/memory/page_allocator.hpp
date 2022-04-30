#ifndef INSIDER_MEMORY_PAGE_ALLOCATOR_HPP
#define INSIDER_MEMORY_PAGE_ALLOCATOR_HPP

#include <cstdint>
#include <memory>
#include <vector>

namespace insider {

static constexpr std::size_t page_size = 4096;

class page_allocator {
public:
  using page = std::unique_ptr<std::byte[]>;

  page
  allocate() {
    if (!reserve_.empty()) {
      page result = std::move(reserve_.back());
      reserve_.pop_back();
      return result;
    }

    ++allocated_pages_;
    return std::make_unique<std::byte[]>(page_size);
  }

  void
  deallocate(page p) {
    reserve_.emplace_back(std::move(p));
  }

  void
  keep_at_most(std::size_t n) {
    if (reserve_.size() > n) {
      deallocated_pages_ += reserve_.size() - n;
      reserve_.resize(n);
    }
  }

  std::size_t
  reserve_pages() const { return reserve_.size(); }

  std::size_t
  allocated_pages() const { return allocated_pages_; }

  std::size_t
  deallocated_pages() const { return deallocated_pages_; }

  void
  reset_stats() {
    allocated_pages_ = 0;
    deallocated_pages_ = 0;
  }

private:
  std::vector<page> reserve_;
  std::size_t allocated_pages_ = 0;
  std::size_t deallocated_pages_ = 0;
};

} // namespace insider

#endif
