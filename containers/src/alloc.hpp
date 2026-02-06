#pragma once

#include <array>
#include <bit>
#include <bitset>
#include <cassert>
#include <climits>
#include <cstddef>

#include "src/static_vector.hpp"
#include "type_traits.hpp"

namespace pooalloc {

constexpr auto align(std::size_t alignment, std::size_t size, void*& ptr, std::size_t& space) -> void* {
  if (space < size) return nullptr;
  /*
   * align       =  8 = 0b0000_1000
   * addr        = 13 = 0b0000_1101
   *
   * mask        =  7 = 0b0000_0111
   * addr + mask = 20 = 0b0001_0100
   * ~mask       = -8 = 0b1111_1000
   * aligned     = 16 = 0b0001_0000
   */

  const auto intptr = std::bit_cast<uintptr_t>(ptr);
  const auto mask = alignment - 1U;
  const auto aligned = (intptr + mask) & ~mask;
  const auto diff = aligned - intptr;

  if (diff > (space - size)) return nullptr;  // same as diff + size > space, just avoids potential overflow

  space -= diff;
  return ptr = std::bit_cast<void*>(aligned);
}

template <std::size_t Capacity, std::size_t MaxAllocations = Capacity>
struct SillyAllocator {
  // NOLINTBEGIN(readability-identifier-naming)
  using size_type = SmallestTypeHoldingT<Capacity>;
  // NOLINTEND(readability-identifier-naming)

  auto allocate(std::size_t size, std::size_t alignment) -> void* {
    if (allocations_.full() || size == 0 || alignment == 0 || !std::has_single_bit(alignment)) {
      return nullptr;  // Invalid request
    }

    // Simple first-fit allocation strategy
    size_type current_offset = 0;

    const auto mask = alignment - 1;
    for (const auto& alloc : allocations_) {
      size_type aligned_offset = (current_offset + mask) & ~mask;
      if (aligned_offset + size <= alloc.offset) {
        // Found a suitable gap
        allocations_.try_insert(&alloc, Allocation{aligned_offset, static_cast<size_type>(size)});
        return buffer_.data() + aligned_offset;
      }
      current_offset = alloc.offset + alloc.size;
    }

    // Check for space at the end of the buffer
    size_type aligned_offset = (current_offset + mask) & ~mask;
    if (aligned_offset + size <= Capacity) {
      allocations_.try_emplace_back(aligned_offset, static_cast<size_type>(size));
      return buffer_.data() + aligned_offset;
    }

    // No suitable space found
    return nullptr;
  }

  void deallocate(void* ptr) {
    const auto* byte_ptr = static_cast<std::byte*>(ptr);
    assert(buffer_.begin() <= byte_ptr && byte_ptr < buffer_.end() && "Pointer does not belong to this allocator");
    const auto offset = distance(buffer_.begin(), byte_ptr);

    auto iter = std::ranges::find(allocations_, offset, &Allocation::offset);
    assert(iter != allocations_.end() && "Pointer was not allocated");

    allocations_.try_erase(iter);
  }

 private:
  template <typename Iter>
  constexpr auto distance(Iter first, Iter second) const -> size_type {
    return static_cast<size_type>(std::distance(first, second));
  }

  struct Allocation {
    size_type offset{};
    size_type size{};
  };
  std::array<std::byte, Capacity> buffer_;
  ctr::StaticVector<Allocation, MaxAllocations> allocations_{};
};

template <std::size_t BlockSize, std::size_t BlockCount>
  requires(BlockSize > 0) && (BlockCount > 0)
struct PooAlloc {
  // NOLINTBEGIN(readability-identifier-naming)
  using size_type = SmallestTypeHoldingT<BlockCount>;
  // NOLINTEND(readability-identifier-naming)

  template <typename T>
  constexpr auto allocate() -> void* {
    constexpr auto size = sizeof(T);
    constexpr auto alignment = alignof(T);

    constexpr auto required_blocks = (size + BlockSize - 1) / BlockSize;
    static_assert(required_blocks > 0 && required_blocks <= BlockCount, "Type too large for allocator");

    constexpr auto blocks_per_alignment = alignment / BlockSize;
    constexpr auto max_iter = used_blocks_.size() - required_blocks + 1;

    BlockSet mask = (1U << required_blocks) - 1;  // Mask for required blocks
    for (size_type i = 0; i < max_iter; i += blocks_per_alignment) {
      mask <<= blocks_per_alignment;
      if ((used_blocks_ & mask).none()) {  // Found contiguous free blocks
        used_blocks_ |= mask;              // Mark blocks as used
        return buffer_.data() + i * BlockSize;
      }
    }
    return nullptr;  // No suitable blocks found
  }

  template <typename T>
  constexpr void deallocate(void* ptr) {
    constexpr auto size = sizeof(T);

    constexpr auto required_blocks = (size + BlockSize - 1) / BlockSize;

    const auto* byte_ptr = static_cast<std::byte*>(ptr);
    assert(is_inside_buffer(byte_ptr) && "Pointer does not belong to this allocator");

    const auto offset = distance(buffer_.data(), byte_ptr);
    assert(offset % BlockSize == 0 && "Pointer is not aligned to block size");

    const auto block_index = offset / BlockSize;
    assert(block_index + required_blocks <= BlockCount && "Invalid deallocation size");

    const BlockSet mask = ((1U << required_blocks) - 1) << block_index;  // Mask for blocks to free
    assert((used_blocks_ & mask) == mask && "Blocks to deallocate are not all allocated");

    used_blocks_ &= ~mask;  // Mark blocks as free
  }

 private:
  template <typename Iter>
  constexpr auto distance(Iter first, Iter second) const -> size_type {
    return static_cast<size_type>(std::distance(first, second));
  }

  constexpr auto is_inside_buffer(const std::byte* ptr) const -> bool {
    return buffer_.begin() <= ptr && ptr < buffer_.end();
  }

  using BlockSet = std::bitset<BlockCount>;

  alignas(alignof(std::max_align_t)) std::array<std::byte, BlockSize * BlockCount> buffer_{};
  BlockSet used_blocks_{};
};

}  // namespace pooalloc
