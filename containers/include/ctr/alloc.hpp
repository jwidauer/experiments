#pragma once

#include <array>
#include <bitset>
#include <cassert>
#include <cstddef>
#include <utility>

#include "type_traits.hpp"

namespace ctr {

template <std::size_t BlockSize, std::size_t BlockCount>
  requires(BlockSize > 0) && (BlockCount > 0)
struct Smallocator {
  // NOLINTBEGIN(readability-identifier-naming)
  using size_type = SmallestTypeHolding<BlockCount>;
  // NOLINTEND(readability-identifier-naming)

  template <typename T>
  constexpr auto allocate() -> T* {
    constexpr auto alignment = alignof(T);

    constexpr auto required_blocks = required_blocks_for<T>();
    static_assert(required_blocks > 0 && required_blocks <= BlockCount, "Type too large for allocator");

    constexpr auto blocks_per_alignment = alignment / BlockSize;
    constexpr auto max_iter = used_blocks_.size() - required_blocks + 1;

    BlockSet mask = (1U << required_blocks) - 1;  // Mask for required blocks
    for (size_type i = 0; i < max_iter; i += blocks_per_alignment) {
      mask <<= blocks_per_alignment;
      if ((used_blocks_ & mask).none()) {  // Found contiguous free blocks
        used_blocks_ |= mask;              // Mark blocks as used
        return std::bit_cast<T*>(buffer_.data() + (i * BlockSize));
      }
    }
    return nullptr;  // No suitable blocks found
  }

  template <typename T>
  constexpr void deallocate(const T* const ptr) {
    constexpr auto required_blocks = required_blocks_for<T>();

    const auto* const byte_ptr = std::bit_cast<std::byte*>(ptr);
    assert(is_inside_buffer(byte_ptr) && "Pointer does not belong to this allocator");

    const auto offset = distance(std::as_const(buffer_).data(), byte_ptr);
    assert(offset % BlockSize == 0 && "Pointer is not aligned to block size");

    const auto block_index = offset / BlockSize;
    assert(block_index + required_blocks <= BlockCount && "Invalid deallocation size");

    const BlockSet mask = ((1U << required_blocks) - 1) << block_index;  // Mask for blocks to free
    assert((used_blocks_ & mask) == mask && "Blocks to deallocate are not all allocated");

    used_blocks_ &= ~mask;  // Mark blocks as free
  }

 private:
  template <typename T>
  constexpr auto required_blocks_for() {
    return (sizeof(T) + BlockSize - 1) / BlockSize;
  }

  template <typename Iter>
  static constexpr auto distance(Iter first, Iter second) -> size_type {
    return static_cast<size_type>(std::distance(first, second));
  }

  constexpr auto is_inside_buffer(const std::byte* const ptr) const -> bool {
    return buffer_.begin() <= ptr && ptr < buffer_.end();
  }

  using BlockSet = std::bitset<BlockCount>;

  alignas(alignof(std::max_align_t)) std::array<std::byte, BlockSize * BlockCount> buffer_{};
  BlockSet used_blocks_{};
};

}  // namespace ctr
