#pragma once

#include <array>

#include "type_traits.hpp"

namespace ctr {

template <class T, std::size_t N>
struct AlignedStorage {
  static constexpr std::size_t alignment = alignof(T);
  static constexpr std::size_t size = sizeof(T);
  static constexpr std::size_t aligned_size = size % alignment == 0 ? size : size + alignment - (size % alignment);

 private:
  struct alignas(T) Storage {
    alignas(T) std::array<std::byte, aligned_size * N> data;
  };

  Storage data_;

 public:
  template <typename Self>
  [[nodiscard]] constexpr auto data(this Self& self) -> copy_const_t<Self, Storage>* {
    return std::addressof(self.data_);
  }
};

}  // namespace ctr
