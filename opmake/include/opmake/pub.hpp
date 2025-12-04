#pragma once

#include <cstddef>

namespace opmake {

template <std::size_t N>
constexpr auto factorial() -> std::size_t {
  if constexpr (N == 0 || N == 1) {
    return 1;
  } else {
    return N * factorial<N - 1>();
  }
}

}  // namespace opmake
