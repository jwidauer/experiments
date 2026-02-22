#pragma once

#include <utility>

namespace util {

/** Shorthand for static_cast. */
template <typename T, typename U>
[[nodiscard]] static constexpr auto as(U&& u) -> T {
  return static_cast<T>(std::forward<U>(u));
}

}  // namespace util
