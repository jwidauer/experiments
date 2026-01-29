#pragma once

#include <algorithm>
#include <array>
#include <bit>
#include <span>

namespace bitconv {

template <typename T>
constexpr bool should_copy_v = sizeof(T) <= 2 * sizeof(void*) && std::is_trivially_copyable_v<T>;

template <typename T>
using value_or_ref_t = std::conditional_t<should_copy_v<T>, T, const T&>;

template <typename T>
using as_bytes_t = std::array<const std::byte, sizeof(T)>;

template <typename T>
using as_mut_bytes_t = std::array<std::byte, sizeof(T)>;

template <typename From>
  requires(should_copy_v<From>)
constexpr auto as_bytes(From from) -> as_bytes_t<From> {
  return std::bit_cast<as_bytes_t<From>>(from);
}

template <typename From>
  requires(!should_copy_v<From>)
constexpr auto as_bytes(const From& from) -> as_bytes_t<From> {
  using real_t = std::remove_cv_t<std::remove_reference_t<From>>;
  return std::bit_cast<as_bytes_t<real_t>>(from);
}

template <typename To>
constexpr auto from_bytes(value_or_ref_t<as_bytes_t<To>> from) -> To {
  return std::bit_cast<To>(from);
}

template <typename To>
constexpr auto from_bytes(std::span<const std::byte> from) -> To {
  as_mut_bytes_t<To> array{};
  std::ranges::copy(from.first(sizeof(To)), array.begin());
  return std::bit_cast<To>(array);
}

}  // namespace bitconv
