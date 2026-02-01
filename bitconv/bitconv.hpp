#pragma once

#include <array>
#include <bit>
#include <cassert>
#include <cstring>
#include <span>
#include <type_traits>

namespace bitconv {

namespace detail {

template <typename To, typename ByteT, std::size_t Extent>
void assert_sizes_match(std::span<ByteT, Extent> from) {
  if constexpr (Extent != std::dynamic_extent) {
    static_assert(from.size() == sizeof(To));
  } else {
    assert(from.size() == sizeof(To));
  }
}

}  // namespace detail

template <typename T>
constexpr bool should_copy_v = sizeof(T) <= 2 * sizeof(void*) && std::is_trivially_copyable_v<T>;

template <typename T>
using value_or_ref_t = std::conditional_t<should_copy_v<T>, T, const T&>;

template <typename T>
using byte_array_t = std::array<const std::byte, sizeof(T)>;

template <typename T>
using mut_byte_array_t = std::array<std::byte, sizeof(T)>;

template <typename T>
using byte_span_t = std::span<const std::byte, sizeof(T)>;

template <typename From>
  requires(should_copy_v<From>)
constexpr auto to_bytes(From from) -> byte_array_t<From> {
  return std::bit_cast<byte_array_t<From>>(from);
}

template <typename From>
  requires(!should_copy_v<From>)
constexpr auto to_bytes(const From& from) -> byte_array_t<From> {
  using real_t = std::remove_cv_t<std::remove_reference_t<From>>;
  return std::bit_cast<byte_array_t<real_t>>(from);
}

template <typename From>
  requires(should_copy_v<From>)
constexpr auto to_mut_bytes(From from) -> mut_byte_array_t<From> {
  return std::bit_cast<mut_byte_array_t<From>>(from);
}

template <typename From>
  requires(!should_copy_v<From>)
constexpr auto to_mut_bytes(From& from) -> mut_byte_array_t<From> {
  using real_t = std::remove_cvref_t<From>;
  return std::bit_cast<mut_byte_array_t<real_t>>(from);
}

template <typename From>
constexpr auto to_byte_span(const From& from) -> byte_span_t<From> {
  using real_t = std::remove_cvref_t<From>;
  return byte_span_t<From>{std::bit_cast<const std::byte*>(&from), sizeof(real_t)};
}

template <typename To>
constexpr auto from_bytes(value_or_ref_t<byte_array_t<To>> from) -> To {
  return std::bit_cast<To>(from);
}

template <typename To>
constexpr auto from_mut_bytes(value_or_ref_t<mut_byte_array_t<To>> from) -> To {
  return std::bit_cast<To>(from);
}

template <typename To, std::size_t Extent>
  requires std::is_trivially_copyable_v<To>
constexpr auto from_bytes(std::span<const std::byte, Extent> from) -> To {
  detail::assert_sizes_match<To>(from);
  To out;
  std::memcpy(&out, from.data(), sizeof(To));
  return out;
}

template <typename To, std::size_t Extent>
  requires std::is_trivially_copyable_v<To>
constexpr auto from_mut_bytes(std::span<std::byte, Extent> from) -> To {
  detail::assert_sizes_match<To>(from);
  To out;
  std::memcpy(&out, from.data(), sizeof(To));
  return out;
}

template <typename To, std::size_t Extent>
  requires std::is_trivially_copyable_v<To>
constexpr auto from_bytes_unsafe(std::span<const std::byte, Extent> from) -> To {
  detail::assert_sizes_match<To>(from);
  return *std::bit_cast<const To*>(from.data());
}

}  // namespace bitconv
