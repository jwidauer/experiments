#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <print>
#include <ranges>
#include <span>

namespace consthash {

namespace detail {

[[nodiscard]] consteval auto ptr_size() -> std::size_t { return sizeof(std::size_t); }

template <std::size_t ptr_size>
  requires(ptr_size == 4 || ptr_size == 8)
using hash_t = std::conditional_t<ptr_size == 4, uint32_t, uint64_t>;

template <std::size_t ptr_size>
struct MixingConstants {
  hash_t<ptr_size> m;
  std::size_t r;
};

template <std::size_t ptr_size>
  requires(ptr_size == 4 || ptr_size == 8)
[[nodiscard]] consteval auto mixing_constants() -> MixingConstants<ptr_size> {
  constexpr std::size_t lower = 0x5bd1e995;
  if constexpr (ptr_size == 4) {
    return {.m = lower, .r = 24};
  } else {
    constexpr std::size_t upper = 0xc6a4a793;
    return {.m = (upper << 32) + lower, .r = 47};
  }
}

template <std::size_t ptr_size>
[[nodiscard]] constexpr auto shift_mix(std::size_t v) -> hash_t<ptr_size> {
  return v ^ (v >> mixing_constants<ptr_size>().r);
}

template <typename T>
  requires std::ranges::contiguous_range<T> && std::same_as<std::byte, std::ranges::range_value_t<T>>
[[nodiscard]] constexpr auto as_size(T bytes) -> std::size_t {
  assert(bytes.size() <= ptr_size());

  std::size_t result = 0;
  for (auto elem : bytes | std::views::reverse) {
    result = (result << 8) + static_cast<unsigned char>(elem);
  }
  return result;
}

template <std::size_t ptr_size>
[[nodiscard]] constexpr auto initialize(std::size_t len, std::size_t seed) -> hash_t<ptr_size> {
  if constexpr (ptr_size == 4) {
    return seed ^ len;
  } else {
    constexpr auto m = mixing_constants<ptr_size>().m;
    return seed ^ (len * m);
  }
}

template <std::size_t ptr_size>
[[nodiscard]] constexpr auto finalize(hash_t<ptr_size> hash) -> hash_t<ptr_size> {
  constexpr auto m = mixing_constants<ptr_size>().m;

  if constexpr (ptr_size == 4) {
    hash ^= hash >> 13;
    hash *= m;
    hash ^= hash >> 15;
    return hash;
  } else {
    hash = shift_mix<ptr_size>(hash) * m;
    hash = shift_mix<ptr_size>(hash);
    return hash;
  }
}

}  // namespace detail

template <std::size_t ptr_size = detail::ptr_size()>
[[nodiscard]] constexpr auto hash(std::span<const std::byte> data, std::size_t seed = 0) -> detail::hash_t<ptr_size> {
  using hash_t = detail::hash_t<ptr_size>;

  const std::size_t aligned_len = data.size() & ~(ptr_size - 1);
  const auto subspan = data.subspan(0, aligned_len);

  hash_t hash = detail::initialize<ptr_size>(data.size(), seed);

  constexpr auto mul = detail::mixing_constants<ptr_size>().m;
  for (auto chunk : subspan | std::views::chunk(ptr_size)) {
    const auto cur_data = detail::shift_mix<ptr_size>(detail::as_size(chunk) * mul) * mul;
    if constexpr (ptr_size == 4) {
      hash *= mul;
      hash ^= cur_data;
    } else {
      hash ^= cur_data;
      hash *= mul;
    }
  }

  // Handle remaining bytes.
  if (const auto remainder = data.subspan(aligned_len); !remainder.empty()) {
    const auto cur_data = detail::as_size(remainder);
    hash ^= cur_data;
    hash *= mul;
  }

  return detail::finalize<ptr_size>(hash);
}

}  // namespace consthash
