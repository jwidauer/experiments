#pragma once

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <ranges>

namespace consthash {

namespace detail {

[[nodiscard]] consteval auto ptr_size() -> std::size_t { return sizeof(std::size_t); }

template <std::size_t r, typename T>
[[nodiscard]] constexpr auto shift_mix(T v) -> T {
  return v ^ (v >> r);
}

template <typename T, typename R>
  requires std::ranges::contiguous_range<R> && std::ranges::sized_range<R>
[[nodiscard]] constexpr auto as_size(R&& r) -> T {
  assert(std::ranges::size(r) <= sizeof(T));

  return std::ranges::fold_left(r | std::views::reverse, T{0},
                                [](T acc, auto b) -> T { return (acc << 8) | static_cast<T>(b); });
}

template <typename R>
  requires std::ranges::contiguous_range<R> && std::ranges::sized_range<R>
[[nodiscard]] constexpr auto hash32(R&& range, uint32_t seed = 0) -> uint32_t {
  using hash_t = uint32_t;

  constexpr auto ptr_size = std::size_t{4};
  constexpr auto m = hash_t{0x5bd1e995};
  constexpr auto r = std::size_t{24};

  const auto len = std::ranges::size(range);
  const auto aligned_len = len & ~(ptr_size - 1);

  hash_t hash = seed ^ len;

  // Process aligned chunks
  for (auto chunk : range | std::views::take(aligned_len) | std::views::chunk(ptr_size)) {
    const auto cur_data = detail::shift_mix<r>(detail::as_size<hash_t>(chunk) * m) * m;
    hash *= m;
    hash ^= cur_data;
  }

  // Handle remaining bytes
  if (const auto tail = range | std::views::drop(aligned_len); !std::ranges::empty(tail)) {
    const auto cur_data = detail::as_size<hash_t>(tail);
    hash ^= cur_data;
    hash *= m;
  }

  // Do a few final mixes of the hash.
  hash ^= hash >> 13;
  hash *= m;
  hash ^= hash >> 15;
  return hash;
}

template <typename R>
  requires std::ranges::contiguous_range<R> && std::ranges::sized_range<R>
[[nodiscard]] constexpr auto hash64(R&& range) -> uint64_t {
  using hash_t = uint64_t;

  constexpr auto ptr_size = std::size_t{8};
  constexpr auto seed = hash_t{0xc70f6907};
  constexpr auto m = (hash_t{0xc6a4a793} << 32) | hash_t{0x5bd1e995};
  constexpr auto r = std::size_t{47};

  const auto len = std::ranges::size(range);
  const auto aligned_len = len & ~(ptr_size - 1);

  hash_t hash = seed ^ (len * m);

  // Process aligned chunks
  for (auto chunk : range | std::views::take(aligned_len) | std::views::chunk(ptr_size)) {
    const auto cur_data = detail::shift_mix<r>(detail::as_size<hash_t>(chunk) * m) * m;
    hash ^= cur_data;
    hash *= m;
  }

  // Handle remaining bytes
  if (const auto tail = range | std::views::drop(aligned_len); !std::ranges::empty(tail)) {
    const auto cur_data = detail::as_size<hash_t>(tail);
    hash ^= cur_data;
    hash *= m;
  }

  // Do a few final mixes of the hash.
  hash = shift_mix<r>(hash) * m;
  hash = shift_mix<r>(hash);
  return hash;
}

}  // namespace detail

template <typename R>
  requires std::ranges::contiguous_range<R> && std::ranges::sized_range<R>
[[nodiscard]] constexpr auto hash(R&& range) {
  if constexpr (detail::ptr_size() == 4) {
    return detail::hash32(range);
  } else {
    return detail::hash64(range);
  }
}

}  // namespace consthash
