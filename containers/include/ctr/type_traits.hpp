#pragma once

#include <cstddef>
#include <cstdint>
#include <limits>
#include <type_traits>

namespace ctr {
namespace detail {

template <typename T>
constexpr T max_v = std::numeric_limits<T>::max();

}  // namespace detail

template <typename In, typename Out>
struct CopyConst {
  using type = Out;  // NOLINT(readability-identifier-naming)
};

template <typename In, typename Out>
struct CopyConst<const In, Out> {
  using type = const Out;  // NOLINT(readability-identifier-naming)
};

template <typename In, typename Out>
struct CopyConst<const In&, Out> {
  using type = const Out;  // NOLINT(readability-identifier-naming)
};

template <typename In, typename Out>
using copy_const_t = typename CopyConst<In, Out>::type;  // NOLINT(readability-identifier-naming)

template <std::size_t N>
consteval auto smallest_type_holding() {
  if constexpr (N <= detail::max_v<std::uint8_t>) {
    return std::type_identity<uint8_t>{};
  } else if constexpr (N <= detail::max_v<std::uint16_t>) {
    return std::type_identity<std::uint16_t>{};
  } else if constexpr (N <= detail::max_v<std::uint32_t>) {
    return std::type_identity<std::uint32_t>{};
  } else if constexpr (N <= detail::max_v<std::uint64_t>) {
    return std::type_identity<std::uint64_t>{};
  } else {
    static_assert(N <= detail::max_v<std::uint64_t>, "No suitable type found to hold the value N");
  }
}

template <std::size_t N>
using SmallestTypeHolding = decltype(smallest_type_holding<N>())::type;

template <typename Fn>
concept TransparentComparator = requires { typename Fn::is_transparent; };

}  // namespace ctr
