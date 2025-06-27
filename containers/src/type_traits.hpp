#pragma once

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
