#pragma once

#include <concepts>
#include <type_traits>
#include <utility>

#include "type_traits.hpp"

namespace ctr {

template <typename T>
union MaybeUninit {
  MaybeUninit()
    requires std::is_trivially_default_constructible_v<T>
  = default;

  MaybeUninit()
    requires(!std::is_trivially_default_constructible_v<T>)
  {}

  template <typename U = std::remove_cv_t<T>>
    requires std::constructible_from<T, U>
  explicit MaybeUninit(U&& val) : value_{std::forward<U>(val)} {}

  ~MaybeUninit()
    requires std::is_trivially_destructible_v<T>
  = default;

  ~MaybeUninit()
    requires(!std::is_trivially_destructible_v<T>)
  {}

  template <typename Self>
  [[nodiscard]] constexpr auto data(this Self& self) noexcept -> copy_const_t<Self, T>* {
    return std::addressof(self.value_);
  }

 private:
  T value_;
};

}  // namespace ctr
