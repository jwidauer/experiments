#pragma once

#include <concepts>
#include <type_traits>
#include <utility>

#include "type_traits.hpp"

namespace ctr {

template <typename T>
union MaybeUninit {
  using value_type = T;  // NOLINT(readability-identifier-naming)

  MaybeUninit()
    requires std::is_trivially_default_constructible_v<T>
  = default;

  MaybeUninit()
    requires(!std::is_trivially_default_constructible_v<T>)
  {}

  template <typename... Args>
    requires std::constructible_from<T, Args...>
  constexpr explicit MaybeUninit(Args&&... args) : value_{std::forward<Args>(args)...} {}

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
