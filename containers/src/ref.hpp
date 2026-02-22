#pragma once

#include <memory>
#include <type_traits>

template <typename T>
struct Ref {
  using type = T;  // NOLINT(readability-identifier-naming) STL-like type alias

  template <typename U>
    requires(!std::is_same_v<Ref, std::decay_t<U>>)
  constexpr explicit Ref(U&& value) : ptr_{Ref::convert(std::forward<U>(value))} {}

  constexpr Ref(const Ref&) = default;
  constexpr auto operator=(const Ref&) -> Ref& = default;

  constexpr explicit operator type&() const noexcept { return this->get(); }

  constexpr auto get() const noexcept -> type& { return *ptr_; }

  constexpr auto operator*() const noexcept -> type& { return *ptr_; }
  constexpr auto operator->() const noexcept -> type* { return ptr_; }

 private:
  constexpr static auto convert(type& val) noexcept -> type* { return std::addressof(val); }
  constexpr static auto convert(type&&) = delete;  // NOLINT(modernize-use-equals-delete)

  type* ptr_;
};
