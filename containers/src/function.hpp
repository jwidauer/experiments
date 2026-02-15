#pragma once

#include <cassert>
#include <functional>
#include <new>
#include <utility>

#include "src/uninitialized_array.hpp"

namespace detail {

template <typename T>
struct Wrapper {
  using type = T;
};

template <typename R, typename... Args>
struct VTable {
  using StoragePtr = void*;

  using InvokeFn = R (*)(StoragePtr, Args...);
  using CopyFn = void (*)(StoragePtr, StoragePtr);
  using DestroyFn = void (*)(StoragePtr);

  const InvokeFn invoke;
  const CopyFn copy;
  const DestroyFn destroy;

  explicit constexpr VTable() noexcept
      : invoke{static_cast<InvokeFn>([](StoragePtr, Args&&...) -> R { assert(false && "Invoking empty function"); })},
        copy{static_cast<CopyFn>([](StoragePtr, StoragePtr) -> void {})},
        destroy{static_cast<DestroyFn>([](StoragePtr) noexcept -> void {})} {}

  template <typename F>
  explicit constexpr VTable(Wrapper<F> /*unused*/) noexcept
      : invoke{static_cast<InvokeFn>([](StoragePtr storage, Args... args) -> R {
          return (*static_cast<F*>(storage))(std::forward<Args>(args)...);
        })},
        copy{static_cast<CopyFn>([](StoragePtr dest, StoragePtr src) -> void { new (dest) F(*static_cast<F*>(src)); })},
        destroy{static_cast<DestroyFn>([](StoragePtr storage) noexcept -> void { static_cast<F*>(storage)->~F(); })} {}

  VTable(const VTable&) = delete;
  auto operator=(const VTable&) -> VTable& = delete;

  VTable(VTable&&) = delete;
  auto operator=(VTable&&) -> VTable& = delete;

  ~VTable() = default;
};

template <typename R, typename... Args>
static const VTable<R, Args...> kEmptyVtable{};

template <typename R, typename... Args>
static const auto kEmptyVtablePtr = std::addressof(kEmptyVtable<R, Args...>);

}  // namespace detail

template <typename, std::size_t>
struct Function;

template <typename R, typename... Args, std::size_t N>
struct Function<R(Args...), N> {
  using Storage = AlignedStorage<void*, N>;

  using Vtable = detail::VTable<R, Args...>;
  using VtablePtr = const Vtable*;

  constexpr Function() noexcept : vtable_{detail::kEmptyVtablePtr<R, Args...>} {}

  template <typename F>
    requires std::is_invocable_r_v<R, F, Args...>
  constexpr explicit Function(F&& f) {
    static_assert(sizeof(F) <= sizeof(storage_), "Function object too large for the specified storage size");
    static_assert(alignof(F) <= alignof(Storage), "Function object alignment requirement exceeds storage alignment");

    static constexpr Vtable vt{detail::Wrapper<F>{}};
    vtable_ = std::addressof(vt);

    new (std::addressof(storage_)) F(std::forward<F>(f));
  }

  constexpr Function(const Function& other) : vtable_{other.vtable_} {
    vtable_->copy(std::addressof(storage_), std::addressof(other.storage_));
  }

  constexpr auto operator=(const Function& other) -> Function& {
    if (this != std::addressof(other)) {
      destroy();
      vtable_ = other.vtable_;
      vtable_->copy(std::addressof(storage_), std::addressof(other.storage_));
    }
    return *this;
  }

  constexpr Function(Function&& other) noexcept
      : vtable_{std::exchange(other.vtable_, detail::kEmptyVtablePtr<R, Args...>)},
        storage_{std::move(other.storage_)} {}

  constexpr auto operator=(Function&& other) noexcept -> Function& {
    if (this != std::addressof(other)) {
      destroy();
      vtable_ = std::exchange(other.vtable_, detail::kEmptyVtablePtr<R, Args...>);
      storage_ = std::move(other.storage_);
    }
    return *this;
  }

  constexpr ~Function() { destroy(); }

  constexpr auto operator()(Args&&... args) const -> R {
    return vtable_->invoke(std::addressof(storage_), std::forward<Args>(args)...);
  }

 private:
  constexpr void destroy() noexcept { vtable_->destroy(std::addressof(storage_)); }

  VtablePtr vtable_;
  mutable Storage storage_;
};
