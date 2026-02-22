#pragma once

#include <cassert>
#include <type_traits>
#include <utility>

#include "ref.hpp"
#include "uninitialized_array.hpp"
#include "util.hpp"

namespace detail {

template <typename Storage, typename R, typename... Args>
struct VTable {
  using InvokeFn = R (*)(Storage&, Args&&...);
  using CopyFn = void (*)(Storage&, Storage&);
  using DestroyFn = void (*)(Storage&);

  const InvokeFn invoke;
  const CopyFn copy;
  const DestroyFn destroy;

  explicit constexpr VTable() noexcept
      : invoke{util::as<InvokeFn>([](Storage&, Args&&...) -> R { assert(false && "Invoking empty function"); })},
        copy{util::as<CopyFn>([](Storage&, Storage&) -> void {})},
        destroy{util::as<DestroyFn>([](Storage&) noexcept -> void {})} {}

  template <typename F>
  explicit constexpr VTable(std::type_identity<F> /*unused*/) noexcept
      : invoke{util::as<InvokeFn>([](Storage& storage, Args&&... args) -> R {
          return (*util::as<F*>(storage.data()))(std::forward<Args>(args)...);
        })},
        copy{util::as<CopyFn>(
            [](Storage& dest, Storage& src) -> void { new (dest.data()) F(*util::as<F*>(src.data())); })},
        destroy{util::as<DestroyFn>([](Storage& storage) noexcept -> void {
          util::as<F*>(storage.data())->~F();
          storage.template destroy<F>();
        })} {}

  VTable(const VTable&) = delete;
  auto operator=(const VTable&) -> VTable& = delete;

  VTable(VTable&&) = delete;
  auto operator=(VTable&&) -> VTable& = delete;

  ~VTable() = default;
};

template <typename Storage, typename R, typename... Args>
static constexpr VTable<Storage, R, Args...> empty_vtable{};

template <typename Storage, typename R, typename... Args>
static constexpr auto empty_vtable_ptr = std::addressof(empty_vtable<Storage, R, Args...>);

}  // namespace detail

template <typename T, typename U>
concept StoragePolicy = requires(T storage, U&& value) {
  { storage.store(std::forward<decltype(value)>(value)) } noexcept;
  { storage.template destroy<std::decay_t<decltype(value)>>() } noexcept;
  { storage.data() } -> std::same_as<void*>;
  { std::as_const(storage).data() } -> std::same_as<const void*>;
};

template <std::size_t N>
struct InlineStorage {
  template <typename T>
  constexpr void store(T&& value) noexcept {
    static_assert(sizeof(T) <= sizeof(Storage), "Value too large for the specified storage size");
    static_assert(alignof(T) <= alignof(Storage), "Value alignment requirement exceeds maximum supported alignment");

    new (std::addressof(storage_)) T(std::forward<T>(value));
  }

  template <typename T>
  constexpr void destroy() noexcept {}

  [[nodiscard]] constexpr auto data() noexcept -> void* { return storage_.data(); }
  [[nodiscard]] constexpr auto data() const noexcept -> const void* { return storage_.data(); }

 private:
  using Storage = AlignedStorage<void*, N>;
  Storage storage_;
};

template <typename Allocator>
struct AllocatedStorage {
  constexpr explicit AllocatedStorage(Allocator& allocator) : allocator_{allocator} {}

  template <typename T>
  constexpr void store(T&& value) noexcept {
    storage_ = allocator_->template allocate<T>();
    assert(storage_ != nullptr && "Allocator failed to allocate memory for the object");
    new (storage_) T(std::forward<T>(value));
  }

  template <typename T>
  constexpr void destroy() noexcept {
    allocator_->template deallocate<T>(storage_);
    storage_ = nullptr;
  }

  [[nodiscard]] constexpr auto data() noexcept -> void* { return storage_; }
  [[nodiscard]] constexpr auto data() const noexcept -> const void* { return storage_; }

 private:
  Ref<Allocator> allocator_;
  void* storage_{nullptr};
};

template <typename, typename>
struct Function;

template <typename R, typename... Args, typename Storage>
struct Function<R(Args...), Storage> {
  using VTable = detail::VTable<Storage, R, Args...>;
  using VTablePtr = const VTable*;

  constexpr Function() noexcept : vtable_{detail::empty_vtable_ptr<Storage, R, Args...>} {}

  template <typename F>
    requires(std::is_invocable_r_v<R, F, Args...> && StoragePolicy<Storage, F> &&
             std::is_default_constructible_v<Storage>)
  constexpr explicit Function(F&& f) : storage_{} {
    construct(std::forward<F>(f));
  }

  template <typename F, typename... StorageArgs>
    requires(std::is_invocable_r_v<R, F, Args...> && StoragePolicy<Storage, F> &&
             std::is_constructible_v<Storage, StorageArgs...>)
  constexpr explicit Function(F&& f, StorageArgs&&... args) : storage_{std::forward<StorageArgs>(args)...} {
    construct(std::forward<F>(f));
  }

  constexpr Function(const Function& other) : vtable_{other.vtable_} { copy(other); }

  constexpr auto operator=(const Function& other) -> Function& {
    destroy();
    vtable_ = other.vtable_;
    copy(other);
    return *this;
  }

  constexpr Function(Function&& other) noexcept
      : vtable_{std::exchange(other.vtable_, detail::empty_vtable_ptr<R, Args...>)},
        storage_{std::move(other.storage_)} {}

  constexpr auto operator=(Function&& other) noexcept -> Function& {
    destroy();
    vtable_ = std::exchange(other.vtable_, detail::empty_vtable_ptr<R, Args...>);
    storage_ = std::move(other.storage_);
    return *this;
  }

  constexpr ~Function() { destroy(); }

  constexpr auto operator()(Args&&... args) const -> R {
    return vtable_->invoke(storage_, std::forward<Args>(args)...);
  }

 private:
  template <typename F>
  constexpr void construct(F&& f) {
    static constexpr VTable vt{std::type_identity<std::decay_t<F>>{}};
    vtable_ = std::addressof(vt);
    storage_.store(std::forward<F>(f));
  }
  constexpr void destroy() noexcept { vtable_->destroy(storage_); }
  constexpr void copy(const Function& other) { vtable_->copy(storage_, other.storage_); }

  VTablePtr vtable_;
  mutable Storage storage_;
};
