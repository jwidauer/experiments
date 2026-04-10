#pragma once

#include <algorithm>
#include <cstddef>
#include <span>
#include <tl/optional.hpp>
#include <tuple>
#include <utility>

#include "type_traits.hpp"
#include "uninitialized_array.hpp"

namespace ctr {

template <std::size_t N, typename... Ts>
  requires(sizeof...(Ts) > 0)
struct MultiObjectVector {
  // NOLINTBEGIN(readability-identifier-naming)
  using size_type = SmallestTypeHolding<N>;
  using difference_type = std::ptrdiff_t;
  // NOLINTEND(readability-identifier-naming)

  using Storage = std::tuple<UninitializedArray<Ts, N>...>;

  template <std::size_t I>
  using StorageObject = std::tuple_element_t<I, Storage>;

  template <std::size_t I>
  using StorageElement = typename StorageObject<I>::value_type;

  constexpr MultiObjectVector() = default;

  constexpr ~MultiObjectVector()
    requires((std::is_trivially_destructible_v<Ts>) || ...)
  = default;

  constexpr ~MultiObjectVector()
    requires((!std::is_trivially_destructible_v<Ts>) || ...)
  {
    clear();
  }

  [[nodiscard]] constexpr auto size() const -> size_type { return size_; }
  [[nodiscard]] constexpr auto capacity() const -> size_type { return N; }
  [[nodiscard]] constexpr auto empty() const -> bool { return size_ == 0; }
  [[nodiscard]] constexpr auto full() const -> bool { return size_ == N; }

  template <std::size_t I, typename Self>
  [[nodiscard]] constexpr auto begin(this Self& self) {
    return self.template storage<I>().begin();
  }

  template <typename T, typename Self>
  [[nodiscard]] constexpr auto begin(this Self& self) {
    return self.template storage<T>().begin();
  }

  template <std::size_t I, typename Self>
  [[nodiscard]] constexpr auto end(this Self& self) {
    return self.template storage<I>().begin() + self.size_;
  }

  template <typename T, typename Self>
  [[nodiscard]] constexpr auto end(this Self& self) {
    return self.template storage<T>().begin() + self.size_;
  }

  template <std::size_t I, typename Self>
  [[nodiscard]] constexpr auto data(this Self& self) -> copy_const_t<Self, StorageElement<I>>* {
    return std::get<I>(self.storages_).data();
  }

  template <typename T, typename Self>
  [[nodiscard]] constexpr auto data(this Self& self) -> copy_const_t<Self, T>* {
    return std::get<UninitializedArray<T, N>>(self.storages_).data();
  }

  template <std::size_t I, typename Self>
  [[nodiscard]] constexpr auto storage(this Self& self) -> std::span<copy_const_t<Self, StorageElement<I>>> {
    return {std::get<I>(self.storages_).data(), self.size()};
  }

  template <typename T, typename Self>
  [[nodiscard]] constexpr auto storage(this Self& self) -> std::span<copy_const_t<Self, T>> {
    return {std::get<UninitializedArray<T, N>>(self.storages_).data(), self.size()};
  }

  template <std::size_t I, typename Self>
  [[nodiscard]] constexpr auto at(this Self& self, size_type idx)
      -> tl::optional<copy_const_t<Self, typename StorageObject<I>::value_type>&> {
    if (idx >= self.size_) return tl::nullopt;
    return self.template storage<I>()[idx];
  }

  template <typename T, typename Self>
  [[nodiscard]] constexpr auto at(this Self& self, size_type idx) -> tl::optional<copy_const_t<Self, T>&> {
    if (idx >= self.size_) return tl::nullopt;
    return self.template storage<T>()[idx];
  }

  constexpr auto try_push_back(const Ts&... values) -> tl::optional<size_type> {
    if (full()) return tl::nullopt;

    ++size_;
    construct_at(size_ - 1, values...);
    return size_ - 1;
  }

  constexpr auto try_push_back(Ts&&... values) -> tl::optional<size_type> {
    if (full()) return tl::nullopt;

    ++size_;
    construct_at(size_ - 1, std::move(values)...);
    return size_ - 1;
  }

  constexpr auto try_insert(size_type index, const Ts&... values) -> tl::optional<size_type> {
    if (full() || index > size_) return tl::nullopt;

    // Move elements to make space
    ((std::ranges::move_backward(begin<Ts>() + index, end<Ts>(), end<Ts>() + 1)), ...);

    ++size_;
    // Insert new elements
    construct_at(index, values...);
    return index;
  }

  constexpr auto try_insert(size_type index, Ts&&... values) -> tl::optional<size_type> {
    if (full() || index > size_) return tl::nullopt;

    // Move elements to make space
    ((std::ranges::move_backward(begin<Ts>() + index, end<Ts>(), end<Ts>() + 1)), ...);

    ++size_;
    // Insert new elements
    construct_at(index, std::move(values)...);
    return index;
  }

  constexpr void try_pop_back() {
    if (empty()) return;

    destroy_at(size_ - 1);
    --size_;
  }

  constexpr void clear() {
    for (std::size_t i = 0; i < size_; ++i) destroy_at(i);
    size_ = 0;
  }

  constexpr auto try_erase(size_type index) -> bool {
    if (index >= size_) return false;

    // Destroy elements at index
    destroy_at(index);

    // Move elements to fill the gap
    ((std::ranges::move(begin<Ts>() + index + 1, end<Ts>(), begin<Ts>() + index)), ...);

    --size_;
    return true;
  }

  template <std::size_t I>
  constexpr auto find(const StorageElement<I>& value) const -> tl::optional<size_type> {
    return find_impl(storage<I>(), value);
  }

  template <typename T>
  constexpr auto find(const T& value) const -> tl::optional<size_type> {
    return find_impl(storage<T>(), value);
  }

  template <std::size_t I>
  constexpr auto contains(const StorageElement<I>& value) const -> bool {
    return std::ranges::contains(storage<I>(), value);
  }

  template <typename T>
  constexpr auto contains(const T& value) const -> bool {
    return std::ranges::contains(storage<T>(), value);
  }

 private:
  static constexpr auto indices = std::index_sequence_for<Ts...>{};

  [[nodiscard]] constexpr auto find_impl(const auto& store, const auto& value) const -> tl::optional<size_type> {
    const auto iter = std::ranges::find(store, value);
    if (iter == store.end()) return tl::nullopt;

    return static_cast<size_type>(std::distance(store.begin(), iter));
  }

  template <std::size_t I, typename Self>
  [[nodiscard]] constexpr auto address_of(this Self& self, size_type idx) -> copy_const_t<Self, StorageElement<I>>* {
    return std::get<I>(self.storages_)[idx];
  }

  template <typename... Args, std::size_t... I>
  constexpr void construct_at(size_type index, std::index_sequence<I...> /*unused*/, Args&&... args) {
    ((std::construct_at(address_of<I>(index), std::forward<Args>(args))), ...);
  }
  template <typename... Args>
  constexpr void construct_at(size_type index, Args&&... args) {
    construct_at(index, indices, std::forward<Args>(args)...);
  }

  template <std::size_t... I>
  constexpr void destroy_at(size_type index, std::index_sequence<I...> /*unused*/) {
    ((std::destroy_at(address_of<I>(index))), ...);
  }
  constexpr void destroy_at(size_type index) { destroy_at(index, indices); }

  Storage storages_;
  size_type size_ = 0;
};

}  // namespace ctr
