#pragma once

#include <algorithm>
#include <cstddef>
#include <tl/optional.hpp>
#include <tuple>

#include "uninitialized_array.hpp"

template <std::size_t N, typename... Ts>
struct MultiObjectVector {
  static_assert(sizeof...(Ts) > 0, "At least one type must be provided");

  using storage_t = std::tuple<UninitializedArray<Ts, N>...>;

  template <std::size_t I>
  using storage_element_t = std::tuple_element_t<I, storage_t>;

  // NOLINTBEGIN(readability-identifier-naming)
  using size_type = std::size_t;
  // NOLINTEND(readability-identifier-naming)

  ~MultiObjectVector() { clear(); }

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
  [[nodiscard]] constexpr auto data(this Self& self) -> copy_const_t<Self, typename storage_element_t<I>::value_type>* {
    return std::get<I>(self.storages_).data();
  }

  template <typename T, typename Self>
  [[nodiscard]] constexpr auto data(this Self& self) -> copy_const_t<Self, T>* {
    return std::get<UninitializedArray<T, N>>(self.storages_).data();
  }

  template <std::size_t I, typename Self>
  [[nodiscard]] constexpr auto storage(this Self& self) -> copy_const_t<Self, storage_element_t<I>>& {
    return std::get<I>(self.storages_);
  }

  template <typename T, typename Self>
  [[nodiscard]] constexpr auto storage(this Self& self) -> copy_const_t<Self, UninitializedArray<T, N>>& {
    return std::get<UninitializedArray<T, N>>(self.storages_);
  }

  template <std::size_t I, typename Self>
  [[nodiscard]] constexpr auto at(this Self& self, size_type idx)
      -> tl::optional<copy_const_t<Self, typename storage_element_t<I>::value_type>&> {
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

    ((std::construct_at(address_of<Ts>(size_), values)), ...);
    ++size_;
    return size_ - 1;
  }

  constexpr auto try_insert(size_type index, const Ts&... values) -> tl::optional<size_type> {
    if (full() || index > size_) return tl::nullopt;

    // Move elements to make space
    ((std::ranges::move_backward(begin<Ts>() + index, end<Ts>(), end<Ts>() + 1)), ...);

    // Insert new elements
    ((std::construct_at(address_of<Ts>(index), values)), ...);
    ++size_;
    return index;
  }

  constexpr auto try_push_back(Ts&&... values) -> tl::optional<size_type> {
    if (full()) return tl::nullopt;

    ((std::construct_at(address_of<Ts>(size_), std::move(values))), ...);
    ++size_;
    return size_ - 1;
  }

  constexpr void try_pop_back() {
    if (empty()) return;

    --size_;
    ((std::destroy_at(address_of<Ts>(size_))), ...);
  }

  constexpr void clear() {
    while (!empty()) try_pop_back();
  }

  constexpr auto erase(size_type index) -> bool {
    if (index >= size_) return false;

    // Destroy elements at index
    ((std::destroy_at(address_of<Ts>(index))), ...);

    // Move elements to fill the gap
    ((std::ranges::move(begin<Ts>() + index + 1, end<Ts>(), begin<Ts>() + index)), ...);

    --size_;
    return true;
  }

 private:
  template <typename T, typename Self>
  [[nodiscard]] constexpr auto address_of(this Self& self, size_type idx) -> copy_const_t<Self, T>* {
    return std::addressof(self.template storage<T>()[idx]);
  }

  storage_t storages_;
  std::size_t size_ = 0;
};
