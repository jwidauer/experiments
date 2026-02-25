#pragma once

#include <algorithm>
#include <cassert>
#include <cstddef>

#include "tl/optional.hpp"
#include "uninitialized_array.hpp"

namespace ctr {

template <typename T, std::size_t N>
struct StaticVector {
  // NOLINTBEGIN(readability-identifier-naming)
  using value_type = T;
  using size_type = SmallestTypeHoldingT<N>;
  using difference_type = std::make_signed_t<size_type>;
  using reference = value_type&;
  using const_reference = const value_type&;
  using pointer = value_type*;
  using const_pointer = const value_type*;
  using iterator = pointer;
  using const_iterator = const_pointer;
  // NOLINTEND(readability-identifier-naming)

  constexpr StaticVector() = default;

  constexpr ~StaticVector()
    requires(std::is_trivially_destructible_v<T>)
  = default;

  constexpr ~StaticVector()
    requires(!std::is_trivially_destructible_v<T>)
  {
    clear();
  }

  [[nodiscard]] constexpr auto begin() -> T* { return data_.data(); }
  [[nodiscard]] constexpr auto begin() const -> const T* { return data_.data(); }
  [[nodiscard]] constexpr auto cbegin() const -> const T* { return data_.data(); }

  [[nodiscard]] constexpr auto end() -> T* { return data_.data() + size_; }
  [[nodiscard]] constexpr auto end() const -> const T* { return data_.data() + size_; }
  [[nodiscard]] constexpr auto cend() const -> const T* { return data_.data() + size_; }

  constexpr auto try_push_back(const T& value) -> tl::optional<T&> {
    if (full()) return tl::nullopt;  // Vector is full
    return *std::construct_at(data_[size_++], value);
  }
  constexpr auto try_push_back(T&& value) -> tl::optional<T&> {
    if (full()) return tl::nullopt;  // Vector is full
    return *std::construct_at(data_[size_++], std::move(value));
  }

  template <typename... Args>
  constexpr auto try_emplace_back(Args&&... args) -> tl::optional<T&> {
    if (full()) return tl::nullopt;  // Vector is full
    return *std::construct_at(data_[size_++], std::forward<Args>(args)...);
  }

  constexpr void pop_back() {
    if (empty()) return;  // Vector is empty

    std::destroy_at(data_[size_ - 1]);
    --size_;
  }

  constexpr auto try_insert(std::size_t index, const T& value) -> tl::optional<T&> {
    if (full() || index > size_) return tl::nullopt;  // Vector is full or index is out of bounds

    // Move elements to make space
    std::ranges::move_backward(data_[index], end(), end() + 1);

    // Insert new element
    ++size_;
    return *std::construct_at(data_[index], value);
  }

  constexpr auto try_insert(std::size_t index, T&& value) -> tl::optional<T&> {
    if (full() || index > size_) return tl::nullopt;  // Vector is full or index is out of bounds

    // Move elements to make space
    std::ranges::move_backward(data_[index], end(), end() + 1);

    // Insert new element
    ++size_;
    return *std::construct_at(data_[index], std::move(value));
  }

  constexpr auto try_insert(iterator pos, const T& value) -> tl::optional<T&> {
    return try_insert(distance(begin(), pos), value);
  }
  constexpr auto try_insert(iterator pos, T&& value) -> tl::optional<T&> {
    return try_insert(distance(begin(), pos), std::move(value));
  }

  constexpr auto try_erase(std::size_t index) -> bool {
    if (empty() || !is_valid_index(index)) return false;  // Vector is empty or index is out of bounds

    // Destroy the element at the specified index
    std::destroy_at(data_[index]);

    // Move elements to fill the gap
    std::ranges::move(data_[index + 1], end(), data_[index]);

    --size_;
    return true;
  }

  constexpr auto try_erase(iterator pos) -> bool { return try_erase(distance(begin(), pos)); }

  [[nodiscard]] constexpr auto at(std::size_t index) -> tl::optional<T&> {
    if (!is_valid_index(index)) return tl::nullopt;
    return *data_[index];
  }
  [[nodiscard]] constexpr auto at(std::size_t index) const -> tl::optional<const T&> {
    if (!is_valid_index(index)) return tl::nullopt;
    return *data_[index];
  }

  [[nodiscard]] constexpr auto operator[](std::size_t index) -> T& {
    assert(is_valid_index(index) && "Index out of bounds");
    return data_[index];
  }
  [[nodiscard]] constexpr auto operator[](std::size_t index) const -> const T& {
    assert(is_valid_index(index) && "Index out of bounds");
    return data_[index];
  }

  constexpr void clear() {
    for (std::size_t i = 0; i < size_; ++i) std::destroy_at(data_[i]);
    size_ = 0;
  }

  [[nodiscard]] constexpr auto size() const -> std::size_t { return size_; }

  [[nodiscard]] constexpr auto capacity() const -> std::size_t { return N; }

  [[nodiscard]] constexpr auto empty() const -> bool { return size_ == 0; }
  [[nodiscard]] constexpr auto full() const -> bool { return size_ == N; }

 private:
  [[nodiscard]] constexpr auto distance(iterator first, iterator second) const -> size_type {
    return static_cast<size_type>(std::distance(first, second));
  }
  [[nodiscard]] constexpr auto is_valid_index(std::size_t index) const -> bool { return index < size_; }
  UninitializedArray<T, N> data_;
  size_type size_{};
};

}  // namespace ctr
