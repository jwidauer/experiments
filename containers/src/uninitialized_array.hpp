#ifndef INCLUDE_UNINITIALIZED_ARRAY_HPP_
#define INCLUDE_UNINITIALIZED_ARRAY_HPP_

#include <array>
#include <bit>
#include <cassert>
#include <optional>

#include "normal_iterator.hpp"
#include "type_traits.hpp"

template <class T, std::size_t N>
struct AlignedStorage {
 private:
  struct alignas(T) Data {
    std::array<std::byte, sizeof(T) * N> data;
  } data_;

 public:
  [[nodiscard]] constexpr auto data() -> Data* { return std::addressof(data_); }
  [[nodiscard]] constexpr auto data() const -> const Data* { return std::addressof(data_); }
};

template <typename T, std::size_t N>
class UninitializedArray {
 public:
  // NOLINTBEGIN(readability-identifier-naming)
  using value_type = T;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;
  using reference = value_type&;
  using const_reference = const value_type&;
  using pointer = value_type*;
  using const_pointer = const value_type*;
  using iterator = NormalIterator<T*, UninitializedArray>;
  using const_iterator = NormalIterator<const T*, UninitializedArray>;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;
  // NOLINTEND(readability-identifier-naming)

  constexpr auto at [[nodiscard]] (this auto& self, std::size_t idx)
  -> std::optional<NormalIterator<copy_const_t<decltype(self), T>*, UninitializedArray>> {
    return idx < N ? std::make_optional(self.begin() + idx) : std::nullopt;
  }

  template <typename Self>
  constexpr auto operator[] [[nodiscard]] (this Self& self, std::size_t idx) -> copy_const_t<Self, T>* {
    assert(idx < N && "Index out of bounds");
    return self.data() + idx;
  }

  constexpr auto begin [[nodiscard]] (this auto& self) -> decltype(auto) { return self.make_iter(self.data()); }
  constexpr auto cbegin [[nodiscard]] () const -> const_iterator { return data(); }

  constexpr auto end [[nodiscard]] (this auto& self) -> decltype(auto) { return self.make_iter(self.data() + N); }
  constexpr auto cend [[nodiscard]] () const -> const_iterator { return const_iterator{data() + N}; }

  constexpr auto rbegin [[nodiscard]] (this auto& self) -> decltype(auto) {
    return std::make_reverse_iterator(self.end());
  }
  constexpr auto crbegin [[nodiscard]] () const -> const_reverse_iterator { return std::make_reverse_iterator(cend()); }

  constexpr auto rend [[nodiscard]] (this auto& self) -> decltype(auto) {
    return std::make_reverse_iterator(self.begin());
  }
  constexpr auto crend [[nodiscard]] () const -> const_reverse_iterator { return std::make_reverse_iterator(cbegin()); }

  consteval auto size [[nodiscard]] () const -> std::size_t { return N; }

  template <typename Self>
  constexpr auto data [[nodiscard]] (this Self& self) -> copy_const_t<Self, T>* {
    if constexpr (is_sufficiently_trivial) {
      return self.storage_.data();
    } else {
      using ptr_t = copy_const_t<Self, T>*;
      return std::bit_cast<ptr_t>(self.storage_.data());
    }
  }

 private:
  template <typename Self, typename Iter>
  constexpr auto make_iter(this Self& /*self*/, Iter&& ptr) -> decltype(auto) {
    return make_normal_iterator<Self>(std::forward<Iter>(ptr));
  }

  static constexpr bool is_sufficiently_trivial =
      std::is_trivially_default_constructible_v<T> && std::is_trivially_destructible_v<T>;

  using storage_t = std::conditional_t<is_sufficiently_trivial, std::array<T, N>, AlignedStorage<T, N>>;

  // Allow storage of zero elements to not take up space
  [[no_unique_address]] storage_t storage_;
};

#endif  // INCLUDE_UNINITIALIZED_ARRAY_HPP_
