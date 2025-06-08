#ifndef INCLUDE_UNINITIALIZED_ARRAY_HPP_
#define INCLUDE_UNINITIALIZED_ARRAY_HPP_

#include <array>
#include <cassert>
#include <optional>

#include "normal_iterator.hpp"

namespace detail {

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

}  // namespace detail

template <std::size_t Len, std::size_t Align>
struct AlignedStorage {
  constexpr auto data(this auto& self) -> decltype(auto) { return self.data_.data(); }

 private:
  alignas(Align) std::array<std::byte, Len> data_;
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

  constexpr auto at [[nodiscard]] (this auto& self, std::size_t idx) -> decltype(auto) {
    return idx < N ? std::make_optional(self.begin() + idx) : std::nullopt;
  }

  constexpr auto operator[] [[nodiscard]] (this auto& self, std::size_t idx) -> decltype(auto) {
    assert(idx < N && "Index out of bounds");
    return *(self.data() + idx);
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
  constexpr auto data [[nodiscard]] (this auto& self) -> decltype(auto) {
    if constexpr (is_sufficiently_trivial) {
      return self.storage_.data();
    } else {
      using TPtr = detail::copy_const_t<decltype(self), T>*;
      return reinterpret_cast<TPtr>(self.storage_.data());
    }
  }

 private:
  template <typename Self, typename Iter>
  constexpr auto make_iter(this Self& /*self*/, Iter&& ptr) -> decltype(auto) {
    return make_normal_iterator<Self>(std::forward<Iter>(ptr));
  }

  static constexpr bool is_sufficiently_trivial =
      std::is_trivially_default_constructible_v<T> && std::is_trivially_destructible_v<T>;

  using Storage =
      std::conditional_t<is_sufficiently_trivial, std::array<T, N>, AlignedStorage<sizeof(T) * N, alignof(T)>>;

  // Allow storage of zero elements to not take up space
  [[no_unique_address]] Storage storage_;
};

#endif  // INCLUDE_UNINITIALIZED_ARRAY_HPP_
