#pragma once

#include <array>
#include <cassert>
#include <cstddef>
#include <iterator>
#include <optional>

#include "maybe_uninit.hpp"
#include "normal_iterator.hpp"
#include "type_traits.hpp"

namespace ctr {

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

  template <typename Self>
  constexpr auto at [[nodiscard]] (this Self& self, size_type idx)
      -> std::optional<NormalIterator<copy_const_t<Self, T>*, UninitializedArray>> {
    return idx < N ? std::make_optional(self.begin() + idx) : std::nullopt;
  }

  template <typename Self>
  constexpr auto operator[] [[nodiscard]] (this Self& self, size_type idx) -> copy_const_t<Self, T>* {
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

  consteval auto size [[nodiscard]] () const -> size_type { return N; }

  template <typename Self>
  constexpr auto data [[nodiscard]] (this Self& self) -> copy_const_t<Self, T>* {
    static_assert(N != 0, "Cannot call data() on an UninitializedArray of size 0");
    return self.storage_.data()->data();
  }

 private:
  template <typename Self, typename Ptr>
  constexpr auto make_iter(this Self& /*self*/, Ptr&& ptr) -> decltype(auto) {
    return make_normal_iterator<Self>(std::forward<Ptr>(ptr));
  }

  // Allow storage of zero elements to not take up space
  [[no_unique_address]] std::array<MaybeUninit<T>, N> storage_;
};

}  // namespace ctr
