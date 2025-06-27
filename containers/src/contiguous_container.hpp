#pragma once

#include <algorithm>
#include <optional>

#include "normal_iterator.hpp"

template <class Derived>
concept ContiguousDataHolder = requires(Derived& d) {
  typename Derived::value_type;
  typename Derived::size_type;
  typename Derived::difference_type;
  { d.data() } -> std::same_as<typename Derived::value_type*>;
  { d.data() } -> std::same_as<const typename Derived::value_type*>;
  { d.size() } -> std::convertible_to<typename Derived::size_type>;
};

template <class Derived>
  requires ContiguousDataHolder<Derived>
struct ContiguousContainer {
  // NOLINTBEGIN(readability-identifier-naming)
  using value_type = typename Derived::value_type;
  using size_type = typename Derived::size_type;
  using difference_type = typename Derived::difference_type;
  using reference = value_type&;
  using const_reference = const value_type&;
  using pointer = value_type*;
  using const_pointer = const value_type*;
  using iterator = NormalIterator<pointer, Derived>;
  using const_iterator = NormalIterator<const_pointer, Derived>;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;
  // NOLINTEND(readability-identifier-naming)

  // iterators
  constexpr auto begin [[nodiscard]] () noexcept -> iterator { return data(); }
  constexpr auto begin [[nodiscard]] () const noexcept -> const_iterator { return data(); }
  constexpr auto cbegin [[nodiscard]] () const noexcept -> const_iterator { return data(); }

  constexpr auto end [[nodiscard]] () noexcept -> iterator { return data() + sz(); }
  constexpr auto end [[nodiscard]] () const noexcept -> const_iterator { return data() + sz(); }
  constexpr auto cend [[nodiscard]] () const noexcept -> const_iterator { return data() + sz(); }

  constexpr auto rbegin [[nodiscard]] () noexcept -> reverse_iterator { return std::make_reverse_iterator(end()); }
  constexpr auto rbegin [[nodiscard]] () const noexcept -> const_reverse_iterator {
    return std::make_reverse_iterator(end());
  }
  constexpr auto crbegin [[nodiscard]] () const noexcept -> const_reverse_iterator {
    return std::make_reverse_iterator(cend());
  }

  constexpr auto rend [[nodiscard]] () noexcept -> reverse_iterator { return std::make_reverse_iterator(begin()); }
  constexpr auto rend [[nodiscard]] () const noexcept -> const_reverse_iterator {
    return std::make_reverse_iterator(begin());
  }
  constexpr auto crend [[nodiscard]] () const noexcept -> const_reverse_iterator {
    return std::make_reverse_iterator(cbegin());
  }

  // element access
  constexpr auto at [[nodiscard]] (size_type idx) -> std::optional<reference> {
    return idx < sz() ? std::make_optional(std::ref(*(data() + idx))) : std::nullopt;
  }
  constexpr auto at [[nodiscard]] (size_type idx) const -> std::optional<const_reference> {
    return idx < sz() ? std::make_optional(std::cref(*(data() + idx))) : std::nullopt;
  }

  constexpr auto operator[] [[nodiscard]] (size_type idx) -> reference {
    assert(idx < sz() && "Index out of bounds");
    return *(data() + idx);
  }
  constexpr auto operator[] [[nodiscard]] (size_type idx) const -> const_reference {
    assert(idx < sz() && "Index out of bounds");
    return *(data() + idx);
  }

  constexpr auto front [[nodiscard]] () -> reference {
    assert(!empty() && "Container is empty");
    return *begin();
  }
  constexpr auto front [[nodiscard]] () const -> const_reference {
    assert(!empty() && "Container is empty");
    return *begin();
  }

  constexpr auto back [[nodiscard]] () -> reference {
    assert(!empty() && "Container is empty");
    return *(end() - 1);
  }
  constexpr auto back [[nodiscard]] () const -> const_reference {
    assert(!empty() && "Container is empty");
    return *(end() - 1);
  }

  constexpr auto empty [[nodiscard]] () const noexcept -> bool { return sz() == 0; }

  constexpr auto contains [[nodiscard]] (const_reference value) const -> bool {
    return std::ranges::contains(*this, value);
  }

  constexpr auto find [[nodiscard]] (const_reference value) -> iterator { return std::ranges::find(*this, value); }
  constexpr auto find [[nodiscard]] (const_reference value) const -> const_iterator {
    return std::ranges::find(*this, value);
  }

 private:
  static_assert(std::is_base_of_v<ContiguousContainer<Derived>, Derived>,
                "Derived must inherit from ContiguousContainer<Derived>");

  constexpr auto to_derived [[nodiscard]] () noexcept -> Derived& { return static_cast<Derived&>(*this); }
  constexpr auto to_derived [[nodiscard]] () const noexcept -> const Derived& {
    return static_cast<const Derived&>(*this);
  }

  constexpr auto data() noexcept -> pointer { return to_derived().data(); }
  constexpr auto sz [[nodiscard]] () const noexcept -> size_type { return to_derived().size(); }
};
