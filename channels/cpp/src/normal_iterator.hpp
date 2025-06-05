#ifndef INCLUDE_SRC_NORMAL_ITERATOR_HPP_
#define INCLUDE_SRC_NORMAL_ITERATOR_HPP_

#include <iterator>

// This iterator adapter is @a normal in the sense that it does not
// change the semantics of any of the operators of its iterator
// parameter.  Its primary purpose is to convert an iterator that is
// not a class, e.g. a pointer, into an iterator that is a class.
// The _Container parameter exists solely so that different containers
// using this template can instantiate different types, even if the
// _Iterator parameter is the same.
template <typename Iterator, typename Container>
class NormalIterator {
 protected:
  Iterator current;

  using traits_type = std::iterator_traits<Iterator>;

  template <typename Iter>
  using convertible_from = std::enable_if_t<std::is_convertible_v<Iter, Iterator>>;

 public:
  using iterator_type = Iterator;
  using iterator_category = typename traits_type::iterator_category;
  using value_type = typename traits_type::value_type;
  using difference_type = typename traits_type::difference_type;
  using reference = typename traits_type::reference;
  using pointer = typename traits_type::pointer;

  using iterator_concept = std::__detail::__iter_concept<Iterator>;

  constexpr NormalIterator() noexcept : current(Iterator()) {}

  explicit constexpr NormalIterator(const Iterator& i) noexcept : current(i) {}

  // Allow iterator to const_iterator conversion
  template <typename Iter, typename = convertible_from<Iter>>
  constexpr explicit NormalIterator(const NormalIterator<Iter, Container>& i) noexcept : current(i.base()) {}

  // Forward iterator requirements
  constexpr auto operator*() const noexcept -> reference { return *current; }

  constexpr auto operator->() const noexcept -> pointer { return current; }

  constexpr auto operator++() noexcept -> NormalIterator& {
    ++current;
    return *this;
  }

  constexpr auto operator++(int) noexcept -> NormalIterator { return NormalIterator(current++); }

  // Bidirectional iterator requirements
  constexpr auto operator--() noexcept -> NormalIterator& {
    --current;
    return *this;
  }

  constexpr auto operator--(int) noexcept -> NormalIterator { return NormalIterator(current--); }

  // Random access iterator requirements
  constexpr auto operator[](difference_type n) const noexcept -> reference { return current[n]; }

  constexpr auto operator+=(difference_type n) noexcept -> NormalIterator& {
    current += n;
    return *this;
  }

  constexpr auto operator+(difference_type n) const noexcept -> NormalIterator { return NormalIterator(current + n); }

  constexpr auto operator-=(difference_type n) noexcept -> NormalIterator& {
    current -= n;
    return *this;
  }

  constexpr auto operator-(difference_type n) const noexcept -> NormalIterator { return NormalIterator(current - n); }

  constexpr auto base() const noexcept -> const Iterator& { return current; }
};

template <typename T>
concept convertiable_to_bool = std::convertible_to<T, bool>;

template <typename T>
concept boolean_testable = convertiable_to_bool<T> && requires(T&& t) {
  { !static_cast<T &&>(t) } -> convertiable_to_bool;
};

template <typename T, typename U>
[[nodiscard]] constexpr auto cmp(const T& t, const U& u) noexcept(_S_noexcept<T, U>())
  requires requires {
    { t < u } -> boolean_testable;
    { u < t } -> boolean_testable;
  }
{
  if constexpr (std::three_way_comparable_with<T, U>)
    return t <=> u;
  else {
    if (t < u) return std::weak_ordering::less;
    if (u < t) return std::weak_ordering::greater;

    return std::weak_ordering::equivalent;
  }
}
template <typename T, typename U = T>
using cmp_result_t = decltype(cmp(std::declval<T&>(), std::declval<U&>()));

template <typename IterL, typename IterR, typename Container>
[[nodiscard]] constexpr auto operator==(const NormalIterator<IterL, Container>& lhs,
                                        const NormalIterator<IterR, Container>& rhs) noexcept(noexcept(lhs.base() ==
                                                                                                       rhs.base()))
    -> bool
  requires requires {
    { lhs.base() == rhs.base() } -> std::convertible_to<bool>;
  }
{
  return lhs.base() == rhs.base();
}

template <typename IterL, typename IterR, typename Container>
[[nodiscard]] constexpr auto operator<=>(
    const NormalIterator<IterL, Container>& lhs,
    const NormalIterator<IterR, Container>& rhs) noexcept(noexcept(cmp(lhs.base(), rhs.base())))
    -> cmp_result_t<IterR, IterL> {
  return cmp(lhs.base(), rhs.base());
}

template <typename Iter, typename Container>
[[nodiscard]] constexpr auto operator==(const NormalIterator<Iter, Container>& lhs,
                                        const NormalIterator<Iter, Container>& rhs) noexcept(noexcept(lhs.base() ==
                                                                                                      rhs.base()))
    -> bool
  requires requires {
    { lhs.base() == rhs.base() } -> std::convertible_to<bool>;
  }
{
  return lhs.base() == rhs.base();
}

template <typename Iter, typename Container>
[[nodiscard]] constexpr auto operator<=>(const NormalIterator<Iter, Container>& lhs,
                                         const NormalIterator<Iter, Container>& rhs) noexcept(noexcept(cmp(lhs.base(),
                                                                                                           rhs.base())))
    -> cmp_result_t<Iter> {
  return cmp(lhs.base(), rhs.base());
}

template <typename IterL, typename IterR, typename Container>
[[nodiscard]] constexpr auto operator-(const NormalIterator<IterL, Container>& lhs,
                                       const NormalIterator<IterR, Container>& rhs) noexcept
    -> decltype(lhs.base() - rhs.base()) {
  return lhs.base() - rhs.base();
}

template <typename Iter, typename Container>
[[nodiscard]] constexpr auto operator-(const NormalIterator<Iter, Container>& lhs,
                                       const NormalIterator<Iter, Container>& rhs) noexcept ->
    typename NormalIterator<Iter, Container>::difference_type {
  return lhs.base() - rhs.base();
}

template <typename Iter, typename Container>
[[nodiscard]] constexpr auto operator+(typename NormalIterator<Iter, Container>::difference_type n,
                                       const NormalIterator<Iter, Container>& i) noexcept
    -> NormalIterator<Iter, Container> {
  return NormalIterator<Iter, Container>(i.base() + n);
}

#endif  // INCLUDE_SRC_NORMAL_ITERATOR_HPP_
