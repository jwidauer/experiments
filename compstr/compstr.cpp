#include <algorithm>
#include <array>
#include <limits>
#include <string_view>

#define CONSTEVAL_FN [[nodiscard]] consteval auto
#define CONSTEXPR_FN [[nodiscard]] constexpr auto

class SplitIter {
 public:
  using difference_type = std::ptrdiff_t;
  using value_type = std::string_view;
  using pointer = const char*;
  using reference = const char&;
  using iterator_category = std::forward_iterator_tag;

  constexpr SplitIter() = default;
  constexpr SplitIter(std::string_view sv, char needle)
      : sv_(sv), needle_(needle) {}

  constexpr auto operator++() -> SplitIter& {
    if (sv_.empty()) return *this;

    auto pos = sv_.find(needle_);
    if (pos == std::string_view::npos) {
      sv_ = {};
    } else {
      sv_ = sv_.substr(pos + 1);
    }
    return *this;
  }
  constexpr auto operator++(int) -> SplitIter {
    SplitIter tmp = *this;
    ++(*this);
    return tmp;
  }

  CONSTEXPR_FN operator*() const noexcept -> value_type {
    return sv_.substr(0, sv_.find(needle_));
  }

  CONSTEXPR_FN operator==(const SplitIter& other) const noexcept -> bool {
    return sv_ == other.sv_ && needle_ == other.needle_;
  }

 private:
  std::string_view sv_;
  char needle_;
};

static_assert(std::forward_iterator<SplitIter>);

template <std::size_t N>
class CompTimeString {
  static_assert(N > 0, "CompTimeString size must be greater than 0");
  static_assert(N < std::numeric_limits<std::size_t>::max() - 1,
                "CompTimeString size must be less than max size_t");

  consteval static auto calc_size(std::size_t cur, std::size_t pos,
                                  std::size_t len) -> std::size_t {
    if (pos >= cur) return 0;
    if (len == npos) return cur - pos;
    return std::min(cur - pos, len + 1);
  }

 public:
  using value_type = char;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;

  using reference = value_type&;
  using const_reference = const value_type&;
  using pointer = value_type*;
  using const_pointer = const value_type*;
  using iterator = std::array<char, N>::iterator;
  using const_iterator = std::array<char, N>::const_iterator;
  using reverse_iterator = std::array<char, N>::reverse_iterator;
  using const_reverse_iterator = std::array<char, N>::const_reverse_iterator;

  static constexpr size_type npos = std::numeric_limits<size_type>::max();

  consteval CompTimeString() noexcept = default;

  consteval CompTimeString(const char (&str)[N]) noexcept {
    std::copy_n(str, N, str_.begin());
  }

  CONSTEVAL_FN operator[](std::size_t index)->char& { return str_[index]; }
  CONSTEVAL_FN operator[](std::size_t index) const->char { return str_[index]; }
  CONSTEVAL_FN size() const noexcept -> std::size_t { return str_.size() - 1; }
  CONSTEVAL_FN empty() const noexcept -> bool { return str_.empty(); }

  CONSTEXPR_FN begin() noexcept -> iterator { return str_.begin(); }
  CONSTEXPR_FN begin() const noexcept -> const_iterator { return str_.begin(); }
  CONSTEXPR_FN cbegin() const noexcept -> const_iterator {
    return str_.cbegin();
  }
  CONSTEXPR_FN rbegin() const noexcept -> reverse_iterator {
    return str_.rbegin();
  }
  CONSTEXPR_FN crbegin() const noexcept -> const_reverse_iterator {
    return str_.crbegin();
  }

  CONSTEXPR_FN end() noexcept -> iterator { return str_.end(); }
  CONSTEXPR_FN end() const noexcept -> const_iterator { return str_.end(); }
  CONSTEXPR_FN cend() const noexcept -> const_iterator { return str_.cend(); }
  CONSTEXPR_FN rend() const noexcept -> reverse_iterator { return str_.rend(); }
  CONSTEXPR_FN crend() const noexcept -> const_reverse_iterator {
    return str_.crend();
  }

  CONSTEXPR_FN data() const noexcept -> const_pointer { return str_.data(); }

  CONSTEXPR_FN c_str() const noexcept -> const_pointer { return str_.data(); }

  template <std::size_t M>
  CONSTEVAL_FN append(const CompTimeString<M>& other) const noexcept
      -> CompTimeString<N + M - 1> {
    CompTimeString<N + M - 1> result;
    std::copy_n(cbegin(), size(), result.begin());
    std::copy_n(other.cbegin(), M, result.begin() + size());
    return result;
  }

  template <std::size_t M>
  CONSTEVAL_FN append(const char (&other)[M]) const noexcept
      -> CompTimeString<N + M - 1> {
    return append(CompTimeString<M>(other));
  }

  template <std::size_t pos, std::size_t len = npos>
  CONSTEVAL_FN substr() const noexcept
      -> CompTimeString<calc_size(N, pos, len)> {
    constexpr auto res_size = calc_size(N, pos, len);
    CompTimeString<res_size> result;
    std::copy_n(cbegin() + pos, result.size(), result.begin());
    result.str_.back() = '\0';  // Null-terminate the string
    return result;
  }

  CONSTEXPR_FN substr(std::size_t pos, std::size_t len = npos) const noexcept
      -> std::string_view {
    return std::string_view{c_str(), size()}.substr(pos, len);
  }

  CONSTEVAL_FN split(char needle) const noexcept -> SplitIter {
    return SplitIter{std::string_view{c_str(), size()}, needle};
  }

  consteval operator std::string_view() const noexcept {
    return std::string_view{c_str(), size()};
  }

 private:
  template <std::size_t M>
  friend class CompTimeString;

  std::array<char, N> str_;
};

template <std::size_t N, std::size_t M>
CONSTEVAL_FN operator+(const CompTimeString<N>& lhs,
                       const CompTimeString<M>& rhs) noexcept
    -> CompTimeString<N + M - 1> {
  return lhs.append(rhs);
}

template <std::size_t N, std::size_t M>
CONSTEVAL_FN operator+(const CompTimeString<N>& lhs,
                       const char (&rhs)[M]) noexcept
    -> CompTimeString<N + M - 1> {
  return lhs.append(rhs);
}

template <std::size_t N, std::size_t M>
CONSTEVAL_FN operator+(const char (&lhs)[N],
                       const CompTimeString<M>& rhs) noexcept
    -> CompTimeString<N + M - 1> {
  return CompTimeString<N>(lhs).append(rhs);
}

template <std::size_t N, std::size_t M>
CONSTEVAL_FN operator==(const CompTimeString<N>& lhs,
                        const CompTimeString<M>& rhs) noexcept -> bool {
  return std::ranges::equal(lhs, rhs);
}

template <std::size_t N, std::size_t M>
CONSTEXPR_FN operator==(const CompTimeString<N>& lhs,
                        const char (&rhs)[M]) noexcept -> bool {
  return std::ranges::equal(lhs, rhs);
}

int main() {
  constexpr CompTimeString str1("Hello");
  constexpr CompTimeString str2(" World");

  static_assert(str1.append(str2) == CompTimeString{"Hello World"});
  static_assert(str1.append(str2) == "Hello World");
  static_assert(str1 + str2 == "Hello World");
  static_assert(str1 + " World" == "Hello World");

  static_assert(str1.substr(0, 2) == "He");
  static_assert(str1.substr(2) == "llo");
  static_assert(str1.substr<0, 2>() == "He");

  static_assert([] {
    constexpr auto str = CompTimeString{"Hello World"};
    auto split = str.split(' ');
    auto first = *split;
    auto second = *(++split);
    return first == "Hello" && second == "World";
  }());

  return 0;
}
