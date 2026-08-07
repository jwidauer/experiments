#pragma once

#include <bits/c++config.h>
#include <bits/functexcept.h>
#include <bits/functional_hash.h>

#include <algorithm>  // For std::fill
#include <bit>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <cwctype>
#include <functional>
#include <iosfwd>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>

#define GLIBCXX_BITSET_BITS_PER_ULL (__CHAR_BIT__ * __SIZEOF_LONG_LONG__)

namespace ctr {

static constexpr std::size_t bits_per_word = __CHAR_BIT__ * sizeof(std::uint64_t);

constexpr auto bitset_words(std::size_t n) noexcept -> std::size_t {
  return (n / bits_per_word) + (n % bits_per_word == 0 ? 0 : 1);
}

/**
 *  Base class, general case.  It is a class invariant that _Nw will be
 *  nonnegative.
 *
 *  See documentation for bitset.
 */
template <size_t Nw>
struct BaseBitset {
  using WordT = std::uint64_t;

  /// 0 is the least significant word.
  WordT M_w[Nw];

  constexpr BaseBitset() noexcept : M_w() {}

  constexpr explicit BaseBitset(std::uint64_t val) noexcept
      : M_w{static_cast<WordT>(val), static_cast<WordT>(val >> bits_per_word)} {}

  static constexpr auto s_whichword(size_t pos) noexcept -> size_t { return pos / bits_per_word; }

  static constexpr auto s_whichbyte(size_t pos) noexcept -> size_t { return (pos % bits_per_word) / __CHAR_BIT__; }

  static constexpr auto s_whichbit(size_t pos) noexcept -> size_t { return pos % bits_per_word; }

  static constexpr auto s_maskbit(size_t pos) noexcept -> WordT { return (static_cast<WordT>(1)) << s_whichbit(pos); }

  constexpr auto m_getword(size_t pos) noexcept -> WordT& { return M_w[s_whichword(pos)]; }

  [[nodiscard]] constexpr auto m_getword(size_t pos) const noexcept -> WordT { return M_w[s_whichword(pos)]; }

  [[nodiscard]] constexpr auto m_getdata() const noexcept -> const WordT* { return M_w; }

  constexpr auto m_hiword() noexcept -> WordT& { return M_w[Nw - 1]; }

  [[nodiscard]] constexpr auto m_hiword() const noexcept -> WordT { return M_w[Nw - 1]; }

  constexpr void m_do_and(const BaseBitset<Nw>& x) noexcept {
    for (size_t i = 0; i < Nw; i++) M_w[i] &= x.M_w[i];
  }

  constexpr void m_do_or(const BaseBitset<Nw>& x) noexcept {
    for (size_t i = 0; i < Nw; i++) M_w[i] |= x.M_w[i];
  }

  constexpr void m_do_xor(const BaseBitset<Nw>& x) noexcept {
    for (size_t i = 0; i < Nw; i++) M_w[i] ^= x.M_w[i];
  }

  constexpr void m_do_left_shift(size_t shift) noexcept;

  constexpr void m_do_right_shift(size_t shift) noexcept;

  constexpr void m_do_flip() noexcept {
    for (size_t i = 0; i < Nw; i++) M_w[i] = ~M_w[i];
  }

  constexpr void m_do_set() noexcept {
    if (std::is_constant_evaluated()) {
      for (WordT& w : M_w) w = ~static_cast<WordT>(0);
      return;
    }
    std::memset(M_w, 0xFF, Nw * sizeof(WordT));
  }

  constexpr void m_do_reset() noexcept {
    if (std::is_constant_evaluated()) {
      for (WordT& w : M_w) w = 0;
      return;
    }
    std::memset(M_w, 0, Nw * sizeof(WordT));
  }

  constexpr auto m_is_equal(const BaseBitset<Nw>& x) const noexcept -> bool {
    if (std::is_constant_evaluated()) {
      for (size_t i = 0; i < Nw; ++i)
        if (M_w[i] != x.M_w[i]) return false;
      return true;
    }
    return !std::memcmp(M_w, x.M_w, Nw * sizeof(WordT));
  }

  template <size_t Nb>
  [[nodiscard]] constexpr auto m_are_all() const noexcept -> bool {
    for (size_t i = 0; i < Nw - 1; i++)
      if (M_w[i] != ~static_cast<WordT>(0)) return false;
    return m_hiword() == (~static_cast<WordT>(0) >> ((Nw * bits_per_word) - Nb));
  }

  [[nodiscard]] constexpr auto m_is_any() const noexcept -> bool {
    for (size_t i = 0; i < Nw; i++)
      if (M_w[i] != static_cast<WordT>(0)) return true;
    return false;
  }

  [[nodiscard]] constexpr auto m_do_count() const noexcept -> size_t {
    size_t result = 0;
    for (size_t i = 0; i < Nw; i++) result += __builtin_popcountl(M_w[i]);
    return result;
  }

  [[nodiscard]] constexpr auto m_do_to_ulong() const -> std::uint64_t;

  [[nodiscard]] constexpr auto m_do_to_ullong() const -> std::uint64_t;

  // find first "on" bit
  [[nodiscard]] constexpr auto m_do_find_first(size_t /*__not_found*/) const noexcept -> size_t;

  // find the next "on" bit that follows "prev"
  [[nodiscard]] constexpr auto m_do_find_next(size_t /*__prev*/, size_t /*__not_found*/) const noexcept -> size_t;
};

// Definitions of non-inline functions from _Base_bitset.
template <size_t Nw>
constexpr void BaseBitset<Nw>::m_do_left_shift(size_t shift) noexcept {
  if (__builtin_expect(static_cast<std::int64_t>(shift != 0), 1) != 0) {
    const size_t wshift = shift / bits_per_word;
    const size_t offset = shift % bits_per_word;

    if (offset == 0)
      for (size_t n = Nw - 1; n >= wshift; --n) M_w[n] = M_w[n - wshift];
    else {
      const size_t sub_offset = (bits_per_word - offset);
      for (size_t n = Nw - 1; n > wshift; --n)
        M_w[n] = ((M_w[n - wshift] << offset) | (M_w[n - wshift - 1] >> sub_offset));
      M_w[wshift] = M_w[0] << offset;
    }

    std::fill(M_w + 0, M_w + wshift, static_cast<WordT>(0));
  }
}

template <size_t Nw>
constexpr void BaseBitset<Nw>::m_do_right_shift(size_t shift) noexcept {
  if (__builtin_expect(static_cast<std::int64_t>(shift != 0), 1) != 0) {
    const size_t wshift = shift / bits_per_word;
    const size_t offset = shift % bits_per_word;
    const size_t limit = Nw - wshift - 1;

    if (offset == 0)
      for (size_t n = 0; n <= limit; ++n) M_w[n] = M_w[n + wshift];
    else {
      const size_t sub_offset = (bits_per_word - offset);
      for (size_t n = 0; n < limit; ++n) M_w[n] = ((M_w[n + wshift] >> offset) | (M_w[n + wshift + 1] << sub_offset));
      M_w[limit] = M_w[Nw - 1] >> offset;
    }

    std::fill(M_w + limit + 1, M_w + Nw, static_cast<WordT>(0));
  }
}

template <size_t Nw>
constexpr auto BaseBitset<Nw>::m_do_to_ulong() const -> std::uint64_t {
  for (size_t i = 1; i < Nw; ++i)
    if (M_w[i]) throw std::overflow_error("_Base_bitset::_M_do_to_ulong");
  return M_w[0];
}

template <size_t Nw>
constexpr auto BaseBitset<Nw>::m_do_to_ullong() const -> std::uint64_t {
  return m_do_to_ulong();
}

template <size_t Nw>
constexpr auto BaseBitset<Nw>::m_do_find_first(size_t not_found) const noexcept -> size_t {
  for (size_t i = 0; i < Nw; i++) {
    WordT thisword = M_w[i];
    if (thisword != static_cast<WordT>(0)) return ((i * bits_per_word) + std::countr_zero(thisword));
  }
  // not found, so return an indication of failure.
  return not_found;
}

template <size_t Nw>
constexpr auto BaseBitset<Nw>::m_do_find_next(size_t prev, size_t not_found) const noexcept -> size_t {
  // make bound inclusive
  ++prev;

  // check out of bounds
  if (prev >= Nw * bits_per_word) return not_found;

  // search first word
  size_t i = s_whichword(prev);
  WordT thisword = M_w[i];

  // mask off bits below bound
  thisword &= (~static_cast<WordT>(0)) << s_whichbit(prev);

  if (thisword != static_cast<WordT>(0)) return ((i * bits_per_word) + std::countr_zero(thisword));

  // check subsequent words
  i++;
  for (; i < Nw; i++) {
    thisword = M_w[i];
    if (thisword != static_cast<WordT>(0)) return ((i * bits_per_word) + std::countr_zero(thisword));
  }
  // not found, so return an indication of failure.
  return not_found;
}  // end _M_do_find_next

/**
 *  Base class, specialization for a single word.
 *
 *  See documentation for bitset.
 */
template <>
struct BaseBitset<1> {
  using WordT = std::uint64_t;
  WordT M_w;

  constexpr BaseBitset() noexcept : M_w(0) {}

  constexpr explicit BaseBitset(std::uint64_t val) noexcept : M_w(val) {}

  static constexpr auto s_whichword(size_t pos) noexcept -> size_t { return pos / bits_per_word; }

  static constexpr auto s_whichbyte(size_t pos) noexcept -> size_t { return (pos % bits_per_word) / __CHAR_BIT__; }

  static constexpr auto s_whichbit(size_t pos) noexcept -> size_t { return pos % bits_per_word; }

  static constexpr auto s_maskbit(size_t pos) noexcept -> WordT { return (static_cast<WordT>(1)) << s_whichbit(pos); }

  constexpr auto m_getword(size_t /*unused*/) noexcept -> WordT& { return M_w; }

  [[nodiscard]] constexpr auto m_getword(size_t /*unused*/) const noexcept -> WordT { return M_w; }

  [[nodiscard]] constexpr auto m_getdata() const noexcept -> const WordT* { return &M_w; }

  constexpr auto m_hiword() noexcept -> WordT& { return M_w; }

  [[nodiscard]] constexpr auto m_hiword() const noexcept -> WordT { return M_w; }

  constexpr void m_do_and(const BaseBitset<1>& x) noexcept { M_w &= x.M_w; }

  constexpr void m_do_or(const BaseBitset<1>& x) noexcept { M_w |= x.M_w; }

  constexpr void m_do_xor(const BaseBitset<1>& x) noexcept { M_w ^= x.M_w; }

  constexpr void m_do_left_shift(size_t shift) noexcept { M_w <<= shift; }

  constexpr void m_do_right_shift(size_t shift) noexcept { M_w >>= shift; }

  constexpr void m_do_flip() noexcept { M_w = ~M_w; }

  constexpr void m_do_set() noexcept { M_w = ~static_cast<WordT>(0); }

  constexpr void m_do_reset() noexcept { M_w = 0; }

  [[nodiscard]] constexpr auto m_is_equal(const BaseBitset<1>& x) const noexcept -> bool { return M_w == x.M_w; }

  template <size_t Nb>
  [[nodiscard]] constexpr auto m_are_all() const noexcept -> bool {
    return M_w == (~static_cast<WordT>(0) >> (bits_per_word - Nb));
  }

  [[nodiscard]] constexpr auto m_is_any() const noexcept -> bool { return M_w != 0; }

  [[nodiscard]] constexpr auto m_do_count() const noexcept -> size_t { return __builtin_popcountl(M_w); }

  [[nodiscard]] constexpr auto m_do_to_ulong() const noexcept -> std::uint64_t { return M_w; }

  [[nodiscard]] constexpr auto m_do_to_ullong() const noexcept -> std::uint64_t { return M_w; }

  [[nodiscard]] constexpr auto m_do_find_first(size_t not_found) const noexcept -> size_t {
    if (M_w != 0) return std::countr_zero(M_w);
    return not_found;
  }

  // find the next "on" bit that follows "prev"
  [[nodiscard]] constexpr auto m_do_find_next(size_t prev, size_t not_found) const noexcept -> size_t {
    ++prev;
    if (prev >= static_cast<size_t>(bits_per_word)) return not_found;

    WordT const x = M_w >> prev;
    if (x != 0) return std::countr_zero(x) + prev;
    return not_found;
  }
};

/**
 *  Base class, specialization for no storage (zero-length %bitset).
 *
 *  See documentation for bitset.
 */
template <>
struct BaseBitset<0> {
  using WordT = std::uint64_t;

  constexpr BaseBitset() noexcept = default;

  constexpr explicit BaseBitset(std::uint64_t /*unused*/) noexcept {}

  static constexpr auto s_whichword(size_t pos) noexcept -> size_t { return pos / bits_per_word; }

  static constexpr auto s_whichbyte(size_t pos) noexcept -> size_t { return (pos % bits_per_word) / __CHAR_BIT__; }

  static constexpr auto s_whichbit(size_t pos) noexcept -> size_t { return pos % bits_per_word; }

  static constexpr auto s_maskbit(size_t pos) noexcept -> WordT { return (static_cast<WordT>(1)) << s_whichbit(pos); }

  // This would normally give access to the data.  The bounds-checking
  // in the bitset class will prevent the user from getting this far,
  // but this must fail if the user calls _Unchecked_set directly.
  // Let's not penalize zero-length users unless they actually
  // make an unchecked call; all the memory ugliness is therefore
  // localized to this single should-never-get-this-far function.
  [[noreturn]] auto m_getword(size_t /*unused*/) noexcept -> WordT& {
    throw std::out_of_range("_Base_bitset::_M_getword");
  }

  [[nodiscard]] constexpr auto m_getword(size_t /*unused*/) const noexcept -> WordT { return 0; }

  [[nodiscard]] constexpr auto m_hiword() const noexcept -> WordT { return 0; }

  constexpr void m_do_and(const BaseBitset<0>& /*unused*/) noexcept {}

  constexpr void m_do_or(const BaseBitset<0>& /*unused*/) noexcept {}

  constexpr void m_do_xor(const BaseBitset<0>& /*unused*/) noexcept {}

  constexpr void m_do_left_shift(size_t /*unused*/) noexcept {}

  constexpr void m_do_right_shift(size_t /*unused*/) noexcept {}

  constexpr void m_do_flip() noexcept {}

  constexpr void m_do_set() noexcept {}

  constexpr void m_do_reset() noexcept {}

  // Are all empty bitsets equal to each other?  Are they equal to
  // themselves?  How to compare a thing which has no state?  What is
  // the sound of one zero-length bitset clapping?
  [[nodiscard]] constexpr auto m_is_equal(const BaseBitset<0>& /*unused*/) const noexcept -> bool { return true; }

  template <size_t Nb>
  [[nodiscard]] constexpr auto m_are_all() const noexcept -> bool {
    return true;
  }

  [[nodiscard]] constexpr auto m_is_any() const noexcept -> bool { return false; }

  [[nodiscard]] constexpr auto m_do_count() const noexcept -> size_t { return 0; }

  [[nodiscard]] constexpr auto m_do_to_ulong() const noexcept -> std::uint64_t { return 0; }

  [[nodiscard]] constexpr auto m_do_to_ullong() const noexcept -> std::uint64_t { return 0; }

  // Normally "not found" is the size, but that could also be
  // misinterpreted as an index in this corner case.  Oh well.
  [[nodiscard]] constexpr auto m_do_find_first(size_t /*unused*/) const noexcept -> size_t { return 0; }

  [[nodiscard]] constexpr auto m_do_find_next(size_t /*unused*/, size_t /*unused*/) const noexcept -> size_t {
    return 0;
  }
};

// Helper class to zero out the unused high-order bits in the highest word.
template <size_t Extrabits>
struct Sanitize {
  using WordT = std::uint64_t;

  static constexpr void s_do_sanitize(WordT& val) noexcept { val &= ~((~static_cast<WordT>(0)) << Extrabits); }
};

template <>
struct Sanitize<0> {
  using WordT = std::uint64_t;

  static constexpr void s_do_sanitize(WordT /*unused*/) noexcept {}
};

template <size_t Nb, bool = (Nb < GLIBCXX_BITSET_BITS_PER_ULL)>
struct SanitizeVal {
  static constexpr auto s_do_sanitize_val(std::uint64_t val) -> std::uint64_t { return val; }
};

template <size_t Nb>
struct SanitizeVal<Nb, true> {
  static constexpr auto s_do_sanitize_val(std::uint64_t val) -> std::uint64_t {
    return val & ~((~static_cast<std::uint64_t>(0)) << Nb);
  }
};

namespace bitset {
template <typename CharT>
using String = std::basic_string_view<CharT>;
}  // namespace bitset

/**
 *  @brief The %bitset class represents a @e fixed-size sequence of bits.
 *  @ingroup utilities
 *
 *  (Note that %bitset does @e not meet the formal requirements of a
 *  <a href="tables.html#65">container</a>.  Mainly, it lacks iterators.)
 *
 *  The template argument, `Nb`, may be any non-negative number,
 *  specifying the number of bits (e.g., "0", "12", "1024*1024").
 *
 *  In the general unoptimized case, storage is allocated in word-sized
 *  blocks.  Let B be the number of bits in a word, then (Nb+(B-1))/B
 *  words will be used for storage.  B - Nb%B bits are unused.  (They are
 *  the high-order bits in the highest word.)  It is a class invariant
 *  that those unused bits are always zero.
 *
 *  If you think of %bitset as <em>a simple array of bits</em>, be
 *  aware that your mental picture is reversed: a %bitset behaves
 *  the same way as bits in integers do, with the bit at index 0 in
 *  the <em>least significant / right-hand</em> position, and the bit at
 *  index Nb-1 in the <em>most significant / left-hand</em> position.
 *  Thus, unlike other containers, a %bitset's index <em>counts from
 *  right to left</em>, to put it very loosely.
 *
 *  This behavior is preserved when translating to and from strings.  For
 *  example, the first line of the following program probably prints
 *  <em>b(&apos;a&apos;) is 0001100001</em> on a modern ASCII system.
 *
 *  @code
 *     #include <bitset>
 *     #include <iostream>
 *     #include <sstream>
 *
 *     using namespace std;
 *
 *     int main()
 *     {
 *         long         a = 'a';
 *         bitset<10>   b(a);
 *
 *         cout << "b('a') is " << b << endl;
 *
 *         ostringstream s;
 *         s << b;
 *         string  str = s.str();
 *         cout << "index 3 in the string is " << str[3] << " but\n"
 *              << "index 3 in the bitset is " << b[3] << endl;
 *     }
 *  @endcode
 *
 *  Also see:
 *  https://gcc.gnu.org/onlinedocs/libstdc++/manual/ext_containers.html
 *  for a description of extensions.
 *
 *  Most of the actual code isn't contained in %bitset<> itself, but in the
 *  base class _Base_bitset.  The base class works with whole words, not with
 *  individual bits.  This allows us to specialize _Base_bitset for the
 *  important special case where the %bitset is only a single word.
 *
 *  Extra confusion can result due to the fact that the storage for
 *  _Base_bitset @e is a regular array, and is indexed as such.  This is
 *  carefully encapsulated.
 */
template <size_t Nb>
class Bitset : private BaseBitset<bitset_words(Nb)> {
 private:
  using Base = BaseBitset<bitset_words(Nb)>;
  using WordT = std::uint64_t;

  template <class Str>
  constexpr void m_check_initial_position(const Str& s, typename Str::size_type position) const {
    if (position > s.size())
      std::__throw_out_of_range_fmt(__N("bitset::bitset:"
                                        " __position (which is %zu) > __s.size() (which is %zu)"),
                                    size_t(position), size_t(s.size()));
  }

  constexpr void m_check(size_t position, const char* s) const {
    if (position >= Nb)
      std::__throw_out_of_range_fmt(__N("%s: __position (which is %zu) >= _Nb (which is %zu)"), s, position, Nb);
  }

  constexpr void m_do_sanitize() noexcept {
    using SanitizeType = Sanitize<Nb % bits_per_word>;
    SanitizeType::_S_do_sanitize(this->m_hiword());
  }

  friend struct std::hash<Bitset>;

 public:
  /**
   *  This encapsulates the concept of a single bit.  An instance of this
   *  class is a proxy for an actual bit; this way the individual bit
   *  operations are done as faster word-size bitwise instructions.
   *
   *  Most users will never need to use this class directly; conversions
   *  to and from bool are automatic and should be transparent.  Overloaded
   *  operators help to preserve the illusion.
   *
   *  (On a typical system, this <em>bit %reference</em> is 64
   *  times the size of an actual bit.  Ha.)
   */
  class Reference {
    friend class Bitset;

    WordT* M_wp_;
    size_t M_bpos_;

    constexpr Reference(Bitset& b, size_t pos) noexcept {
      M_wp_ = &b.m_getword(pos);
      M_bpos_ = Base::s_whichbit(pos);
    }

   public:
    Reference(const Reference&) = default;

    constexpr ~Reference() noexcept = default;

    // For b[i] = __x;
    constexpr auto operator=(bool x) noexcept -> Reference& {
      if (x)
        *M_wp_ |= Base::s_maskbit(M_bpos_);
      else
        *M_wp_ &= ~Base::s_maskbit(M_bpos_);
      return *this;
    }

    // _GLIBCXX_RESOLVE_LIB_DEFECTS
    // 4187. bitset::reference should be const-assignable
    constexpr auto operator=(bool x) const noexcept -> const Reference& {
      if (x)
        *M_wp_ |= Base::s_maskbit(M_bpos_);
      else
        *M_wp_ &= ~Base::s_maskbit(M_bpos_);
      return *this;
    }

    // For b[i] = b[__j];
    constexpr auto operator=(const Reference& j) noexcept -> Reference& {
      if ((*(j.M_wp_) & Base::s_maskbit(j.M_bpos_)))
        *M_wp_ |= Base::s_maskbit(M_bpos_);
      else
        *M_wp_ &= ~Base::s_maskbit(M_bpos_);
      return *this;
    }

    // Flips the bit
    constexpr auto operator~() const noexcept -> bool { return (*(M_wp_)&Base::s_maskbit(M_bpos_)) == 0; }

    // For __x = b[i];
    constexpr explicit operator bool() const noexcept { return (*(M_wp_)&Base::s_maskbit(M_bpos_)) != 0; }

    // For b[i].flip();
    constexpr auto flip() noexcept -> Reference& {
      *M_wp_ ^= Base::s_maskbit(M_bpos_);
      return *this;
    }

    constexpr friend void swap(Reference x, Reference y) noexcept {
      bool tmp = x;
      x = y;
      y = tmp;
    }

    constexpr friend void swap(Reference x, bool& y) noexcept {
      bool tmp = x;
      x = y;
      y = tmp;
    }

    constexpr friend void swap(bool& x, Reference y) noexcept {
      bool tmp = x;
      x = y;
      y = tmp;
    }
  };
  friend class Reference;

  // 23.3.5.1 constructors:
  /// All bits set to zero.
  constexpr Bitset() noexcept = default;

  /// Initial bits bitwise-copied from a single word (others set to zero).
  constexpr explicit Bitset(std::uint64_t val) noexcept : Base(SanitizeVal<Nb>::_S_do_sanitize_val(val)) {}

  /**
   *  Use a subset of a string.
   *  @param  __s  A string of `0` and `1` characters.
   *  @param  __position  Index of the first character in `__s` to use;
   *                    defaults to zero.
   *  @throw  std::out_of_range  If `__position > __s.size()`.
   *  @throw  std::invalid_argument  If a character appears in the string
   *                                 which is neither `0` nor `1`.
   */
  template <class CharT, class Traits, class Alloc>
  constexpr explicit Bitset(const std::basic_string<CharT, Traits, Alloc>& s, size_t position = 0) : Base() {
    _M_check_initial_position(s, position);
    _M_copy_from_string(s, position, std::basic_string<CharT, Traits, Alloc>::npos, CharT('0'), CharT('1'));
  }

  /**
   *  Use a subset of a string.
   *  @param  __s  A string of `0` and `1` characters.
   *  @param  __position  Index of the first character in `__s` to use.
   *  @param  __n    The number of characters to copy.
   *  @throw  std::out_of_range  If `__position > __s.size()`.
   *  @throw  std::invalid_argument  If a character appears in the string
   *                                 which is neither `0` nor `1`.
   */
  template <class CharT, class Traits, class Alloc>
  constexpr Bitset(const std::basic_string<CharT, Traits, Alloc>& s, size_t position, size_t n) : Base() {
    _M_check_initial_position(s, position);
    _M_copy_from_string(s, position, n, CharT('0'), CharT('1'));
  }

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 396. what are characters zero and one.
  template <class CharT, class Traits, class Alloc>
  constexpr Bitset(const std::basic_string<CharT, Traits, Alloc>& s, size_t position, size_t n, CharT zero,
                   CharT one = CharT('1'))
      : Base() {
    _M_check_initial_position(s, position);
    _M_copy_from_string(s, position, n, zero, one);
  }

  /**
   *  Use a subset of a string view.
   *  @param  __s  A `string_view` of a sequence of `0` and `1` characters.
   *  @param  __position  Index of the first character in `__s` to use.
   *  @param  __n    The maximum number of characters from `__s` to use.
   *  @param  __zero The character corresponding to the value 0.
   *  @param  __one  The character corresponding to the value 1.
   *  @throw  std::out_of_range  If `__position > __s.size()`.
   *  @throw  std::invalid_argument  If a character appears in `__s`
   *                                 which is neither `0` nor `1`.
   */
  template <class CharT, class Traits>
  constexpr explicit Bitset(
      std::basic_string_view<CharT, Traits> s, std::basic_string_view<CharT, Traits>::size_type position = 0,
      std::basic_string_view<CharT, Traits>::size_type n = std::basic_string_view<CharT, Traits>::npos,
      CharT zero = CharT('0'), CharT one = CharT('1'))
      : Base() {
    _M_check_initial_position(s, position);
    _M_copy_from_ptr<CharT, Traits>(s.data(), s.size(), position, n, zero, one);
  }

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 4294. bitset(const CharT*) constructor needs to be constrained
  /**
   *  Construct from a character %array.
   *  @param  __str  An %array of characters `__zero` and `__one`.
   *  @param  __n    The number of characters to use.
   *  @param  __zero The character corresponding to the value 0.
   *  @param  __one  The character corresponding to the value 1.
   *  @throw  std::invalid_argument If a character appears in the string
   *                                which is neither `__zero` nor `__one`.
   */
  template <typename CharT>
    requires(std::is_trivially_copyable_v<CharT> && std::is_standard_layout_v<CharT> &&
             std::is_trivially_default_constructible_v<CharT> && std::negation_v<std::is_array<CharT>>)
  [[__gnu__::__nonnull__]]
  constexpr explicit Bitset(const CharT* str, typename bitset::String<CharT>::size_type n = bitset::String<CharT>::npos,
                            CharT zero = CharT('0'), CharT one = CharT('1'))
      : Base() {
    if (!str) std::__throw_logic_error(__N("bitset::bitset(const _CharT*, ...)"));
    using Traits = typename bitset::String<CharT>::traits_type;

    if (n == bitset::String<CharT>::npos) n = Traits::length(str);
    _M_copy_from_ptr<CharT, Traits>(str, n, 0, n, zero, one);
  }

  // 23.3.5.2 bitset operations:
  ///@{
  /**
   *  Operations on bitsets.
   *  @param  __rhs  A same-sized bitset.
   *
   *  These should be self-explanatory.
   */
  constexpr auto operator&=(const Bitset<Nb>& rhs) noexcept -> Bitset<Nb>& {
    this->_M_do_and(rhs);
    return *this;
  }

  constexpr auto operator|=(const Bitset<Nb>& rhs) noexcept -> Bitset<Nb>& {
    this->_M_do_or(rhs);
    return *this;
  }

  constexpr auto operator^=(const Bitset<Nb>& rhs) noexcept -> Bitset<Nb>& {
    this->_M_do_xor(rhs);
    return *this;
  }
  ///@}

  ///@{
  /**
   *  Operations on bitsets.
   *  @param  __position  The number of places to shift.
   *
   *  These should be self-explanatory.
   */
  constexpr auto operator<<=(size_t position) noexcept -> Bitset<Nb>& {
    if (__builtin_expect(static_cast<std::int64_t>(position < Nb), 1) != 0) {
      this->_M_do_left_shift(position);
      this->_M_do_sanitize();
    } else
      this->_M_do_reset();
    return *this;
  }

  constexpr auto operator>>=(size_t position) noexcept -> Bitset<Nb>& {
    if (__builtin_expect(static_cast<std::int64_t>(position < Nb), 1) != 0)
      this->_M_do_right_shift(position);
    else
      this->_M_do_reset();
    return *this;
  }
  ///@}

  ///@{
  /**
   *  These versions of single-bit set, reset, flip, and test are
   *  extensions from the SGI version.  They do no range checking.
   *  @ingroup SGIextensions
   */
  constexpr auto unchecked_set(size_t pos) noexcept -> Bitset<Nb>& {
    this->_M_getword(pos) |= Base::_S_maskbit(pos);
    return *this;
  }

  constexpr auto unchecked_set(size_t pos, int val) noexcept -> Bitset<Nb>& {
    if (val != 0)
      this->_M_getword(pos) |= Base::_S_maskbit(pos);
    else
      this->_M_getword(pos) &= ~Base::_S_maskbit(pos);
    return *this;
  }

  constexpr auto unchecked_reset(size_t pos) noexcept -> Bitset<Nb>& {
    this->_M_getword(pos) &= ~Base::_S_maskbit(pos);
    return *this;
  }

  constexpr auto unchecked_flip(size_t pos) noexcept -> Bitset<Nb>& {
    this->_M_getword(pos) ^= Base::_S_maskbit(pos);
    return *this;
  }

  [[nodiscard]] constexpr auto unchecked_test(size_t pos) const noexcept -> bool {
    return ((this->_M_getword(pos) & Base::_S_maskbit(pos)) != static_cast<WordT>(0));
  }
  ///@}

  // Set, reset, and flip.
  /**
   *  @brief Sets every bit to true.
   */
  constexpr auto set() noexcept -> Bitset<Nb>& {
    this->_M_do_set();
    this->_M_do_sanitize();
    return *this;
  }

  /**
   *  @brief Sets a given bit to a particular value.
   *  @param  __position  The index of the bit.
   *  @param  __val  Either true or false, defaults to true.
   *  @throw  std::out_of_range  If @a pos is bigger the size of the %set.
   */
  constexpr auto set(size_t position, bool val = true) -> Bitset<Nb>& {
    this->_M_check(position, __N("bitset::set"));
    return unchecked_set(position, val);
  }

  /**
   *  @brief Sets every bit to false.
   */
  constexpr auto reset() noexcept -> Bitset<Nb>& {
    this->_M_do_reset();
    return *this;
  }

  /**
   *  @brief Sets a given bit to false.
   *  @param  __position  The index of the bit.
   *  @throw  std::out_of_range  If @a pos is bigger the size of the %set.
   *
   *  Same as writing @c set(pos,false).
   */
  constexpr auto reset(size_t position) -> Bitset<Nb>& {
    this->_M_check(position, __N("bitset::reset"));
    return unchecked_reset(position);
  }

  /**
   *  @brief Toggles every bit to its opposite value.
   */
  constexpr auto flip() noexcept -> Bitset<Nb>& {
    this->_M_do_flip();
    this->_M_do_sanitize();
    return *this;
  }

  /**
   *  @brief Toggles a given bit to its opposite value.
   *  @param  __position  The index of the bit.
   *  @throw  std::out_of_range  If @a pos is bigger the size of the %set.
   */
  constexpr auto flip(size_t position) -> Bitset<Nb>& {
    this->_M_check(position, __N("bitset::flip"));
    return unchecked_flip(position);
  }

  /// See the no-argument flip().
  constexpr auto operator~() const noexcept -> Bitset<Nb> { return Bitset<Nb>(*this).flip(); }

  ///@{
  /**
   *  @brief  Array-indexing support.
   *  @param  __position  Index into the %bitset.
   *  @return A bool for a <em>const %bitset</em>.  For non-const
   *           bitsets, an instance of the reference proxy class.
   *  @note  These operators do no range checking and throw no exceptions,
   *         as required by DR 11 to the standard.
   *
   *  _GLIBCXX_RESOLVE_LIB_DEFECTS Note that this implementation already
   *  resolves DR 11 (items 1 and 2), but does not do the range-checking
   *  required by that DR's resolution.  -pme
   *  The DR has since been changed:  range-checking is a precondition
   *  (users' responsibility), and these functions must not throw.  -pme
   */
  constexpr auto operator[](size_t position) -> Reference {
    assert(position < Nb);
    return reference(*this, position);
  }

  constexpr auto operator[](size_t position) const -> bool {
    assert(position < Nb);
    return unchecked_test(position);
  }
  ///@}

  /**
   *  @brief Returns a numerical interpretation of the %bitset.
   *  @return  The integral equivalent of the bits.
   *  @throw  std::overflow_error  If there are too many bits to be
   *                               represented in an @c unsigned @c long.
   */
  [[nodiscard]] constexpr auto to_ulong() const -> std::uint64_t { return this->_M_do_to_ulong(); }

  [[nodiscard]] constexpr auto to_ullong() const -> std::uint64_t { return this->_M_do_to_ullong(); }

  /**
   *  @brief Returns a character interpretation of the %bitset.
   *  @return  The string equivalent of the bits.
   *
   *  Note the ordering of the bits:  decreasing character positions
   *  correspond to increasing bit positions (see the main class notes for
   *  an example).
   */
  template <class CharT, class Traits, class Alloc>
  constexpr auto to_string() const -> std::basic_string<CharT, Traits, Alloc> {
    std::basic_string<CharT, Traits, Alloc> result;
    _M_copy_to_string(result, CharT('0'), CharT('1'));
    return result;
  }

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 396. what are characters zero and one.
  template <class CharT, class Traits, class Alloc>
  constexpr auto to_string(CharT zero, CharT one = CharT('1')) const -> std::basic_string<CharT, Traits, Alloc> {
    std::basic_string<CharT, Traits, Alloc> result;
    _M_copy_to_string(result, zero, one);
    return result;
  }

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 434. bitset::to_string() hard to use.
  template <class CharT, class Traits>
  constexpr auto to_string() const -> std::basic_string<CharT, Traits, std::allocator<CharT>> {
    return to_string<CharT, Traits, std::allocator<CharT>>();
  }

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 853. to_string needs updating with zero and one.
  template <class CharT, class Traits>
  constexpr auto to_string(CharT zero, CharT one = CharT('1')) const
      -> std::basic_string<CharT, Traits, std::allocator<CharT>> {
    return to_string<CharT, Traits, std::allocator<CharT>>(zero, one);
  }

  template <class CharT>
  constexpr auto to_string() const -> std::basic_string<CharT, std::char_traits<CharT>, std::allocator<CharT>> {
    return to_string<CharT, std::char_traits<CharT>, std::allocator<CharT>>();
  }

  template <class CharT>
  constexpr auto to_string(CharT zero, CharT one = CharT('1')) const
      -> std::basic_string<CharT, std::char_traits<CharT>, std::allocator<CharT>> {
    return to_string<CharT, std::char_traits<CharT>, std::allocator<CharT>>(zero, one);
  }

  [[nodiscard]] constexpr auto to_string() const
      -> std::basic_string<char, std::char_traits<char>, std::allocator<char>> {
    return to_string<char, std::char_traits<char>, std::allocator<char>>();
  }

  [[nodiscard]] constexpr auto to_string(char zero, char one = '1') const
      -> std::basic_string<char, std::char_traits<char>, std::allocator<char>> {
    return to_string<char, std::char_traits<char>, std::allocator<char>>(zero, one);
  }

  /// Returns the number of bits which are set.
  [[nodiscard]] constexpr auto count() const noexcept -> size_t { return this->_M_do_count(); }

  /// Returns the total number of bits.
  [[nodiscard]] constexpr auto size() const noexcept -> size_t { return Nb; }

  ///@{
  /// These comparisons for equality/inequality are, well, @e bitwise.
  constexpr auto operator==(const Bitset<Nb>& rhs) const noexcept -> bool { return this->_M_is_equal(rhs); }

  constexpr auto operator!=(const Bitset<Nb>& rhs) const noexcept -> bool { return !this->_M_is_equal(rhs); }
  ///@}

  /**
   *  @brief Tests the value of a bit.
   *  @param  __position  The index of a bit.
   *  @return  The value at @a pos.
   *  @throw  std::out_of_range  If @a pos is bigger the size of the %set.
   */
  [[nodiscard]] constexpr auto test(size_t position) const -> bool {
    this->_M_check(position, __N("bitset::test"));
    return unchecked_test(position);
  }

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // DR 693. std::bitset::all() missing.
  /**
   *  @brief Tests whether all the bits are on.
   *  @return  True if all the bits are set.
   */
  [[nodiscard]] constexpr auto all() const noexcept -> bool { return this->template _M_are_all<Nb>(); }

  /**
   *  @brief Tests whether any of the bits are on.
   *  @return  True if at least one bit is set.
   */
  [[nodiscard]] constexpr auto any() const noexcept -> bool { return this->_M_is_any(); }

  /**
   *  @brief Tests whether any of the bits are on.
   *  @return  True if none of the bits are set.
   */
  [[nodiscard]] constexpr auto none() const noexcept -> bool { return !this->_M_is_any(); }

  ///@{
  /// Self-explanatory.
  constexpr auto operator<<(size_t position) const noexcept -> Bitset<Nb> { return Bitset<Nb>(*this) <<= position; }

  constexpr auto operator>>(size_t position) const noexcept -> Bitset<Nb> { return Bitset<Nb>(*this) >>= position; }
  ///@}

  /**
   *  @brief  Finds the index of the first "on" bit.
   *  @return  The index of the first bit set, or size() if not found.
   *  @ingroup SGIextensions
   *  @sa  _Find_next
   */
  [[nodiscard]] constexpr auto find_first() const noexcept -> size_t { return this->_M_do_find_first(Nb); }

  /**
   *  @brief  Finds the index of the next "on" bit after prev.
   *  @return  The index of the next bit set, or size() if not found.
   *  @param  __prev  Where to start searching.
   *  @ingroup SGIextensions
   *  @sa  _Find_first
   */
  [[nodiscard]] constexpr auto find_next(size_t prev) const noexcept -> size_t {
    return this->_M_do_find_next(prev, Nb);
  }

 private:
  // Helper functions for string operations.
  template <class CharT, class Traits>
  constexpr void m_copy_from_ptr(const CharT*, size_t, size_t, size_t, CharT, CharT);

  template <class CharT, class Traits, class Alloc>
  constexpr void m_copy_from_string(const std::basic_string<CharT, Traits, Alloc>& s, size_t pos, size_t n, CharT zero,
                                    CharT one) {
    _M_copy_from_ptr<CharT, Traits>(s.data(), s.size(), pos, n, zero, one);
  }

  template <class CharT, class Traits, class Alloc>
  constexpr void m_copy_to_string(std::basic_string<CharT, Traits, Alloc>& /*__s*/, CharT /*__zero*/,
                                  CharT /*__one*/) const;

  template <class CharT, class Traits, size_t Nb2>
  friend auto operator>>(std::basic_istream<CharT, Traits>&, Bitset<Nb2>&) -> std::basic_istream<CharT, Traits>&;

  template <class CharT, class Traits, size_t Nb2>
  friend auto operator<<(std::basic_ostream<CharT, Traits>&, const Bitset<Nb2>&) -> std::basic_ostream<CharT, Traits>&;
};

// Definitions of non-inline member functions.
template <size_t Nb>
template <class CharT, class Traits>
constexpr void Bitset<Nb>::m_copy_from_ptr(const CharT* s, size_t len, size_t pos, size_t n, CharT zero, CharT one) {
  reset();
  const size_t rlen = std::min(n, (len - pos));
  const size_t nbits = std::min(Nb, rlen);
  for (size_t i = rlen - nbits; i > 0; --i) {
    const CharT c = s[pos + rlen - i];
    if (!Traits::eq(c, zero) && !Traits::eq(c, one)) std::__throw_invalid_argument(__N("bitset::_M_copy_from_ptr"));
  }
  for (size_t i = nbits; i > 0; --i) {
    const CharT c = s[pos + nbits - i];
    if (Traits::eq(c, zero))
      ;
    else if (Traits::eq(c, one))
      unchecked_set(i - 1);
    else
      std::__throw_invalid_argument(__N("bitset::_M_copy_from_ptr"));
  }
}

template <size_t Nb>
template <class CharT, class Traits, class Alloc>
constexpr void Bitset<Nb>::m_copy_to_string(std::basic_string<CharT, Traits, Alloc>& s, CharT zero, CharT one) const {
  s.assign(Nb, zero);
  size_t n = this->find_first();
  while (n < Nb) {
    s[Nb - n - 1] = one;
    n = find_next(n);
  }
}

// 23.3.5.3 bitset operations:
///@{
/**
 *  @brief  Global bitwise operations on bitsets.
 *  @param  __x  A bitset.
 *  @param  __y  A bitset of the same size as @a __x.
 *  @return  A new bitset.
 *
 *  These should be self-explanatory.
 */
template <size_t Nb>
constexpr auto operator&(const Bitset<Nb>& x, const Bitset<Nb>& y) noexcept -> Bitset<Nb> {
  Bitset<Nb> result(x);
  result &= y;
  return result;
}

template <size_t Nb>
constexpr auto operator|(const Bitset<Nb>& x, const Bitset<Nb>& y) noexcept -> Bitset<Nb> {
  Bitset<Nb> result(x);
  result |= y;
  return result;
}

template <size_t Nb>
constexpr auto operator^(const Bitset<Nb>& x, const Bitset<Nb>& y) noexcept -> Bitset<Nb> {
  Bitset<Nb> result(x);
  result ^= y;
  return result;
}

}  // namespace ctr

#undef _GLIBCXX_BITSET_BITS_PER_ULL

namespace std {

// DR 1182.
/// std::hash specialization for bitset.
template <size_t Nb>
struct hash<ctr::Bitset<Nb>> : public __hash_base<size_t, ctr::Bitset<Nb>> {
  auto operator()(const ctr::Bitset<Nb>& b) const noexcept -> size_t {
    const size_t clength = (Nb + __CHAR_BIT__ - 1) / __CHAR_BIT__;
    return std::_Hash_impl::hash(b.m_getdata(), clength);
  }
};

template <>
struct hash<ctr::Bitset<0>> : public __hash_base<size_t, ctr::Bitset<0>> {
  auto operator()(const ctr::Bitset<0>& /*unused*/) const noexcept -> size_t { return 0; }
};

}  // namespace std
