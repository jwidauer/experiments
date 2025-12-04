// <format> Formatting -*- C++ -*-

// Copyright The GNU Toolchain Authors.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file include/format
 *  This is a Standard C++ Library header.
 */

#pragma once

#include <bits/requires_hosted.h>  // for std::string

#include <cstdint>
#include <utility>

#define __glibcxx_want_format
#define __glibcxx_want_format_ranges
#define __glibcxx_want_format_uchar

#include <bits/ranges_algobase.h>
#include <bits/ranges_base.h>   // input_range, range_reference_t
#include <bits/ranges_util.h>   // subrange
#include <bits/stl_iterator.h>  // back_insert_iterator
#include <bits/stl_pair.h>      // __is_pair
#include <bits/unicode.h>       // __is_scalar_value, _Utf_view, etc.
#include <bits/utility.h>       // tuple_size_v
#include <bits/version.h>

#include <algorithm>  // ranges::copy
#include <array>
#include <cassert>
#include <charconv>
#include <concepts>
#include <ext/numeric_traits.h>  // __int_traits
#include <limits>
#include <locale>
#include <span>
#include <string>
#include <string_view>
#include <variant>  // monostate
#include <vector>

#if !__has_builtin(__builtin_toupper)
#include <cctype>
#endif

namespace std _GLIBCXX_VISIBILITY(default) {

// [format.context], class template basic_format_context
template <typename Out, typename CharT>
class BasicFormatContext;

// [format.fmt.string], class template basic_format_string
template <typename CharT, typename... Args>
struct BasicFormatString;

/// @cond undocumented
namespace detail {
// Type-erased character sink.
template <typename CharT>
class Sink;

// Output iterator that writes to a type-erase character sink.
template <typename CharT>
class SinkIter;

template <typename CharT>
using format_context = BasicFormatContext<SinkIter<CharT>, CharT>;

template <typename CharT>
struct RuntimeFormatString {
  [[__gnu__::__always_inline__]]
  explicit RuntimeFormatString(basic_string_view<CharT> s) noexcept
      : str_(s) {}

  RuntimeFormatString(const RuntimeFormatString&) = delete;
  void operator=(const RuntimeFormatString&) = delete;

 private:
  basic_string_view<CharT> str_;

  template <typename, typename...>
  friend struct std::BasicFormatString;
};

}  // namespace detail
/// @endcond

using format_context = detail::format_context<char>;

// [format.args], class template basic_format_args
template <typename Context>
class BasicFormatArgs;
using format_args = BasicFormatArgs<format_context>;

// [format.arguments], arguments
// [format.arg], class template basic_format_arg
template <typename Context>
class BasicFormatArg;

/** A compile-time checked format string for the specified argument types.
 *
 * @since C++23 but available as an extension in C++20.
 */
template <typename CharT, typename... Args>
struct BasicFormatString {
  template <typename Tp>
    requires convertible_to<const Tp&, basic_string_view<CharT>>
  consteval explicit BasicFormatString(const Tp& s);

  [[__gnu__::__always_inline__]]
  explicit BasicFormatString(detail::RuntimeFormatString<CharT> s) noexcept
      : str_(s._M_str) {}

  [[__gnu__::__always_inline__]]
  constexpr auto get() const noexcept -> basic_string_view<CharT> {
    return str_;
  }

 private:
  basic_string_view<CharT> str_;
};

template <typename... Args>
using format_string = BasicFormatString<char, type_identity_t<Args>...>;

// [format.formatter], formatter

/// The primary template of std::formatter is disabled.
template <typename Tp, typename CharT = char>
struct Formatter {
  Formatter() = delete;  // No std::formatter specialization for this type.
  Formatter(const Formatter&) = delete;
  auto operator=(const Formatter&) -> Formatter& = delete;
};

// [format.error], class format_error
class FormatError : public runtime_error {
 public:
  explicit FormatError(const string& what) : runtime_error(what) {}
  explicit FormatError(const char* what) : runtime_error(what) {}
};

/// @cond undocumented
[[noreturn]]
inline void throw_format_error(const char* what) {
  _GLIBCXX_THROW_OR_ABORT(FormatError(what));
}

namespace detail {
// XXX use named functions for each constexpr error?

[[noreturn]]
inline void unmatched_left_brace_in_format_string() {
  throw_format_error("format error: unmatched '{' in format string");
}

[[noreturn]]
inline void unmatched_right_brace_in_format_string() {
  throw_format_error("format error: unmatched '}' in format string");
}

[[noreturn]]
inline void conflicting_indexing_in_format_string() {
  throw_format_error("format error: conflicting indexing style in format string");
}

[[noreturn]]
inline void invalid_arg_id_in_format_string() {
  throw_format_error("format error: invalid arg-id in format string");
}

[[noreturn]]
inline void failed_to_parse_format_spec() {
  throw_format_error("format error: failed to parse format-spec");
}

}  // namespace detail
/// @endcond

// [format.parse.ctx], class template basic_format_parse_context
template <typename CharT>
class BasicFormatParseContext;
using format_parse_context = BasicFormatParseContext<char>;

template <typename CharT>
class BasicFormatParseContext {
 public:
  using char_type = CharT;
  using const_iterator = typename basic_string_view<CharT>::const_iterator;
  using iterator = const_iterator;

  constexpr explicit BasicFormatParseContext(basic_string_view<CharT> fmt, size_t num_args = 0) noexcept
      : begin_(fmt.begin()), end_(fmt.end()), num_args_(num_args) {}

  BasicFormatParseContext(const BasicFormatParseContext&) = delete;
  void operator=(const BasicFormatParseContext&) = delete;

  [[nodiscard]] constexpr auto begin() const noexcept -> const_iterator { return begin_; }
  [[nodiscard]] constexpr auto end() const noexcept -> const_iterator { return end_; }

  constexpr void advance_to(const_iterator it) noexcept { begin_ = it; }

  constexpr auto next_arg_id() -> size_t {
    if (indexing_ == Indexing::Manual) detail::conflicting_indexing_in_format_string();
    indexing_ = Indexing::Auto;

    // _GLIBCXX_RESOLVE_LIB_DEFECTS
    // 3825. Missing compile-time argument id check in next_arg_id
    if (std::is_constant_evaluated())
      if (next_arg_id_ == num_args_) detail::invalid_arg_id_in_format_string();
    return next_arg_id_++;
  }

  constexpr void check_arg_id(size_t id) {
    if (indexing_ == Indexing::Auto) detail::conflicting_indexing_in_format_string();
    indexing_ = Indexing::Manual;

    if (std::is_constant_evaluated())
      if (id >= num_args_) detail::invalid_arg_id_in_format_string();
  }

 private:
  iterator begin_;
  iterator end_;
  enum class Indexing : std::uint8_t { Unknown, Manual, Auto };
  Indexing indexing_ = Indexing::Unknown;
  size_t next_arg_id_ = 0;
  size_t num_args_;
};

/// @cond undocumented
template <typename Tp, template <typename...> class Class>
static constexpr bool is_specialization_of = false;
template <template <typename...> class Class, typename... Args>
static constexpr bool is_specialization_of<Class<Args...>, Class> = true;

namespace detail {
// pre: first != last
template <typename CharT>
constexpr auto parse_integer(const CharT* first, const CharT* last) -> pair<uint16_t, const CharT*> {
  if (first == last) __builtin_unreachable();

  if constexpr (is_same_v<CharT, char>) {
    const auto start = first;
    uint16_t val = 0;
    // N.B. std::from_chars is not constexpr in C++20.
    if (__detail::__from_chars_alnum<true>(first, last, val, 10) && first != start) [[likely]]
      return {val, first};
  } else {
    constexpr int n = 32;
    std::array<char, n> buf{};
    for (int i = 0; i < n && (first + i) != last; ++i) buf[i] = first[i];
    auto [v, ptr] = detail::parse_integer(buf.data(), buf.data() + n);
    if (ptr) [[likely]]
      return {v, first + (ptr - buf.data())};
  }
  return {0, nullptr};
}

template <typename CharT>
constexpr auto parse_arg_id(const CharT* first, const CharT* last) -> pair<uint16_t, const CharT*> {
  if (first == last) std::unreachable();

  if (*first == '0') return {0, first + 1};  // No leading zeros allowed, so '0...' == 0

  if ('1' <= *first && *first <= '9') {
    const uint16_t id = *first - '0';
    const auto next = first + 1;
    // Optimize for most likely case of single digit arg-id.
    if (next == last || !('0' <= *next && *next <= '9')) return {id, next};

    return detail::parse_integer(first, last);
  }
  return {0, nullptr};
}

// NOLINTBEGIN(readability-identifier-naming)
enum class PresType : std::uint8_t {
  None = 0,  // Default type (not valid for integer presentation types).
  // Presentation types for integral types (including bool and charT).
  d = 1,
  b,
  B,
  o,
  x,
  X,
  c,
  // Presentation types for floating-point types.
  a = 1,
  A,
  e,
  E,
  f,
  F,
  g,
  G,
  p = 0,
  P,          // For pointers.
  s = 0,      // For strings and bool.
  Esc = 0xf,  // For strings and charT.
};
// NOLINTEND(readability-identifier-naming)

enum class Align : std::uint8_t {
  Default,
  Left,
  Right,
  Centre,
};

enum class Sign : std::uint8_t {
  Default,
  Plus,
  Minus,  // XXX does this need to be distinct from _Sign_default?
  Space,
};

enum class WidthPrec : std::uint8_t {
  None,    // No width/prec specified.
  Value,   // Fixed width/prec specified.
  FromArg  // Use a formatting argument for width/prec.
};

template <typename Context>
auto int_from_arg(const BasicFormatArg<Context>& arg) -> size_t;

constexpr auto is_digit(char c) -> bool { return '0' <= c && c <= '9'; }

constexpr auto is_xdigit(char c) -> bool { return is_digit(c) || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F'); }

template <typename CharT>
struct Spec {
  Align align : 2;
  Sign sign : 2;
  unsigned alt : 1;
  unsigned localized : 1;
  unsigned zero_fill : 1;
  WidthPrec width_kind : 2;
  WidthPrec prec_kind : 2;
  PresType type : 4;
  unsigned reserved : 1;
  unsigned reserved2 : 16;
  uint16_t width;
  uint16_t prec;
  char32_t fill = ' ';

  using iterator = typename basic_string_view<CharT>::iterator;

  static constexpr auto parse_align(CharT c) noexcept -> Align {
    switch (c) {
      case '<':
        return Align::Left;
      case '>':
        return Align::Right;
      case '^':
        return Align::Centre;
      default:
        return Align::Default;
    }
  }

  // pre: __first != __last
  constexpr auto parse_fill_and_align(iterator first, iterator last) noexcept -> iterator {
    if (*first != '{') {
      if constexpr (__unicode::__literal_encoding_is_unicode<CharT>()) {
        // Accept any UCS scalar value as fill character.
        __unicode::_Utf32_view<ranges::subrange<iterator>> uv({first, last});
        if (!uv.empty()) {
          auto beg = uv.begin();
          char32_t c = *beg++;
          if (__unicode::__is_scalar_value(c)) {
            if (const auto* next = beg.base(); next != last)
              if (Align alignment = parse_align(*next); alignment != Align::Default) {
                fill = c;
                align = alignment;
                return ++next;
              }
          }
        }
      } else if (last - first >= 2) {
        if (Align alignment = parse_align(first[1]); alignment != Align::Default) {
          fill = *first;
          align = alignment;
          return first + 2;
        }
      }

      if (Align alignment = parse_align(first[0]); alignment != Align::Default) {
        fill = ' ';
        align = alignment;
        return first + 1;
      }
    }
    return first;
  }

  static constexpr auto parse_sign(CharT c) noexcept -> Sign {
    switch (c) {
      case '+':
        return Sign::Plus;
      case '-':
        return Sign::Minus;
      case ' ':
        return Sign::Space;
      default:
        return Sign::Default;
    }
  }

  // pre: __first != __last
  constexpr auto parse_sign(iterator first, iterator /*unused*/) noexcept -> iterator {
    if (Sign psign = parse_sign(*first); psign != Sign::Default) {
      sign = psign;
      return first + 1;
    }
    return first;
  }

  // pre: *__first is valid
  constexpr auto parse_alternate_form(iterator first, iterator /*unused*/) noexcept -> iterator {
    if (*first == '#') {
      alt = true;
      ++first;
    }
    return first;
  }

  // pre: __first != __last
  constexpr auto parse_zero_fill(iterator first, iterator /* __last */) noexcept -> iterator {
    if (*first == '0') {
      zero_fill = true;
      ++first;
    }
    return first;
  }

  // pre: __first != __last
  static constexpr auto parse_width_or_precision(iterator first, iterator last, uint16_t& val, bool& arg_id,
                                                 BasicFormatParseContext<CharT>& pc) -> iterator {
    if (detail::is_digit(*first)) {
      auto [v, ptr] = detail::parse_integer(first, last);
      if (!ptr) throw_format_error("format error: invalid width or precision in format-spec");

      first = ptr;
      val = v;
    } else if (*first == '{') {
      arg_id = true;
      ++first;
      if (first == last) detail::unmatched_left_brace_in_format_string();
      if (*first == '}') {
        val = pc.next_arg_id();
      } else {
        auto [v, ptr] = detail::parse_arg_id(first, last);
        if (ptr == nullptr || ptr == last || *ptr != '}') detail::invalid_arg_id_in_format_string();
        first = ptr;
        pc.check_arg_id(v);
        val = v;
      }
      ++first;  // past the '}'
    }
    return first;
  }

  // pre: __first != __last
  constexpr auto parse_width(iterator first, iterator last, BasicFormatParseContext<CharT>& pc) -> iterator {
    bool arg_id = false;
    if (*first == '0') throw_format_error("format error: width must be non-zero in format string");

    auto next = parse_width_or_precision(first, last, width, arg_id, pc);
    if (next != first) width_kind = arg_id ? WidthPrec::FromArg : WidthPrec::Value;
    return next;
  }

  // pre: __first != __last
  constexpr auto parse_precision(iterator first, iterator last, BasicFormatParseContext<CharT>& pc) -> iterator {
    if (first[0] != '.') return first;

    iterator next = ++first;
    bool arg_id = false;
    if (next != last) next = parse_width_or_precision(first, last, prec, arg_id, pc);
    if (next == first) throw_format_error("format error: missing precision after '.' in format string");

    prec_kind = arg_id ? WidthPrec::FromArg : WidthPrec::Value;
    return next;
  }

  // pre: __first != __last
  constexpr auto parse_locale(iterator first, iterator /* __last */) noexcept -> iterator {
    if (*first == 'L') {
      localized = true;
      ++first;
    }
    return first;
  }

  template <typename Context>
  auto get_width(Context& ctx) const -> size_t {
    switch (width_kind) {
      case WidthPrec::Value:
        return width;
      case WidthPrec::FromArg:
        return detail::int_from_arg(ctx.arg(width));
      case WidthPrec::None:
        return 0;
    }
  }

  template <typename Context>
  auto get_precision(Context& ctx) const -> size_t {
    switch (prec_kind) {
      case WidthPrec::Value:
        return prec;
      case WidthPrec::FromArg:
        return detail::int_from_arg(ctx.arg(prec));
      case WidthPrec::None:
        return numeric_limits<size_t>::max();
    }
  }
};

template <typename Int>
inline auto put_sign(Int i, Sign sign, char* dest) noexcept -> char* {
  if (i < 0)
    *dest = '-';
  else if (sign == Sign::Plus)
    *dest = '+';
  else if (sign == Sign::Space)
    *dest = ' ';
  else
    ++dest;
  return dest;
}

// Write STR to OUT (and do so efficiently if OUT is a SinkIter).
template <typename Out, typename CharT>
  requires output_iterator<Out, const CharT&>
inline auto write(Out out, basic_string_view<CharT> str) -> Out {
  if constexpr (is_same_v<Out, SinkIter<CharT>>) {
    if (!str.empty()) out = str;
  } else {
    for (CharT c : str) *out++ = c;
  }
  return out;
}

// Write STR to OUT with NFILL copies of FILL_CHAR specified by ALIGN.
// pre: __align != _Align_default
template <typename Out, typename CharT>
auto write_padded(Out out, basic_string_view<CharT> str, Align align, size_t nfill, char32_t fill_char) -> Out {
  const size_t buflen = 0x20;
  std::array<CharT, buflen> padding_chars;
  padding_chars[0] = CharT();
  basic_string_view<CharT> padding{padding_chars.data(), padding_chars.size()};

  auto pad = [&padding](size_t n, Out& o) -> auto {
    if (n == 0) return;
    while (n > padding.size()) {
      o = detail::write(std::move(o), padding);
      n -= padding.size();
    }
    if (n != 0) o = detail::write(std::move(o), padding.substr(0, n));
  };

  size_t l;
  size_t r;
  size_t max;
  if (align == Align::Centre) {
    l = nfill / 2;
    r = l + (nfill & 1);
    max = r;
  } else if (align == Align::Right) {
    l = nfill;
    r = 0;
    max = l;
  } else {
    l = 0;
    r = nfill;
    max = r;
  }

  if (max < buflen)
    padding.remove_suffix(buflen - max);
  else
    max = buflen;

  char_traits<CharT>::assign(padding_chars.data(), max, fill_char);
  pad(l, out);
  out = detail::write(std::move(out), str);
  pad(r, out);

  return out;
}

// Write STR to OUT, with alignment and padding as determined by SPEC.
// pre: __spec._M_align != _Align_default || __align != _Align_default
template <typename CharT, typename Out>
auto write_padded_as_spec(basic_string_view<type_identity_t<CharT>> str, size_t estimated_width,
                          BasicFormatContext<Out, CharT>& fc, const Spec<CharT>& spec, Align alignement = Align::Left)
    -> Out {
  size_t width = spec.get_width(fc);

  if (width <= estimated_width) return detail::write(fc.out(), str);

  const size_t nfill = width - estimated_width;

  if (spec.align != Align::Default) alignement = spec.align;

  return detail::write_padded(fc.out(), str, alignement, nfill, spec.fill);
}

// A lightweight optional<locale>.
struct OptionalLocale {
  [[__gnu__::__always_inline__]]
  OptionalLocale() = default;

  explicit OptionalLocale(const locale& loc) noexcept : loc(loc), hasval(true) {}

  OptionalLocale(const OptionalLocale& l) noexcept : hasval(l.hasval) {
    if (hasval) std::construct_at(&loc, l.loc);
  }

  auto operator=(const OptionalLocale& l) noexcept -> OptionalLocale& {
    if (hasval) {
      if (l.hasval)
        loc = l.loc;
      else {
        loc.~locale();
        hasval = false;
      }
    } else if (l.hasval) {
      std::construct_at(&loc, l.loc);
      hasval = true;
    }
    return *this;
  }

  ~OptionalLocale() {
    if (hasval) loc.~locale();
  }

  auto operator=(locale&& iloc) noexcept -> OptionalLocale& {
    if (hasval)
      loc = iloc;
    else {
      std::construct_at(&loc, std::move(iloc));
      hasval = true;
    }
    return *this;
  }

  auto value() noexcept -> const locale& {
    if (!hasval) {
      std::construct_at(&loc);
      hasval = true;
    }
    return loc;
  }

  [[nodiscard]] auto has_value() const noexcept -> bool { return hasval; }

  union {
    char dummy = '\0';
    std::locale loc;
  };
  bool hasval = false;
};

template <typename CharT>
concept Char = std::same_as<CharT, char>;

template <Char CharT>
struct FormatterStr {
  constexpr auto parse(BasicFormatParseContext<CharT>& pc) -> typename BasicFormatParseContext<CharT>::iterator {
    auto first = pc.begin();
    const auto last = pc.end();
    Spec<CharT> spec{};

    auto finished = [&] -> auto {
      if (first == last || *first == '}') {
        spec_ = spec;
        return true;
      }
      return false;
    };

    if (finished()) return first;

    first = spec.parse_fill_and_align(first, last);
    if (finished()) return first;

    first = spec.parse_width(first, last, pc);
    if (finished()) return first;

    first = spec.parse_precision(first, last, pc);
    if (finished()) return first;

    if (*first == 's')
      ++first;
    else if (*first == '?') {
      spec.type = PresType::Esc;
      ++first;
    }

    if (finished()) return first;

    detail::failed_to_parse_format_spec();
  }

  template <typename Out>
  auto format(basic_string_view<CharT> s, BasicFormatContext<Out, CharT>& fc) const -> Out {
    if (spec_.type == PresType::Esc) {
      // TODO: C++23 escaped string presentation
    }

    if (spec_.width_kind == WidthPrec::None && spec_.prec_kind == WidthPrec::None) return detail::write(fc.out(), s);

    size_t estimated_width;
    if constexpr (__unicode::__literal_encoding_is_unicode<CharT>()) {
      if (spec_.prec_kind != WidthPrec::None) {
        size_t prec = spec_.get_precision(fc);
        estimated_width = __unicode::__truncate(s, prec);
      } else
        estimated_width = __unicode::__field_width(s);
    } else {
      s = s.substr(0, spec_.get_precision(fc));
      estimated_width = s.size();
    }

    return detail::write_padded_as_spec(s, estimated_width, fc, spec_);
  }

  constexpr void set_debug_format() noexcept { spec_.type = PresType::Esc; }

 private:
  Spec<CharT> spec_{};
};

template <Char CharT>
struct FormatterInt {
  // If no presentation type is specified, meaning of "none" depends
  // whether we are formatting an integer or a char or a bool.
  static constexpr PresType as_integer = PresType::d;
  static constexpr PresType as_bool = PresType::s;
  static constexpr PresType as_char = PresType::c;

  constexpr auto do_parse(BasicFormatParseContext<CharT>& pc, PresType type) ->
      typename BasicFormatParseContext<CharT>::iterator {
    Spec<CharT> spec{};
    spec.type = type;

    const auto last = pc.end();
    auto first = pc.begin();

    auto finished = [&] -> auto {
      if (first == last || *first == '}') {
        spec_ = spec;
        return true;
      }
      return false;
    };

    if (finished()) return first;

    first = spec.parse_fill_and_align(first, last);
    if (finished()) return first;

    first = spec.parse_sign(first, last);
    if (finished()) return first;

    first = spec.parse_alternate_form(first, last);
    if (finished()) return first;

    first = spec.parse_zero_fill(first, last);
    if (finished()) return first;

    first = spec.parse_width(first, last, pc);
    if (finished()) return first;

    first = spec.parse_locale(first, last);
    if (finished()) return first;

    switch (*first) {
      case 'b':
        spec.type = PresType::b;
        ++first;
        break;
      case 'B':
        spec.type = PresType::B;
        ++first;
        break;
      case 'c':
        // _GLIBCXX_RESOLVE_LIB_DEFECTS
        // 3586. format should not print bool with 'c'
        if (type != as_bool) {
          spec.type = PresType::c;
          ++first;
        }
        break;
      case 'd':
        spec.type = PresType::d;
        ++first;
        break;
      case 'o':
        spec.type = PresType::o;
        ++first;
        break;
      case 'x':
        spec.type = PresType::x;
        ++first;
        break;
      case 'X':
        spec.type = PresType::X;
        ++first;
        break;
      case 's':
        if (type == as_bool) {
          spec.type = PresType::s;  // same value (and meaning) as "none"
          ++first;
        }
        break;
      case '?':
        if (type == as_char) {
          spec.type = PresType::Esc;
          ++first;
        }
        break;
    }

    if (finished()) return first;

    detail::failed_to_parse_format_spec();
  }

  template <typename Tp>
  constexpr auto parse(BasicFormatParseContext<CharT>& pc) -> typename BasicFormatParseContext<CharT>::iterator {
    if constexpr (is_same_v<Tp, bool>) {
      auto end = do_parse(pc, as_bool);
      if (spec_.type == PresType::s)
        if (spec_.sign || spec_.alt || spec_.zero_fill)
          throw_format_error("format error: format-spec contains invalid formatting options for 'bool'");
      return end;
    } else if constexpr (Char<Tp>) {
      auto end = do_parse(pc, as_char);
      if (spec_.type == PresType::c || spec_.type == PresType::Esc)
        if (spec_.sign || spec_.alt || spec_.zero_fill
            /* XXX should be invalid? || _M_spec._M_localized */)
          throw_format_error("format error: format-spec contains invalid formatting options for 'charT'");
      return end;
    } else
      return do_parse(pc, as_integer);
  }

  template <typename Int, typename Out>
  auto format(Int i, BasicFormatContext<Out, CharT>& fc) const -> typename BasicFormatContext<Out, CharT>::iterator {
    if (spec_.type == PresType::c) return format_character(to_character(i), fc);

    std::array<char, (sizeof(Int) * __CHAR_BIT__) + 3> buf;
    std::to_chars_result res{};

    string_view base_prefix;
    std::make_unsigned_t<Int> u;
    if (i < 0)
      u = -static_cast<decltype(u)>(i);
    else
      u = i;

    char* start = buf.begin() + 3;
    char* const end = buf.end();
    char* const start_digits = start;

    switch (spec_.type) {
      case PresType::b:
      case PresType::B:
        base_prefix = spec_.type == PresType::b ? "0b" : "0B";
        res = to_chars(start, end, u, 2);
        break;
      case PresType::None:
        // Should not reach here with _Pres_none for bool or charT, so:
        [[fallthrough]];
      case PresType::d:
        res = to_chars(start, end, u, 10);
        break;
      case PresType::o:
        if (i != 0) base_prefix = "0";
        res = to_chars(start, end, u, 8);
        break;
      case PresType::x:
      case PresType::X:
        base_prefix = spec_.type == PresType::x ? "0x" : "0X";
        res = to_chars(start, end, u, 16);
        if (spec_.type == PresType::X)
          for (auto* p = start; p != res.ptr; ++p) *p = std::toupper(*p);
        break;
      default:
        std::unreachable();
    }

    if (spec_.alt && base_prefix.size()) {
      start -= base_prefix.size();
      __builtin_memcpy(start, base_prefix.data(), base_prefix.size());
    }
    start = detail::put_sign(i, spec_.sign, start - 1);

    return format_int(string_view(start, res.ptr - start), start_digits - start, fc);
  }

  template <typename Out>
  auto format(bool i, BasicFormatContext<Out, CharT>& fc) const -> typename BasicFormatContext<Out, CharT>::iterator {
    if (spec_.type == PresType::c) return format_character(static_cast<unsigned char>(i), fc);
    if (spec_.type != PresType::s) return format(static_cast<unsigned char>(i), fc);

    basic_string<CharT> s;
    size_t est_width;
    if (spec_.localized) [[unlikely]] {
      auto& np = std::use_facet<numpunct<CharT>>(fc.locale());
      s = i ? np.truename() : np.falsename();
      est_width = s.size();  // TODO Unicode-aware estimate
    } else {
      if constexpr (is_same_v<char, CharT>)
        s = i ? "true" : "false";
      else
        s = i ? L"true" : L"false";
      est_width = s.size();
    }

    return detail::write_padded_as_spec(s, est_width, fc, spec_);
  }

  template <typename Out>
  auto format_character(CharT c, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    return detail::write_padded_as_spec({&c, 1U}, 1, fc, spec_);
  }

  template <typename Int>
  static auto to_character(Int i) -> CharT {
    using traits = __gnu_cxx::__int_traits<CharT>;
    if constexpr (is_signed_v<Int> == is_signed_v<CharT>) {
      if (traits::__min <= i && i <= traits::__max) return static_cast<CharT>(i);
    } else if constexpr (is_signed_v<Int>) {
      if (i >= 0 && make_unsigned_t<Int>(i) <= traits::__max) return static_cast<CharT>(i);
    } else if (i <= make_unsigned_t<CharT>(traits::__max))
      return static_cast<CharT>(i);
    throw_format_error("format error: integer not representable as character");
  }

  template <typename Out>
  auto format_int(string_view narrow_str, size_t prefix_len, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    size_t width = spec_.get_width(fc);

    basic_string_view<CharT> str;
    if constexpr (is_same_v<char, CharT>) str = narrow_str;

    if (spec_.localized) {
      const auto& l = fc.locale();
      if (l.name() != "C") {
        auto& np = use_facet<numpunct<CharT>>(l);
        string grp = np.grouping();
        if (!grp.empty()) {
          size_t n = str.size() - prefix_len;
          auto p = (CharT*)__builtin_alloca((2 * n * sizeof(CharT)) + prefix_len);
          auto s = str.data();
          char_traits<CharT>::copy(p, s, prefix_len);
          s += prefix_len;
          auto end = std::__add_grouping(p + prefix_len, np.thousands_sep(), grp.data(), grp.size(), s, s + n);
          str = {p, size_t(end - p)};
        }
      }
    }

    if (width <= str.size()) return detail::write(fc.out(), str);

    char32_t fill_char = spec_.fill;
    Align align = spec_.align;

    size_t nfill = width - str.size();
    auto out = fc.out();
    if (align == Align::Default) {
      align = Align::Right;
      if (spec_.zero_fill) {
        fill_char = CharT('0');
        // Write sign and base prefix before zero filling.
        if (prefix_len != 0) {
          out = detail::write(std::move(out), str.substr(0, prefix_len));
          str.remove_prefix(prefix_len);
        }
      } else
        fill_char = CharT(' ');
    }
    return detail::write_padded(std::move(out), str, align, nfill, fill_char);
  }

  [[nodiscard]] constexpr auto spec() const noexcept -> const Spec<CharT>& { return spec_; }

 private:
  Spec<CharT> spec_{};
};

using std::to_chars;

// We can format a floating-point type iff it is usable with to_chars.
template <typename Tp>
concept formattable_float = is_same_v<remove_cv_t<Tp>, Tp> &&
                            requires(Tp t, char* p) { detail::to_chars(p, p, t, chars_format::scientific, 6); };

template <Char CharT>
struct FormatterFp {
  constexpr auto parse(BasicFormatParseContext<CharT>& pc) -> typename BasicFormatParseContext<CharT>::iterator {
    Spec<CharT> spec{};
    const auto last = pc.end();
    auto first = pc.begin();

    auto finished = [&] -> auto {
      if (first == last || *first == '}') {
        spec_ = spec;
        return true;
      }
      return false;
    };

    if (finished()) return first;

    first = spec.parse_fill_and_align(first, last);
    if (finished()) return first;

    first = spec.parse_sign(first, last);
    if (finished()) return first;

    first = spec.parse_alternate_form(first, last);
    if (finished()) return first;

    first = spec.parse_zero_fill(first, last);
    if (finished()) return first;

    if (first[0] != '.') {
      first = spec.parse_width(first, last, pc);
      if (finished()) return first;
    }

    first = spec.parse_precision(first, last, pc);
    if (finished()) return first;

    first = spec.parse_locale(first, last);
    if (finished()) return first;

    switch (*first) {
      case 'a':
        spec.type = PresType::a;
        ++first;
        break;
      case 'A':
        spec.type = PresType::A;
        ++first;
        break;
      case 'e':
        spec.type = PresType::e;
        ++first;
        break;
      case 'E':
        spec.type = PresType::E;
        ++first;
        break;
      case 'f':
        spec.type = PresType::f;
        ++first;
        break;
      case 'F':
        spec.type = PresType::F;
        ++first;
        break;
      case 'g':
        spec.type = PresType::g;
        ++first;
        break;
      case 'G':
        spec.type = PresType::G;
        ++first;
        break;
    }

    if (finished()) return first;

    detail::failed_to_parse_format_spec();
  }

  template <typename Fp, typename Out>
  auto format(Fp v, BasicFormatContext<Out, CharT>& fc) const -> typename BasicFormatContext<Out, CharT>::iterator {
    std::string dynbuf;
    std::array<char, 128> buf;
    to_chars_result res{};

    size_t prec = 6;
    bool use_prec = spec_.prec_kind != WidthPrec::None;
    if (use_prec) prec = spec_.get_precision(fc);

    char* start = buf.begin() + 1;  // reserve space for sign
    char* end = buf.end();

    chars_format fmt{};
    bool upper = false;
    bool trailing_zeros = false;
    char expc = 'e';

    switch (spec_.type) {
      case PresType::A:
        upper = true;
        expc = 'P';
        [[fallthrough]];
      case PresType::a:
        if (spec_.type != PresType::A) expc = 'p';
        fmt = chars_format::hex;
        break;
      case PresType::E:
        upper = true;
        expc = 'E';
        [[fallthrough]];
      case PresType::e:
        use_prec = true;
        fmt = chars_format::scientific;
        break;
      case PresType::F:
        upper = true;
        [[fallthrough]];
      case PresType::f:
        use_prec = true;
        fmt = chars_format::fixed;
        break;
      case PresType::G:
        upper = true;
        expc = 'E';
        [[fallthrough]];
      case PresType::g:
        trailing_zeros = true;
        use_prec = true;
        fmt = chars_format::general;
        break;
      case PresType::None:
        if (use_prec) fmt = chars_format::general;
        break;
      default:
        __builtin_unreachable();
    }

    // Write value into buffer using std::to_chars.
    auto to_chars = [&](char* b, char* e) -> auto {
      if (use_prec) return detail::to_chars(b, e, v, fmt, prec);
      if (fmt != chars_format{}) return detail::to_chars(b, e, v, fmt);
      return detail::to_chars(b, e, v);
    };

    // First try using stack buffer.
    res = to_chars(start, end);

    if (__builtin_expect(res.ec == errc::value_too_large, 0)) {
      // If the buffer is too small it's probably because of a large
      // precision, or a very large value in fixed format.
      size_t guess = 8 + prec;
      if (fmt == chars_format::fixed)  // +ddd.prec
      {
        if constexpr (is_same_v<Fp, float> || is_same_v<Fp, double> || is_same_v<Fp, long double>) {
          // The number of digits to the left of the decimal point
          // is floor(log10(max(abs(__v),1)))+1
          int exp{};
          if constexpr (is_same_v<Fp, float>)
            __builtin_frexpf(v, &exp);
          else if constexpr (is_same_v<Fp, double>)
            __builtin_frexp(v, &exp);
          else if constexpr (is_same_v<Fp, long double>)
            __builtin_frexpl(v, &exp);
          if (exp > 0) guess += 1U + (exp * 4004U / 13301U);  // log10(2) approx.
        } else
          guess += numeric_limits<Fp>::max_exponent10;
      }
      if (guess <= sizeof(buf)) [[unlikely]]
        guess = sizeof(buf) * 2;
      dynbuf.reserve(guess);

      do {
        auto overwrite = [&to_chars, &res](char* p, size_t n) -> auto {
          res = to_chars(p + 1, p + n - 1);
          return res.ec == errc{} ? res.ptr - p : 0;
        };

        dynbuf.__resize_and_overwrite(dynbuf.capacity() * 2, overwrite);
        start = dynbuf.data() + 1;  // reserve space for sign
        end = dynbuf.data() + dynbuf.size();
      } while (__builtin_expect(res.ec == errc::value_too_large, 0));
    }

    // Use uppercase for 'A', 'E', and 'G' formats.
    if (upper) {
      for (char* p = start; p != res.ptr; ++p) *p = std::toupper(*p);
    }

    bool have_sign = true;
    // Add sign for non-negative values.
    if (!__builtin_signbit(v)) {
      if (spec_.sign == Sign::Plus)
        *--start = '+';
      else if (spec_.sign == Sign::Space)
        *--start = ' ';
      else
        have_sign = false;
    }

    string_view narrow_str(start, res.ptr - start);

    // Use alternate form. Ensure decimal point is always present,
    // and add trailing zeros (up to precision) for g and G forms.
    if (spec_.alt && __builtin_isfinite(v)) {
      string_view s = narrow_str;
      size_t sigfigs;              // Number of significant figures.
      size_t z = 0;                // Number of trailing zeros to add.
      size_t p;                    // Position of the exponent character (if any).
      size_t d = s.find('.');      // Position of decimal point.
      if (d != string_view::npos)  // Found decimal point.
      {
        p = s.find(expc, d + 1);
        if (p == string_view::npos) p = s.size();

        // If presentation type is g or G we might need to add zeros.
        if (trailing_zeros) {
          // Find number of digits after first significant figure.
          if (s[have_sign] != '0')
            // A string like "D.D" or "-D.DDD"
            sigfigs = p - have_sign - 1;
          else
            // A string like "0.D" or "-0.0DD".
            // Safe to assume there is a non-zero digit, because
            // otherwise there would be no decimal point.
            sigfigs = p - s.find_first_not_of('0', d + 1);
        }
      } else  // No decimal point, we need to insert one.
      {
        p = s.find(expc);  // Find the exponent, if present.
        if (p == string_view::npos) p = s.size();
        d = p;  // Position where '.' should be inserted.
        sigfigs = d - have_sign;
      }

      if (trailing_zeros && prec != 0) {
        // For g and G presentation types std::to_chars produces
        // no more than prec significant figures. Insert this many
        // zeros so the result has exactly prec significant figures.
        z = prec - sigfigs;
      }

      if (size_t extras = static_cast<int>(d == p) + z)  // How many to add.
      {
        if (dynbuf.empty() && extras <= static_cast<size_t>(end - res.ptr)) {
          // The stack buffer is large enough for the result.
          // Move exponent to make space for extra chars.
          __builtin_memmove(start + p + extras, start + p, s.size() - p);
          if (d == p) start[p++] = '.';
          __builtin_memset(start + p, '0', z);
          narrow_str = {s.data(), s.size() + extras};
        } else  // Need to switch to the dynamic buffer.
        {
          dynbuf.reserve(s.size() + extras);
          if (dynbuf.empty()) {
            dynbuf = s.substr(0, p);
            if (d == p) dynbuf += '.';
            if (z) dynbuf.append(z, '0');
            dynbuf.append(s.substr(p));
          } else {
            dynbuf.insert(p, extras, '0');
            if (d == p) dynbuf[p] = '.';
          }
          narrow_str = dynbuf;
        }
      }
    }

    basic_string<CharT> wstr;
    basic_string_view<CharT> str;
    if constexpr (is_same_v<CharT, char>) str = narrow_str;

    if (spec_.localized && __builtin_isfinite(v)) {
      wstr = localize(str, expc, fc.locale());
      if (!wstr.empty()) str = wstr;
    }

    size_t width = spec_.get_width(fc);

    if (width <= str.size()) return detail::write(fc.out(), str);

    char32_t fill_char = spec_.fill;
    Align alignment = spec_.align;

    size_t nfill = width - str.size();
    auto out = fc.out();
    if (alignment == Align::Default) {
      alignment = Align::Right;
      if (spec_.zero_fill && __builtin_isfinite(v)) {
        fill_char = CharT('0');
        // Write sign before zero filling.
        if (!detail::is_xdigit(narrow_str[0])) {
          *out++ = str[0];
          str.remove_prefix(1);
        }
      } else
        fill_char = CharT(' ');
    }
    return detail::write_padded(std::move(out), str, alignment, nfill, fill_char);
  }

  // Locale-specific format.
  [[nodiscard]] auto localize(basic_string_view<CharT> str, char expc, const locale& loc) const -> basic_string<CharT> {
    basic_string<CharT> lstr;

    if (loc == locale::classic()) return lstr;  // Nothing to do.

    const auto& np = use_facet<numpunct<CharT>>(loc);
    const CharT point = np.decimal_point();
    const string grp = np.grouping();

    CharT dot;
    CharT exp;
    if constexpr (is_same_v<CharT, char>) {
      dot = '.';
      exp = expc;
    } else {
      dot = L'.';
      switch (expc) {
        case 'e':
          exp = L'e';
          break;
        case 'E':
          exp = L'E';
          break;
        case 'p':
          exp = L'p';
          break;
        case 'P':
          exp = L'P';
          break;
        default:
          __builtin_unreachable();
      }
    }

    if (grp.empty() && point == dot) return lstr;  // Locale uses '.' and no grouping.

    size_t d = str.find(dot);
    size_t e = min(d, str.find(exp));
    if (e == str.npos) e = str.size();
    const size_t r = str.size() - e;
    auto overwrite = [&](CharT* p, size_t) -> auto {
      auto end = std::__add_grouping(p, np.thousands_sep(), grp.data(), grp.size(), str.data(), str.data() + e);
      if (r) {
        if (d != str.npos) {
          *end = point;
          ++end;
          ++e;
        }
        if (r > 1) end += str.copy(end, str.npos, e);
      }
      return (end - p);
    };
    lstr.__resize_and_overwrite((e * 2) + r, overwrite);
    return lstr;
  }

 private:
  Spec<CharT> spec_{};
};

}  // namespace detail
/// @endcond

/// Format a character.
template <detail::Char CharT>
struct Formatter<CharT, CharT> {
  Formatter() = default;

  constexpr auto parse(BasicFormatParseContext<CharT>& pc) -> typename BasicFormatParseContext<CharT>::iterator {
    return f_.template parse<CharT>(pc);
  }

  template <typename Out>
  auto format(CharT u, BasicFormatContext<Out, CharT>& fc) const -> typename BasicFormatContext<Out, CharT>::iterator {
    if (f_.spec().type == detail::PresType::None || f_.spec().type == detail::PresType::c)
      return f_.format_character(u, fc);
    if (f_.spec().type == detail::PresType::Esc) {
      // TODO
      return fc.out();
    }
    return f_.format(static_cast<make_unsigned_t<CharT>>(u), fc);
  }

  constexpr void set_debug_format() noexcept { f_.spec_.type = detail::PresType::Esc; }

 private:
  detail::FormatterInt<CharT> f_;
};

/** Format a string.
 * @{
 */
template <detail::Char CharT>
struct Formatter<CharT*, CharT> {
  Formatter() = default;

  [[__gnu__::__always_inline__]]
  constexpr auto parse(BasicFormatParseContext<CharT>& pc) -> typename BasicFormatParseContext<CharT>::iterator {
    return f_.parse(pc);
  }

  template <typename Out>
  [[__gnu__::__nonnull__]]
  auto format(CharT* u, BasicFormatContext<Out, CharT>& fc) const -> typename BasicFormatContext<Out, CharT>::iterator {
    return f_.format(u, fc);
  }

  constexpr void set_debug_format() noexcept { f_.set_debug_format(); }

 private:
  detail::FormatterStr<CharT> f_;
};

template <detail::Char CharT>
struct Formatter<const CharT*, CharT> {
  Formatter() = default;

  [[__gnu__::__always_inline__]]
  constexpr auto parse(BasicFormatParseContext<CharT>& pc) -> typename BasicFormatParseContext<CharT>::iterator {
    return f_.parse(pc);
  }

  template <typename Out>
  [[__gnu__::__nonnull__]]
  auto format(const CharT* u, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    return f_.format(u, fc);
  }

  constexpr void set_debug_format() noexcept { f_.set_debug_format(); }

 private:
  detail::FormatterStr<CharT> f_;
};

template <detail::Char CharT, size_t Nm>
struct Formatter<CharT[Nm], CharT> {
  Formatter() = default;

  [[__gnu__::__always_inline__]]
  constexpr auto parse(BasicFormatParseContext<CharT>& pc) -> typename BasicFormatParseContext<CharT>::iterator {
    return f_.parse(pc);
  }

  template <typename Out>
  auto format(const CharT (&u)[Nm], BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    return f_.format({u, Nm}, fc);
  }

  constexpr void set_debug_format() noexcept { f_.set_debug_format(); }

 private:
  detail::FormatterStr<CharT> f_;
};

template <typename Traits, typename Alloc>
struct Formatter<basic_string<char, Traits, Alloc>, char> {
  Formatter() = default;

  [[__gnu__::__always_inline__]]
  constexpr auto parse(BasicFormatParseContext<char>& pc) -> typename BasicFormatParseContext<char>::iterator {
    return f_.parse(pc);
  }

  template <typename Out>
  auto format(const basic_string<char, Traits, Alloc>& u, BasicFormatContext<Out, char>& fc) const ->
      typename BasicFormatContext<Out, char>::iterator {
    return f_.format(u, fc);
  }

  constexpr void set_debug_format() noexcept { f_.set_debug_format(); }

 private:
  detail::FormatterStr<char> f_;
};

template <typename Traits>
struct Formatter<basic_string_view<char, Traits>, char> {
  Formatter() = default;

  [[__gnu__::__always_inline__]]
  constexpr auto parse(BasicFormatParseContext<char>& pc) -> typename BasicFormatParseContext<char>::iterator {
    return f_.parse(pc);
  }

  template <typename Out>
  auto format(basic_string_view<char, Traits> u, BasicFormatContext<Out, char>& fc) const ->
      typename BasicFormatContext<Out, char>::iterator {
    return f_.format(u, fc);
  }

  constexpr void set_debug_format() noexcept { f_.set_debug_format(); }

 private:
  detail::FormatterStr<char> f_;
};

/// @cond undocumented
namespace detail {

// each cv-unqualified arithmetic type ArithmeticT other than
// char, wchar_t, char8_t, char16_t, or char32_t
template <typename Tp>
constexpr bool is_formattable_integer = __is_integer<Tp>::__value;

#if defined __SIZEOF_INT128__
template <>
inline constexpr bool is_formattable_integer<__int128> = true;
template <>
inline constexpr bool is_formattable_integer<unsigned __int128> = true;
#endif

template <>
inline constexpr bool is_formattable_integer<char> = false;
template <>
inline constexpr bool is_formattable_integer<wchar_t> = false;
template <>
inline constexpr bool is_formattable_integer<char8_t> = false;
template <>
inline constexpr bool is_formattable_integer<char16_t> = false;
template <>
inline constexpr bool is_formattable_integer<char32_t> = false;
}  // namespace detail
/// @endcond

/// Format an integer.
template <typename Tp, detail::Char CharT>
  requires detail::is_formattable_integer<Tp>
struct Formatter<Tp, CharT> {
  Formatter() = default;

  [[__gnu__::__always_inline__]]
  constexpr auto parse(BasicFormatParseContext<CharT>& pc) -> typename BasicFormatParseContext<CharT>::iterator {
    return f_.template parse<Tp>(pc);
  }

  template <typename Out>
  auto format(Tp u, BasicFormatContext<Out, CharT>& fc) const -> typename BasicFormatContext<Out, CharT>::iterator {
    return f_.format(u, fc);
  }

 private:
  detail::FormatterInt<CharT> f_;
};

/// Format a floating-point value.
template <detail::formattable_float Tp, detail::Char CharT>
struct Formatter<Tp, CharT> {
  Formatter() = default;

  [[__gnu__::__always_inline__]]
  constexpr auto parse(BasicFormatParseContext<CharT>& pc) -> typename BasicFormatParseContext<CharT>::iterator {
    return f_.parse(pc);
  }

  template <typename Out>
  auto format(Tp u, BasicFormatContext<Out, CharT>& fc) const -> typename BasicFormatContext<Out, CharT>::iterator {
    return f_.format(u, fc);
  }

 private:
  detail::FormatterFp<CharT> f_;
};

/** Format a pointer.
 * @{
 */
template <detail::Char CharT>
struct Formatter<const void*, CharT> {
  Formatter() = default;

  constexpr auto parse(BasicFormatParseContext<CharT>& pc) -> typename BasicFormatParseContext<CharT>::iterator {
    detail::Spec<CharT> spec{};
    const auto last = pc.end();
    auto first = pc.begin();

    auto finalize = [this, &spec] -> auto { spec_ = spec; };

    auto finished = [&] -> auto {
      if (first == last || *first == '}') {
        finalize();
        return true;
      }
      return false;
    };

    if (finished()) return first;

    first = spec.parse_fill_and_align(first, last);
    if (finished()) return first;

    first = spec.parse_zero_fill(first, last);
    if (finished()) return first;

    first = spec.parse_width(first, last, pc);

    if (first != last) {
      if (*first == 'p')
        ++first;
      else if (*first == 'P') {
        // _GLIBCXX_RESOLVE_LIB_DEFECTS
        // P2510R3 Formatting pointers
        spec.type = detail::PresType::P;
        ++first;
      }
    }

    if (finished()) return first;

    detail::failed_to_parse_format_spec();
  }

  template <typename Out>
  auto format(const void* v, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    auto u = reinterpret_cast<uintptr_t>(v);
    std::array<char, 2 + (sizeof(v) * 2)> buf;
    auto [ptr, ec] = std::to_chars(buf.begin() + 2, std::end(buf), u, 16);
    int n = ptr - buf.data();
    buf[0] = '0';
    buf[1] = 'x';
    if (spec_.type == detail::PresType::P) {
      buf[1] = 'X';
      for (auto* p = buf.begin() + 2; p != ptr; ++p) *p = std::toupper(*p);
    }

    basic_string_view<CharT> str;
    if constexpr (is_same_v<CharT, char>) str = string_view(buf.data(), n);

    if (spec_.zero_fill) {
      size_t width = spec_.get_width(fc);
      if (width <= str.size()) return detail::write(fc.out(), str);

      auto out = fc.out();
      // Write "0x" or "0X" prefix before zero-filling.
      out = detail::write(std::move(out), str.substr(0, 2));
      str.remove_prefix(2);
      size_t nfill = width - n;
      return detail::write_padded(std::move(out), str, detail::Align::Right, nfill, CharT('0'));
    }

    return detail::write_padded_as_spec(str, n, fc, spec_, detail::Align::Right);
  }

 private:
  detail::Spec<CharT> spec_{};
};

template <detail::Char CharT>
struct Formatter<void*, CharT> {
  Formatter() = default;

  [[__gnu__::__always_inline__]]
  constexpr auto parse(BasicFormatParseContext<CharT>& pc) -> typename BasicFormatParseContext<CharT>::iterator {
    return f_.parse(pc);
  }

  template <typename Out>
  auto format(void* v, BasicFormatContext<Out, CharT>& fc) const -> typename BasicFormatContext<Out, CharT>::iterator {
    return f_.format(v, fc);
  }

 private:
  Formatter<const void*, CharT> f_;
};

template <detail::Char CharT>
struct Formatter<nullptr_t, CharT> {
  Formatter() = default;

  [[__gnu__::__always_inline__]]
  constexpr auto parse(BasicFormatParseContext<CharT>& pc) -> typename BasicFormatParseContext<CharT>::iterator {
    return f_.parse(pc);
  }

  template <typename Out>
  auto format(nullptr_t, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    return f_.format(nullptr, fc);
  }

 private:
  Formatter<const void*, CharT> f_;
};
/// @}

/// @cond undocumented
namespace detail {

template <typename Tp, typename Context,
          typename Formatter = typename Context::template formatter_type<remove_const_t<Tp>>,
          typename ParseContext = BasicFormatParseContext<typename Context::char_type>>
concept parsable_with = std::semiregular<Formatter> && requires(Formatter f, ParseContext pc) {
  { f.parse(pc) } -> same_as<typename ParseContext::iterator>;
};

template <typename Tp, typename Context,
          typename Formatter = typename Context::template formatter_type<remove_const_t<Tp>>,
          typename ParseContext = BasicFormatParseContext<typename Context::char_type>>
concept formattable_with = std::semiregular<Formatter> && requires(const Formatter cf, Tp&& t, Context fc) {
  { cf.format(t, fc) } -> same_as<typename Context::iterator>;
};

// An unspecified output iterator type used in the `formattable` concept.
template <typename CharT>
using iter_for = back_insert_iterator<basic_string<CharT>>;

template <typename Tp, typename CharT, typename Context = BasicFormatContext<iter_for<CharT>, CharT>>
concept formattable_impl = parsable_with<Tp, Context> && formattable_with<Tp, Context>;

}  // namespace detail
/// @endcond

// Concept std::formattable was introduced by P2286R8 "Formatting Ranges",
// but we can't guard it with __cpp_lib_format_ranges until we define that!
// [format.formattable], concept formattable
template <typename Tp, typename CharT>
concept formattable = detail::formattable_impl<remove_reference_t<Tp>, CharT>;

/// @cond undocumented
namespace detail {
template <typename Rg, typename CharT>
concept const_formattable_range =
    std::ranges::input_range<const Rg> && formattable<std::ranges::range_reference_t<const Rg>, CharT>;

template <typename Rg, typename CharT>
using maybe_const_range = std::conditional_t<const_formattable_range<Rg, CharT>, const Rg, Rg>;

}  // namespace detail
   /// @endcond

/// An iterator after the last character written, and the number of
/// characters that would have been written.
template <typename Out>
struct FormatToNResult {
  Out out;
  iter_difference_t<Out> size;
};

/// @cond undocumented
namespace detail {

template <typename CharT>
class SinkIter {
  Sink<CharT>* sink_ = nullptr;

 public:
  using iterator_category = output_iterator_tag;
  using value_type = void;
  using difference_type = ptrdiff_t;
  using pointer = void;
  using reference = void;

  SinkIter() = default;
  SinkIter(const SinkIter&) = default;
  auto operator=(const SinkIter&) -> SinkIter& = default;

  [[__gnu__::__always_inline__]]
  explicit constexpr SinkIter(Sink<CharT>& sink)
      : sink_(std::addressof(sink)) {}

  [[__gnu__::__always_inline__]]
  constexpr auto operator=(CharT c) -> SinkIter& {
    sink_->write(c);
    return *this;
  }

  [[__gnu__::__always_inline__]]
  constexpr auto operator=(basic_string_view<CharT> s) -> SinkIter& {
    sink_->write(s);
    return *this;
  }

  [[__gnu__::__always_inline__]]
  constexpr auto operator*() -> SinkIter& {
    return *this;
  }

  [[__gnu__::__always_inline__]]
  constexpr auto operator++() -> SinkIter& {
    return *this;
  }

  [[__gnu__::__always_inline__]]
  constexpr auto operator++(int) -> SinkIter {
    return *this;
  }

  [[nodiscard]] auto reserve(size_t n) const { return sink_->reserve(n); }
};

// Abstract base class for type-erased character sinks.
// All formatting and output is done via this type's iterator,
// to reduce the number of different template instantiations.
template <typename CharT>
class Sink {
  friend class SinkIter<CharT>;

  std::span<CharT> span_;
  typename std::span<CharT>::iterator next_;

  // Called when the span is full, to make more space available.
  // Precondition: _M_next != _M_span.begin()
  // Postcondition: _M_next != _M_span.end()
  // TODO: remove the precondition? could make overflow handle it.
  virtual void overflow() = 0;

 protected:
  // Precondition: __span.size() != 0
  [[__gnu__::__always_inline__]]
  explicit constexpr Sink(std::span<CharT> span) noexcept
      : span_(span), next_(span.begin()) {}

  // The portion of the span that has been written to.
  [[__gnu__::__always_inline__]] [[nodiscard]] auto used() const noexcept -> std::span<CharT> {
    return span_.first(next_ - span_.begin());
  }

  // The portion of the span that has not been written to.
  [[__gnu__::__always_inline__]] [[nodiscard]] constexpr auto unused() const noexcept -> std::span<CharT> {
    return span_.subspan(next_ - span_.begin());
  }

  // Use the start of the span as the next write position.
  [[__gnu__::__always_inline__]]
  constexpr void rewind() noexcept {
    next_ = span_.begin();
  }

  // Replace the current output range.
  void reset(span<CharT> s, size_t pos = 0) noexcept {
    span_ = s;
    next_ = s.begin() + pos;
  }

  // Called by the iterator for *it++ = c
  constexpr void write(CharT c) {
    *next_++ = c;
    if (next_ - span_.begin() == std::ssize(span_)) [[unlikely]]
      overflow();
  }

  constexpr void write(basic_string_view<CharT> s) {
    std::span to = unused();
    while (to.size() <= s.size()) {
      s.copy(to.data(), to.size());
      next_ += to.size();
      s.remove_prefix(to.size());
      overflow();
      to = unused();
    }
    if (s.size()) {
      s.copy(to.data(), s.size());
      next_ += s.size();
    }
  }

  // A successful _Reservation can be used to directly write
  // up to N characters to the sink to avoid unwanted buffering.
  struct Reservation {
    // True if the reservation was successful, false otherwise.
    explicit operator bool() const noexcept { return sink; }
    // A pointer to write directly to the sink.
    [[nodiscard]] auto get() const noexcept -> CharT* { return sink->next_.operator->(); }
    // Add n to the _M_next iterator for the sink.
    void bump(size_t n) { sink->bump(n); }

    Sink* sink;
  };

  // Attempt to reserve space to write n characters to the sink.
  // If anything is written to the reservation then there must be a call
  // to _M_bump(N2) before any call to another member function of *this,
  // where N2 is the number of characters written.
  virtual auto reserve(size_t n) -> Reservation {
    if (n <= unused().size()) return {this};

    if (n <= span_.size())  // Cannot meet the request.
    {
      overflow();  // Make more space available.
      if (n <= unused().size()) return {this};
    }
    return {nullptr};
  }

  // Update the next output position after writing directly to the sink.
  // pre: no calls to _M_write or _M_overflow since _M_reserve.
  virtual void bump(size_t n) { next_ += n; }

 public:
  Sink(const Sink&) = delete;
  auto operator=(const Sink&) -> Sink& = delete;

  [[__gnu__::__always_inline__]]
  constexpr auto out() noexcept -> SinkIter<CharT> {
    return SinkIter<CharT>(*this);
  }
};

// A sink with an internal buffer. This is used to implement concrete sinks.
template <typename CharT>
class BufSink : public Sink<CharT> {
 protected:
  CharT buf[32 * sizeof(void*) / sizeof(CharT)];

  [[__gnu__::__always_inline__]]
  constexpr BufSink() noexcept
      : Sink<CharT>(buf) {}
};

using std::vector;

// A sink that fills a sequence (e.g. std::string, std::vector, std::deque).
// Writes to a buffer then appends that to the sequence when it fills up.
template <typename Seq>
class SeqSink final : public BufSink<typename Seq::value_type> {
  using char_t = typename Seq::value_type;

  Seq seq_;

  // Transfer buffer contents to the sequence, so buffer can be refilled.
  void overflow() override {
    auto s = this->used();
    if (s.empty()) [[unlikely]]
      return;  // Nothing in the buffer to transfer to _M_seq.

    // If _M_reserve was called then _M_bump must have been called too.
    assert(s.data() != seq_.data());

    if constexpr (is_specialization_of<Seq, basic_string>)
      seq_.append(s.data(), s.size());
    else
      seq_.insert(seq_.end(), s.begin(), s.end());

    // Make the whole of _M_buf available for the next write:
    this->rewind();
  }

  auto reserve(size_t n) -> typename Sink<char_t>::Reservation override {
    // We might already have n characters available in this->_M_unused(),
    // but the whole point of this function is to be an optimization for
    // the std::format("{}", x) case. We want to avoid writing to _M_buf
    // and then copying that into a basic_string if possible, so this
    // function prefers to create space directly in _M_seq rather than
    // using _M_buf.

    if constexpr (is_specialization_of<Seq, basic_string> || is_specialization_of<Seq, vector>) {
      // Flush the buffer to _M_seq first (should not be needed).
      if (this->used().size()) [[unlikely]]
        SeqSink::overflow();

      // Expand _M_seq to make __n new characters available:
      const auto sz = seq_.size();
      if constexpr (is_same_v<string, Seq> || is_same_v<wstring, Seq>)
        seq_.resize_and_overwrite(sz + n, [](auto, auto n2) -> auto { return n2; });
      else
        seq_.resize(sz + n);

      // Set _M_used() to be a span over the original part of _M_seq
      // and _M_unused() to be the extra capacity we just created:
      this->reset(seq_, sz);
      return {this};
    } else  // Try to use the base class' buffer.
      return Sink<char_t>::_M_reserve(n);
  }

  void bump(size_t n) override {
    if constexpr (is_specialization_of<Seq, basic_string> || is_specialization_of<Seq, vector>) {
      auto s = this->used();
      assert(s.data() == seq_.data());
      // Truncate the sequence to the part that was actually written to:
      seq_.resize(s.size() + n);
      // Switch back to using buffer:
      this->reset(this->buf);
    }
  }

 public:
  // TODO: for SSO string, use SSO buffer as initial span, then switch
  // to _M_buf if it overflows? Or even do that for all unused capacity?

  [[__gnu__::__always_inline__]]
  SeqSink() noexcept(is_nothrow_default_constructible_v<Seq>) = default;

  explicit SeqSink(Seq&& s) noexcept(is_nothrow_move_constructible_v<Seq>) : seq_(std::move(s)) {}

  using Sink<char_t>::out;

  auto get() && -> Seq {
    if (this->used().size() != 0) SeqSink::overflow();
    return std::move(seq_);
  }

  // A writable span that views everything written to the sink.
  // Will be either a view over _M_seq or the used part of _M_buf.
  auto view() -> span<char_t> {
    auto s = this->used();
    if (seq_.size()) {
      if (s.size() != 0) SeqSink::overflow();
      return seq_;
    }
    return s;
  }
};

template <typename CharT, typename Alloc = allocator<CharT>>
using StrSink = SeqSink<basic_string<CharT, char_traits<CharT>, Alloc>>;

// template<typename _CharT, typename _Alloc = allocator<_CharT>>
// using _Vec_sink = _Seq_sink<vector<_CharT, _Alloc>>;

// A sink that writes to an output iterator.
// Writes to a fixed-size buffer and then flushes to the output iterator
// when the buffer fills up.
template <typename CharT, typename OutIter>
class IterSink : public BufSink<CharT> {
  OutIter out_;
  iter_difference_t<OutIter> max_;

 protected:
  size_t count_ = 0;

  void overflow() override {
    auto s = this->used();
    if (max_ < 0)  // No maximum.
      out_ = ranges::copy(s, std::move(out_)).out;
    else if (count_ < static_cast<size_t>(max_)) {
      auto max = max_ - count_;
      std::span<CharT> first;
      if (max < s.size())
        first = s.first(static_cast<size_t>(max));
      else
        first = s;
      out_ = ranges::copy(first, std::move(out_)).out;
    }
    this->rewind();
    count_ += s.size();
  }

 public:
  [[__gnu__::__always_inline__]]
  explicit IterSink(OutIter out, iter_difference_t<OutIter> max = -1)
      : out_(std::move(out)), max_(max) {}

  using Sink<CharT>::out;

  auto finish() && -> FormatToNResult<OutIter> {
    if (this->used().size() != 0) IterSink::overflow();
    std::iter_difference_t<OutIter> count(count_);
    return {std::move(out_), count};
  }
};

// Partial specialization for contiguous iterators.
// No buffer is used, characters are written straight to the iterator.
// We do not know the size of the output range, so the span size just grows
// as needed. The end of the span might be an invalid pointer outside the
// valid range, but we never actually call _M_span.end(). This class does
// not introduce any invalid pointer arithmetic or overflows that would not
// have happened anyway.
template <typename CharT, contiguous_iterator OutIter>
  requires same_as<iter_value_t<OutIter>, CharT>
class IterSink<CharT, OutIter> : public Sink<CharT> {
  OutIter first_;
  std::iter_difference_t<OutIter> max_ = -1;

 protected:
  size_t count_ = 0;

 private:
  CharT buf_[64];  // Write here after outputting _M_max characters.

 protected:
  void overflow() override {
    if (this->unused().size() != 0) return;  // No need to switch to internal buffer yet.

    auto s = this->used();

    if (max_ >= 0) {
      count_ += s.size();
      // Span was already sized for the maximum character count,
      // if it overflows then any further output must go to the
      // internal buffer, to be discarded.
      this->reset(this->buf_);
    } else {
      // No maximum character count. Just extend the span to allow
      // writing more characters to it.
      this->reset({s.data(), s.size() + 1024}, s.size());
    }
  }

  auto reserve(size_t n) -> typename Sink<CharT>::Reservation final {
    auto avail = this->unused();
    if (n > avail.size()) {
      if (max_ >= 0) return {};  // cannot grow

      auto s = this->used();
      this->reset({s.data(), s.size() + n}, s.size());
    }
    return {this};
  }

 private:
  static auto make_span(CharT* ptr, std::iter_difference_t<OutIter> n, std::span<CharT> buf) noexcept
      -> std::span<CharT> {
    if (n == 0) return buf;  // Only write to the internal buffer.

    if (n > 0) {
      if constexpr (!is_integral_v<iter_difference_t<OutIter>> || sizeof(n) > sizeof(size_t)) {
        // __int128 or __detail::__max_diff_type
        auto m = std::iter_difference_t<OutIter>(static_cast<size_t>(-1));
        if (n > m) n = m;
      }
      return {ptr, static_cast<size_t>(n)};
    }

#if __has_builtin(__builtin_dynamic_object_size)
    if (size_t bytes = __builtin_dynamic_object_size(ptr, 2)) return {ptr, bytes / sizeof(CharT)};
#endif
    // Avoid forming a pointer to a different memory page.
    const auto off = reinterpret_cast<__UINTPTR_TYPE__>(ptr) % 1024;
    n = (1024 - off) / sizeof(CharT);
    if (n > 0) [[likely]]
      return {ptr, static_cast<size_t>(n)};
    else  // Misaligned/packed buffer of wchar_t?
      return {ptr, 1};
  }

 public:
  explicit IterSink(OutIter out, std::iter_difference_t<OutIter> n = -1) noexcept
      : Sink<CharT>(make_span(std::to_address(out), n, buf_)), first_(out), max_(n) {}

  auto finish() && -> FormatToNResult<OutIter> {
    auto s = this->used();
    if (s.data() == buf_) {
      // Switched to internal buffer, so must have written _M_max.
      std::iter_difference_t<OutIter> count(count_ + s.size());
      return {first_ + max_, count};
    }  // Not using internal buffer yet
    std::iter_difference_t<OutIter> count(s.size());
    return {first_ + count, count};
  }
};

enum class ArgT : unsigned char {
  None,
  Bool,
  C,
  I,
  U,
  Ll,
  Ull,
  Flt,
  Dbl,
  Ldbl,
  Str,
  Sv,
  Ptr,
  Handle,
  I128,
  U128,
  Bf16,
  F16,
  F32,
  F64,  // These are unused.
#ifdef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
  Next_value_,
  F128 = Ldbl,
  Ibm128 = Next_value_,
#else
  F128,
#endif
  Max
};

template <typename Context>
struct ArgValue {
  using char_t = typename Context::char_type;

  struct HandleBase {
    const void* ptr;
    void (*func)();
  };

  union {
    monostate none;
    bool boolean;
    char_t c;
    int i;
    unsigned u;
    int64_t ll;
    uint64_t ull;
    float flt;
    double dbl;
#ifndef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT  // No long double if it's ambiguous.
    long double ldbl;
#endif
    const char_t* str;
    basic_string_view<char_t> sv;
    const void* ptr;
    HandleBase handle;
#ifdef __SIZEOF_INT128__
    __int128 i128;
    unsigned __int128 u128;
#endif
#ifdef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
    __ieee128 f128;
    __ibm128 ibm128;
#elif _GLIBCXX_FORMAT_F128 == 2
    __float128_t f128;
#endif
  };

  [[__gnu__::__always_inline__]]
  ArgValue()
      : none() {}

  template <typename Tp, typename Self>
  [[__gnu__::__always_inline__]]
  static auto get(Self& u) noexcept -> auto& {
    if constexpr (is_same_v<Tp, bool>)
      return u._M_bool;
    else if constexpr (is_same_v<Tp, char_t>)
      return u._M_c;
    else if constexpr (is_same_v<Tp, int>)
      return u._M_i;
    else if constexpr (is_same_v<Tp, unsigned>)
      return u._M_u;
    else if constexpr (is_same_v<Tp, int64_t>)
      return u._M_ll;
    else if constexpr (is_same_v<Tp, uint64_t>)
      return u._M_ull;
    else if constexpr (is_same_v<Tp, float>)
      return u._M_flt;
    else if constexpr (is_same_v<Tp, double>)
      return u._M_dbl;
#ifndef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
    else if constexpr (is_same_v<Tp, long double>)
      return u._M_ldbl;
#else
    else if constexpr (is_same_v<_Tp, __ieee128>)
      return __u._M_f128;
    else if constexpr (is_same_v<_Tp, __ibm128>)
      return __u._M_ibm128;
#endif
    else if constexpr (is_same_v<Tp, const char_t*>)
      return u._M_str;
    else if constexpr (is_same_v<Tp, basic_string_view<char_t>>)
      return u._M_sv;
    else if constexpr (is_same_v<Tp, const void*>)
      return u._M_ptr;
#ifdef __SIZEOF_INT128__
    else if constexpr (is_same_v<Tp, __int128>)
      return u._M_i128;
    else if constexpr (is_same_v<Tp, unsigned __int128>)
      return u._M_u128;
#endif
#if _GLIBCXX_FORMAT_F128 == 2
    else if constexpr (is_same_v<_Tp, __float128_t>)
      return __u._M_f128;
#endif
    else if constexpr (derived_from<Tp, HandleBase>)
      return static_cast<Tp&>(u._M_handle);
    // Otherwise, ill-formed.
  }

  template <typename Tp>
  [[__gnu__::__always_inline__]]
  auto get() noexcept -> auto& {
    return get<Tp>(*this);
  }

  template <typename Tp>
  [[__gnu__::__always_inline__]]
  auto get() const noexcept -> const auto& {
    return get<Tp>(*this);
  }

  template <typename Tp>
  [[__gnu__::__always_inline__]]
  void set(Tp v) noexcept {
    if constexpr (derived_from<Tp, HandleBase>)
      std::construct_at(&handle, v);
    else
      get<Tp>(*this) = v;
  }
};

// [format.arg.store], class template format-arg-store
template <typename Context, typename... Args>
class ArgStore;

}  // namespace detail
/// @endcond

template <typename Context>
class BasicFormatArg {
  using CharT = typename Context::char_type;

  template <typename Tp>
  static constexpr bool formattable = detail::formattable_with<Tp, Context>;

 public:
  class Handle : public detail::ArgValue<Context>::HandleBase {
    using Base = typename detail::ArgValue<Context>::HandleBase;

    // Format as const if possible, to reduce instantiations.
    template <typename Tp>
    using maybe_const_t = __conditional_t<formattable<const Tp>, const Tp, Tp>;

    template <typename Tq>
    static void format(BasicFormatParseContext<CharT>& parse_ctx, Context& format_ctx, const void* ptr) {
      using Td = remove_const_t<Tq>;
      typename Context::template formatter_type<Td> f;
      parse_ctx.advance_to(f.parse(parse_ctx));
      Tq& val = *const_cast<Tq*>(static_cast<const Td*>(ptr));
      format_ctx.advance_to(f.format(val, format_ctx));
    }

    template <typename Tp>
    explicit Handle(Tp& val) noexcept {
      this->_M_ptr = __builtin_addressof(val);
      auto func = format<maybe_const_t<Tp>>;
      this->_M_func = reinterpret_cast<void (*)()>(func);
    }

    friend class BasicFormatArg<Context>;

   public:
    Handle(const Handle&) = default;
    auto operator=(const Handle&) -> Handle& = default;

    [[__gnu__::__always_inline__]]
    void format(BasicFormatParseContext<CharT>& pc, Context& fc) const {
      using Func = void (*)(BasicFormatParseContext<CharT>&, Context&, const void*);
      auto f = reinterpret_cast<Func>(this->func);
      f(pc, fc, this->ptr);
    }
  };

  [[__gnu__::__always_inline__]]
  BasicFormatArg() noexcept
      : type_(detail::ArgT::None) {}

  [[nodiscard, __gnu__::__always_inline__]]
  explicit operator bool() const noexcept {
    return type_ != detail::ArgT::None;
  }

 private:
  template <typename Ctx>
  friend class BasicFormatArgs;

  template <typename Ctx, typename... Args>
  friend class detail::ArgStore;

  static_assert(is_trivially_copyable_v<detail::ArgValue<Context>>);

  detail::ArgValue<Context> val_;
  detail::ArgT type_;

  // Transform incoming argument type to the type stored in _Arg_value.
  // e.g. short -> int, std::string -> std::string_view,
  // char[3] -> const char*.
  template <typename Tp>
  static consteval auto to_arg_type() {
    using Td = remove_const_t<Tp>;
    if constexpr (is_same_v<Td, bool>)
      return std::type_identity<bool>();
    else if constexpr (is_same_v<Td, CharT>)
      return std::type_identity<CharT>();
    else if constexpr (is_same_v<Td, char> && is_same_v<CharT, wchar_t>)
      return std::type_identity<CharT>();
#ifdef __SIZEOF_INT128__  // Check before signed/unsigned integer
    else if constexpr (is_same_v<Td, __int128>)
      return std::type_identity<__int128>();
    else if constexpr (is_same_v<Td, unsigned __int128>)
      return std::type_identity<unsigned __int128>();
#endif
    else if constexpr (__is_signed_integer<Td>::value) {
      if constexpr (sizeof(Td) <= sizeof(int))
        return std::type_identity<int>();
      else if constexpr (sizeof(Td) <= sizeof(int64_t))
        return std::type_identity<int64_t>();
    } else if constexpr (__is_unsigned_integer<Td>::value) {
      if constexpr (sizeof(Td) <= sizeof(unsigned))
        return std::type_identity<unsigned>();
      else if constexpr (sizeof(Td) <= sizeof(uint64_t))
        return std::type_identity<uint64_t>();
    } else if constexpr (is_same_v<Td, float>)
      return std::type_identity<float>();
    else if constexpr (is_same_v<Td, double>)
      return std::type_identity<double>();
#ifndef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
    else if constexpr (is_same_v<Td, long double>)
      return std::type_identity<long double>();
#else
    else if constexpr (is_same_v<_Td, __ibm128>)
      return std::type_identity<__ibm128>();
    else if constexpr (is_same_v<_Td, __ieee128>)
      return std::type_identity<__ieee128>();
#endif

#if defined(__FLT16_DIG__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
    else if constexpr (is_same_v<Td, _Float16>)
      return std::type_identity<float>();
#endif

#if defined(__BFLT16_DIG__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
    else if constexpr (is_same_v<_Td, decltype(0.0bf16)>)
      return std::type_identity<float>();
#endif

#ifdef __FLT32_DIG__
    else if constexpr (is_same_v<_Td, _Float32>)
#ifdef _GLIBCXX_FLOAT_IS_IEEE_BINARY32
      return std::type_identity<float>();
#else
      return std::type_identity<_Float32>();
#endif
#endif
#ifdef __FLT64_DIG__
    else if constexpr (is_same_v<_Td, _Float64>)
#ifdef _GLIBCXX_DOUBLE_IS_IEEE_BINARY64
      return std::type_identity<double>();
#else
      return std::type_identity<_Float64>();
#endif
#endif
#if _GLIBCXX_FORMAT_F128
#if __FLT128_DIG__
    else if constexpr (is_same_v<_Td, _Float128>)
      return std::type_identity<__format::__float128_t>();
#endif
#if __SIZEOF_FLOAT128__
    else if constexpr (is_same_v<_Td, __float128>)
      return std::type_identity<__format::__float128_t>();
#endif
#endif
    else if constexpr (is_specialization_of<Td, basic_string_view> || is_specialization_of<Td, basic_string>) {
      if constexpr (is_same_v<typename Td::value_type, CharT>)
        return std::type_identity<basic_string_view<CharT>>();
      else
        return std::type_identity<Handle>();
    } else if constexpr (std::is_same_v<decay_t<Td>, const CharT*>)
      return std::type_identity<const CharT*>();
    else if constexpr (std::is_same_v<decay_t<Td>, CharT*>)
      return std::type_identity<const CharT*>();
    else if constexpr (std::is_void_v<remove_pointer_t<Td>>)
      return std::type_identity<const void*>();
    else if constexpr (std::is_same_v<Td, nullptr_t>)
      return std::type_identity<const void*>();
    else
      return std::type_identity<Handle>();
  }

  // Transform a formattable type to the appropriate storage type.
  template <typename Tp>
  using Normalize = typename decltype(to_arg_type<Tp>())::type;

  // Get the _Arg_t value corresponding to a normalized type.
  template <typename Tp>
  static consteval auto to_enum() -> detail::ArgT {
    using namespace detail;
    if constexpr (std::is_same_v<Tp, bool>)
      return ArgT::Bool;
    else if constexpr (std::is_same_v<Tp, CharT>)
      return ArgT::C;
    else if constexpr (std::is_same_v<Tp, int>)
      return ArgT::I;
    else if constexpr (std::is_same_v<Tp, unsigned>)
      return ArgT::U;
    else if constexpr (std::is_same_v<Tp, int64_t>)
      return ArgT::Ll;
    else if constexpr (std::is_same_v<Tp, uint64_t>)
      return ArgT::Ull;
    else if constexpr (std::is_same_v<Tp, float>)
      return ArgT::Flt;
    else if constexpr (std::is_same_v<Tp, double>)
      return ArgT::Dbl;
#ifndef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
    else if constexpr (std::is_same_v<Tp, long double>)
      return ArgT::Ldbl;
#else
    // Don't use ldbl for this target, it's ambiguous.
    else if constexpr (std::is_same_v<_Tp, __ibm128>)
      return Ibm128;
    else if constexpr (std::is_same_v<_Tp, __ieee128>)
      return F128;
#endif
    else if constexpr (std::is_same_v<Tp, const CharT*>)
      return ArgT::Str;
    else if constexpr (std::is_same_v<Tp, basic_string_view<CharT>>)
      return ArgT::Sv;
    else if constexpr (std::is_same_v<Tp, const void*>)
      return ArgT::Ptr;
#ifdef __SIZEOF_INT128__
    else if constexpr (std::is_same_v<Tp, __int128>)
      return ArgT::I128;
    else if constexpr (std::is_same_v<Tp, unsigned __int128>)
      return ArgT::U128;
#endif

    // N.B. some of these types will never actually be used here,
    // because they get normalized to a standard floating-point type.
#if defined __FLT32_DIG__ && !_GLIBCXX_FLOAT_IS_IEEE_BINARY32
    else if constexpr (std::is_same_v<_Tp, _Float32>)
      return _Arg_f32;
#endif
#if defined __FLT64_DIG__ && !_GLIBCXX_DOUBLE_IS_IEEE_BINARY64
    else if constexpr (std::is_same_v<_Tp, _Float64>)
      return _Arg_f64;
#endif
#if _GLIBCXX_FORMAT_F128 == 2
    else if constexpr (std::is_same_v<_Tp, __format::__float128_t>)
      return _Arg_f128;
#endif
    else if constexpr (std::is_same_v<Tp, Handle>)
      return ArgT::Handle;
  }

  template <typename Tp>
  void set(Tp v) noexcept {
    type_ = to_enum<Tp>();
    val_.set(v);
  }

  template <typename Tp>
    requires detail::formattable_with<Tp, Context>
  explicit BasicFormatArg(Tp& v) noexcept {
    using Td = Normalize<Tp>;
    if constexpr (std::is_same_v<Td, basic_string_view<CharT>>)
      set(Td{v.data(), v.size()});
    else if constexpr (std::is_same_v<remove_const_t<Tp>, char> && std::is_same_v<CharT, wchar_t>)
      set(static_cast<Td>(static_cast<unsigned char>(v)));
    else
      set(static_cast<Td>(v));
  }

  template <typename Ctx, typename... Argz>
  friend auto make_format_args(Argz&...) noexcept;

  template <typename Visitor, typename Ctx>
  friend auto visit_format_arg(Visitor&& vis, BasicFormatArg<Ctx>) -> decltype(auto);

  template <typename Visitor>
  auto visit(Visitor&& vis, detail::ArgT type) -> decltype(auto) {
    using namespace detail;
    switch (type) {
      case ArgT::None:
        return std::forward<Visitor>(vis)(val_.none);
      case ArgT::Bool:
        return std::forward<Visitor>(vis)(val_.boolean);
      case ArgT::C:
        return std::forward<Visitor>(vis)(val_.c);
      case ArgT::I:
        return std::forward<Visitor>(vis)(val_.i);
      case ArgT::U:
        return std::forward<Visitor>(vis)(val_.u);
      case ArgT::Ll:
        return std::forward<Visitor>(vis)(val_.ll);
      case ArgT::Ull:
        return std::forward<Visitor>(vis)(val_.ull);
#if __glibcxx_to_chars  // FIXME: need to be able to format these types!
      case ArgT::Flt:
        return std::forward<Visitor>(vis)(val_.flt);
      case ArgT::Dbl:
        return std::forward<Visitor>(vis)(val_.dbl);
#ifndef _GLIBCXX_LONG_DOUBLE_ALT128_COMPAT
      case ArgT::Ldbl:
        return std::forward<Visitor>(vis)(val_.ldbl);
#else
      case ArgT::F128:
        return std::forward<Visitor>(vis)(val.f128);
      case ArgT::Ibm128:
        return std::forward<Visitor>(vis)(val.ibm128);
#endif
#endif
      case ArgT::Str:
        return std::forward<Visitor>(vis)(val_.str);
      case ArgT::Sv:
        return std::forward<Visitor>(vis)(val_.sv);
      case ArgT::Ptr:
        return std::forward<Visitor>(vis)(val_.ptr);
      case ArgT::Handle: {
        auto& h = static_cast<Handle&>(val_.handle);
        return std::forward<Visitor>(vis)(h);
      }
#ifdef __SIZEOF_INT128__
      case ArgT::I128:
        return std::forward<Visitor>(vis)(val_.i128);
      case ArgT::U128:
        return std::forward<Visitor>(vis)(val_.u128);
#endif

#if _GLIBCXX_FORMAT_F128 == 2
      case _Arg_f128:
        return std::forward<Visitor>(vis)(val.f128);
#endif

      default:
        // _Arg_f16 etc.
        __builtin_unreachable();
    }
  }
};

template <typename Visitor, typename Context>
inline auto visit_format_arg(Visitor&& vis, BasicFormatArg<Context> arg) -> decltype(auto) {
  return arg.visit(std::forward<Visitor>(vis), arg.type_);
}

/// @cond undocumented
namespace detail {

struct WidthPrecVisitor {
  template <typename Tp>
  auto operator()(Tp& arg) const -> size_t {
    if constexpr (is_same_v<Tp, monostate>) detail::invalid_arg_id_in_format_string();
    // _GLIBCXX_RESOLVE_LIB_DEFECTS
    // 3720. Restrict the valid types of arg-id for width and precision
    // 3721. Allow an arg-id with a value of zero for width
    else if constexpr (sizeof(Tp) <= sizeof(int64_t)) {
      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 3720. Restrict the valid types of arg-id for width and precision
      if constexpr (__is_unsigned_integer<Tp>::value)
        return arg;
      else if constexpr (__is_signed_integer<Tp>::value)
        if (arg >= 0) return arg;
    }
    throw_format_error("format error: argument used for width or precision must be a non-negative integer");
  }
};

template <typename Context>
inline auto int_from_arg(const BasicFormatArg<Context>& arg) -> size_t {
  return std::visit_format_arg(WidthPrecVisitor(), arg);
}

// Pack _Arg_t enum values into a single 60-bit integer.
template <int Bits, size_t Nm>
constexpr auto pack_arg_types(const array<ArgT, Nm>& types) {
  uint64_t packed_types = 0;
  for (auto i = types.rbegin(); i != types.rend(); ++i) packed_types = (packed_types << Bits) | *i;
  return packed_types;
}
}  // namespace detail
/// @endcond

template <typename Context>
class BasicFormatArgs {
  static constexpr int packed_type_bits = 5;  // _Arg_t values [0,20]
  static constexpr int packed_type_mask = 0b11111;
  static constexpr int max_packed_args = 12;

  static_assert(std::to_underlying(detail::ArgT::Max) <= (1 << packed_type_bits));

  template <typename... Args>
  using Store = detail::ArgStore<Context, Args...>;

  template <typename Ctx, typename... Args>
  friend class detail::ArgStore;

  using uint64_t = __UINT64_TYPE__;
  using FormatArg = BasicFormatArg<Context>;
  using FormatArgVal = detail::ArgValue<Context>;

  // If args are packed then the number of args is in _M_packed_size and
  // the packed types are in _M_unpacked_size, accessed via _M_type(i).
  // If args are not packed then the number of args is in _M_unpacked_size
  // and _M_packed_size is zero.
  uint64_t packed_size_ : 4;
  uint64_t unpacked_size_ : 60;

  union {
    const FormatArgVal* values;  // Active when _M_packed_size != 0
    const FormatArg* args;       // Active when _M_packed_size == 0
  };

  [[nodiscard]] auto size() const noexcept -> size_t { return (packed_size_ != 0U) ? packed_size_ : unpacked_size_; }

  [[nodiscard]] auto type(size_t i) const noexcept -> typename detail::ArgT {
    uint64_t t = unpacked_size_ >> (i * packed_type_bits);
    return static_cast<detail::ArgT>(t & packed_type_mask);
  }

  template <typename Ctx, typename... Args>
  friend auto make_format_args(Args&...) noexcept;

  // An array of _Arg_t enums corresponding to _Args...
  template <typename... Args>
  static consteval auto types_to_pack() -> array<detail::ArgT, sizeof...(Args)> {
    return {FormatArg::template to_enum<Args>()...};
  }

 public:
  template <typename... Args>
  explicit BasicFormatArgs(const Store<Args...>& store) noexcept;

  [[nodiscard, __gnu__::__always_inline__]]
  auto get(size_t i) const noexcept -> BasicFormatArg<Context> {
    BasicFormatArg<Context> arg;
    if (i < packed_size_) {
      arg.type_ = type(i);
      arg.val_ = values[i];
    } else if (packed_size_ == 0 && i < unpacked_size_)
      arg = args[i];
    return arg;
  }
};

// _GLIBCXX_RESOLVE_LIB_DEFECTS
// 3810. CTAD for std::basic_format_args
template <typename Context, typename... Args>
BasicFormatArgs(detail::ArgStore<Context, Args...>) -> BasicFormatArgs<Context>;

template <typename Context, typename... Args>
auto make_format_args(Args&... fmt_args) noexcept;

// An array of type-erased formatting arguments.
template <typename Context, typename... Args>
class detail::ArgStore {
  friend std::BasicFormatArgs<Context>;

  template <typename Ctx, typename... Argz>
  friend auto std::
#if _GLIBCXX_INLINE_VERSION
      __8::  // Needed for PR c++/59256
#endif
      make_format_args(Argz&...) noexcept;

  // For a sufficiently small number of arguments we only store values.
  // basic_format_args can get the types from the _Args pack.
  static constexpr bool values_only = sizeof...(Args) <= BasicFormatArgs<Context>::max_packed_args;

  using Element = std::conditional_t<values_only, detail::ArgValue<Context>, BasicFormatArg<Context>>;

  Element args_[sizeof...(Args)];

  template <typename Tp>
  static auto make_element(Tp& v) -> Element {
    using Tq = remove_const_t<Tp>;
    using CharT = typename Context::char_type;
    static_assert(std::is_default_constructible_v<Formatter<Tq, CharT>>,
                  "std::formatter must be specialized for the type of each format arg");
    using detail::formattable_with;
    if constexpr (is_const_v<Tp>)
      if constexpr (!formattable_with<Tp, Context>)
        if constexpr (formattable_with<Tq, Context>)
          static_assert(formattable_with<Tp, Context>,
                        "format arg must be non-const because its std::formatter specialization has a non-const "
                        "reference parameter");
    BasicFormatArg<Context> arg(v);
    if constexpr (values_only)
      return arg.val_;
    else
      return arg;
  }

  template <typename... Tp>
    requires(sizeof...(Tp) == sizeof...(Args))
  [[__gnu__::__always_inline__]] explicit ArgStore(Tp&... a) noexcept : args_{make_element(a)...} {}
};

template <typename Context>
class detail::ArgStore<Context> {};

template <typename Context>
template <typename... Args>
inline BasicFormatArgs<Context>::BasicFormatArgs(const Store<Args...>& store) noexcept {
  if constexpr (sizeof...(Args) == 0) {
    packed_size_ = 0;
    unpacked_size_ = 0;
    args = nullptr;
  } else if constexpr (sizeof...(Args) <= max_packed_args) {
    // The number of packed arguments:
    packed_size_ = sizeof...(Args);
    // The packed type enums:
    unpacked_size_ = detail::pack_arg_types<packed_type_bits>(types_to_pack<Args...>());
    // The _Arg_value objects.
    values = store.args_;
  } else {
    // No packed arguments:
    packed_size_ = 0;
    // The number of unpacked arguments:
    unpacked_size_ = sizeof...(Args);
    // The basic_format_arg objects:
    args = store.args_;
  }
}

/// Capture formatting arguments for use by `std::vformat`.
template <typename Context = format_context, typename... Args>
[[nodiscard, __gnu__::__always_inline__]]
inline auto make_format_args(Args&... fmt_args) noexcept {
  using fmt_arg = BasicFormatArg<Context>;
  using store = detail::ArgStore<Context, typename fmt_arg::template _Normalize<Args>...>;
  return store(fmt_args...);
}

/// @cond undocumented
namespace detail {

template <typename Out, typename CharT, typename Context>
auto do_vformat_to(Out /*out*/, basic_string_view<CharT> /*fmt*/, const BasicFormatArgs<Context>& /*args*/,
                   const locale* /*loc*/ = nullptr) -> Out;

}  // namespace detail
/// @endcond

/** Context for std::format and similar functions.
 *
 * A formatting context contains an output iterator and locale to use
 * for the formatting operations. Most programs will never need to use
 * this class template explicitly. For typical uses of `std::format` the
 * library will use the specializations `std::format_context` (for `char`)
 * and `std::wformat_context` (for `wchar_t`).
 */
template <typename Out, typename CharT>
class BasicFormatContext {
  static_assert(output_iterator<Out, const CharT&>);

  BasicFormatArgs<BasicFormatContext> args_;
  Out out_;
  detail::OptionalLocale loc_;

  BasicFormatContext(BasicFormatArgs<BasicFormatContext> args, Out out) : args_(args), out_(std::move(out)) {}

  BasicFormatContext(BasicFormatArgs<BasicFormatContext> args, Out out, const std::locale& loc)
      : args_(args), out_(std::move(out)), loc_(loc) {}

  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 4061. Should std::basic_format_context be
  //       default-constructible/copyable/movable?
  BasicFormatContext(const BasicFormatContext&) = delete;
  auto operator=(const BasicFormatContext&) -> BasicFormatContext& = delete;

  template <typename Out2, typename CharT2, typename Context2>
  friend auto detail::do_vformat_to(Out2, basic_string_view<CharT2>, const BasicFormatArgs<Context2>&, const locale*)
      -> Out2;

 public:
  ~BasicFormatContext() = default;

  using iterator = Out;
  using char_type = CharT;
  template <typename Tp>
  using formatter_type = Formatter<Tp, CharT>;

  [[nodiscard]]
  auto arg(size_t id) const noexcept -> BasicFormatArg<BasicFormatContext> {
    return args_.get(id);
  }

  [[nodiscard]]
  auto locale() -> std::locale {
    return loc_.value();
  }

  [[nodiscard]]
  auto out() -> iterator {
    return std::move(out_);
  }

  void advance_to(iterator it) { out_ = std::move(it); }
};

/// @cond undocumented
namespace detail {
// Abstract base class defining an interface for scanning format strings.
// Scan the characters in a format string, dividing it up into strings of
// ordinary characters, escape sequences, and replacement fields.
// Call virtual functions for derived classes to parse format-specifiers
// or write formatted output.
template <typename CharT>
struct Scanner {
  using iterator = typename BasicFormatParseContext<CharT>::iterator;

  BasicFormatParseContext<CharT> pc;

  constexpr explicit Scanner(basic_string_view<CharT> str, size_t nargs = static_cast<size_t>(-1)) : pc(str, nargs) {}

  [[nodiscard]] constexpr auto begin() const noexcept -> iterator { return pc.begin(); }
  [[nodiscard]] constexpr auto end() const noexcept -> iterator { return pc.end(); }

  constexpr void scan() {
    basic_string_view<CharT> fmt = fmt_str();

    if (fmt.size() == 2 && fmt[0] == '{' && fmt[1] == '}') {
      pc.advance_to(begin() + 1);
      format_arg(pc.next_arg_id());
      return;
    }

    size_t lbr = fmt.find('{');
    size_t rbr = fmt.find('}');

    while (fmt.size()) {
      auto cmp = lbr <=> rbr;
      if (cmp == 0) {
        on_chars(end());
        pc.advance_to(end());
        return;
      }

      if (cmp < 0) {
        if (lbr + 1 == fmt.size() || (rbr == fmt.npos && fmt[lbr + 1] != '{'))
          detail::unmatched_left_brace_in_format_string();
        const bool is_escape = fmt[lbr + 1] == '{';
        iterator last = begin() + lbr + static_cast<int>(is_escape);
        on_chars(last);
        pc.advance_to(last + 1);
        fmt = fmt_str();
        if (is_escape) {
          if (rbr != fmt.npos) rbr -= lbr + 2;
          lbr = fmt.find('{');
        } else {
          on_replacement_field();
          fmt = fmt_str();
          lbr = fmt.find('{');
          rbr = fmt.find('}');
        }
      } else {
        if (++rbr == fmt.size() || fmt[rbr] != '}') detail::unmatched_right_brace_in_format_string();
        iterator last = begin() + rbr;
        on_chars(last);
        pc.advance_to(last + 1);
        fmt = fmt_str();
        if (lbr != fmt.npos) lbr -= rbr + 1;
        rbr = fmt.find('}');
      }
    }
  }

  [[nodiscard]] constexpr auto fmt_str() const noexcept -> basic_string_view<CharT> { return {begin(), end()}; }

  constexpr virtual void on_chars(iterator /*unused*/) {}

  constexpr void on_replacement_field() {
    auto next = begin();

    size_t id;
    if (*next == '}')
      id = pc.next_arg_id();
    else if (*next == ':') {
      id = pc.next_arg_id();
      pc.advance_to(++next);
    } else {
      auto [i, ptr] = detail::parse_arg_id(begin(), end());
      if (!ptr || !(*ptr == '}' || *ptr == ':')) detail::invalid_arg_id_in_format_string();
      pc.check_arg_id(id = i);
      if (*ptr == ':') {
        pc.advance_to(++ptr);
      } else
        pc.advance_to(ptr);
    }
    format_arg(id);
    if (begin() == end() || *begin() != '}') detail::unmatched_left_brace_in_format_string();
    pc.advance_to(begin() + 1);  // Move past '}'
  }

  constexpr virtual void format_arg(size_t id) = 0;
};

// Process a format string and format the arguments in the context.
template <typename Out, typename CharT>
class DetailtingScanner : public Scanner<CharT> {
 public:
  DetailtingScanner(BasicFormatContext<Out, CharT>& fc, basic_string_view<CharT> str) : Scanner<CharT>(str), fc_(fc) {}

 private:
  BasicFormatContext<Out, CharT>& fc_;

  using iterator = typename Scanner<CharT>::iterator;

  constexpr void on_chars(iterator last) override {
    basic_string_view<CharT> str(this->begin(), last);
    fc_.advance_to(detail::write(fc_.out(), str));
  }

  constexpr void format_arg(size_t id) override {
    using Context = BasicFormatContext<Out, CharT>;
    using Handle = typename BasicFormatArg<Context>::Handle;

    std::visit_format_arg(
        [this](auto& arg) -> auto {
          using Type = remove_reference_t<decltype(arg)>;
          using Formatter = typename Context::template formatter_type<Type>;
          if constexpr (is_same_v<Type, monostate>)
            detail::invalid_arg_id_in_format_string();
          else if constexpr (is_same_v<Type, Handle>)
            arg.format(this->pc, this->fc_);
          else if constexpr (std::is_default_constructible_v<Formatter>) {
            Formatter f;
            this->pc.advance_to(f.parse(this->pc));
            this->fc_.advance_to(f.format(arg, this->fc_));
          } else
            static_assert(detail::formattable_with<Type, Context>);
        },
        fc_.arg(id));
  }
};

// Validate a format string for Args.
template <typename CharT, typename... Args>
class CheckingScanner : public Scanner<CharT> {
  static_assert((is_default_constructible_v<Formatter<Args, CharT>> && ...),
                "std::formatter must be specialized for each type being formatted");

 public:
  constexpr explicit CheckingScanner(basic_string_view<CharT> str) : Scanner<CharT>(str, sizeof...(Args)) {}

 private:
  constexpr void format_arg(size_t id) override {
    if constexpr (sizeof...(Args) != 0) {
      if (id < sizeof...(Args)) {
        parse_format_spec<Args...>(id);
        return;
      }
    }
    __builtin_unreachable();
  }

  template <typename Tp, typename... OtherArgs>
  constexpr void parse_format_spec(size_t id) {
    if (id == 0) {
      Formatter<Tp, CharT> f;
      this->pc.advance_to(f.parse(this->pc));
    } else if constexpr (sizeof...(OtherArgs) != 0)
      parse_format_spec<OtherArgs...>(id - 1);
    else
      __builtin_unreachable();
  }
};

template <typename Out, typename CharT, typename Context>
inline auto do_vformat_to(Out out, basic_string_view<CharT> fmt, const BasicFormatArgs<Context>& args,
                          const locale* loc) -> Out {
  IterSink<CharT, Out> sink(std::move(out));
  SinkIter<CharT> sink_out;

  if constexpr (is_same_v<Out, SinkIter<CharT>>)
    sink_out = out;  // Already a sink iterator, safe to use post-move.
  else
    sink_out = sink.out();

  if constexpr (is_same_v<CharT, char>)
    // Fast path for "{}" format strings and simple format arg types.
    if (fmt.size() == 2 && fmt[0] == '{' && fmt[1] == '}') {
      bool done = false;
      std::visit_format_arg(
          [&](auto& arg) -> auto {
            using tp = remove_cvref_t<decltype(arg)>;
            if constexpr (is_same_v<tp, bool>) {
              size_t len = 4 + !arg;
              const char* chars[] = {"false", "true"};
              if (auto res = sink_out.reserve(len)) {
                __builtin_memcpy(res.get(), chars[arg], len);
                res.bump(len);
                done = true;
              }
            } else if constexpr (is_same_v<tp, char>) {
              if (auto res = sink_out.reserve(1)) {
                *res.get() = arg;
                res.bump(1);
                done = true;
              }
            } else if constexpr (is_integral_v<tp>) {
              make_unsigned_t<tp> uval;
              const bool neg = arg < 0;
              if (neg)
                uval = make_unsigned_t<tp>(~arg) + 1U;
              else
                uval = arg;
              const auto n = __detail::__to_chars_len(uval);
              if (auto res = sink_out.reserve(n + neg)) {
                auto ptr = res.get();
                *ptr = '-';
                __detail::__to_chars_10_impl(ptr + (int)neg, n, uval);
                res.bump(n + neg);
                done = true;
              }
            } else if constexpr (is_convertible_v<tp, string_view>) {
              string_view sv = arg;
              if (auto res = sink_out.reserve(sv.size())) {
                __builtin_memcpy(res.get(), sv.data(), sv.size());
                res.bump(sv.size());
                done = true;
              }
            }
          },
          args.get(0));

      if (done) {
        if constexpr (is_same_v<Out, SinkIter<CharT>>)
          return sink_out;
        else
          return std::move(sink)._M_finish().out;
      }
    }

  auto ctx = loc == nullptr ? Context(args, sink_out) : Context(args, sink_out, *loc);
  DetailtingScanner<SinkIter<CharT>, CharT> scanner(ctx, fmt);
  scanner.scan();

  if constexpr (is_same_v<Out, SinkIter<CharT>>)
    return ctx.out();
  else
    return std::move(sink)._M_finish().out;
}

}  // namespace detail
/// @endcond

template <typename CharT, typename... Args>
template <typename Tp>
  requires convertible_to<const Tp&, std::basic_string_view<CharT>>
consteval BasicFormatString<CharT, Args...>::BasicFormatString(const Tp& s) : str_(s) {
  detail::CheckingScanner<CharT, remove_cvref_t<Args>...> scanner(str_);
  scanner.scan();
}

// [format.functions], formatting functions

template <typename Out>
  requires output_iterator<Out, const char&>
[[__gnu__::__always_inline__]]
inline auto vformat_to(Out out, string_view fmt, format_args args) -> Out {
  return detail::do_vformat_to(std::move(out), fmt, args);
}

template <typename Out>
  requires output_iterator<Out, const char&>
[[__gnu__::__always_inline__]]
inline auto vformat_to(Out out, const locale& loc, string_view fmt, format_args args) -> Out {
  return detail::do_vformat_to(std::move(out), fmt, args, &loc);
}

[[nodiscard]]
inline auto vformat(string_view fmt, format_args args) -> string {
  detail::StrSink<char> buf;
  std::vformat_to(buf.out(), fmt, args);
  return std::move(buf).get();
}

[[nodiscard]]
inline auto vformat(const locale& loc, string_view fmt, format_args args) -> string {
  detail::StrSink<char> buf;
  std::vformat_to(buf.out(), loc, fmt, args);
  return std::move(buf).get();
}

template <typename... Args>
[[nodiscard]]
inline auto format(format_string<Args...> fmt, Args&&... args) -> string {
  return std::vformat(fmt.get(), std::make_format_args(args...));
}

template <typename... Args>
[[nodiscard]]
inline auto format(const locale& loc, format_string<Args...> fmt, Args&&... args) -> string {
  return std::vformat(loc, fmt.get(), std::make_format_args(args...));
}

template <typename Out, typename... Args>
  requires output_iterator<Out, const char&>
inline auto format_to(Out out, format_string<Args...> fmt, Args&&... args) -> Out {
  return std::vformat_to(std::move(out), fmt.get(), std::make_format_args(args...));
}

template <typename Out, typename... Args>
  requires output_iterator<Out, const char&>
inline auto format_to(Out out, const locale& loc, format_string<Args...> fmt, Args&&... args) -> Out {
  return std::vformat_to(std::move(out), loc, fmt.get(), std::make_format_args(args...));
}

template <typename Out, typename... Args>
  requires output_iterator<Out, const char&>
inline auto format_to_n(Out out, iter_difference_t<Out> n, format_string<Args...> fmt, Args&&... args)
    -> FormatToNResult<Out> {
  detail::IterSink<char, Out> sink(std::move(out), n);
  std::vformat_to(sink.out(), fmt.get(), std::make_format_args(args...));
  return std::move(sink)._M_finish();
}

template <typename Out, typename... Args>
  requires output_iterator<Out, const char&>
inline auto format_to_n(Out out, iter_difference_t<Out> n, const locale& loc, format_string<Args...> fmt,
                        Args&&... args) -> FormatToNResult<Out> {
  detail::IterSink<char, Out> sink(std::move(out), n);
  std::vformat_to(sink.out(), loc, fmt.get(), std::make_format_args(args...));
  return std::move(sink)._M_finish();
}

/// @cond undocumented
namespace detail {
template <typename CharT>
class CountingSink final : public IterSink<CharT, CharT*> {
 public:
  CountingSink() : IterSink<CharT, CharT*>(nullptr, 0) {}

  [[__gnu__::__always_inline__]] [[nodiscard]] auto count() const -> size_t {
    return this->count_ + this->used().size();
  }
};
}  // namespace detail
/// @endcond

template <typename... Args>
[[nodiscard]]
inline auto formatted_size(format_string<Args...> fmt, Args&&... args) -> size_t {
  detail::CountingSink<char> buf;
  std::vformat_to(buf.out(), fmt.get(), std::make_format_args(args...));
  return buf.count();
}

template <typename... Args>
[[nodiscard]]
inline auto formatted_size(const locale& loc, format_string<Args...> fmt, Args&&... args) -> size_t {
  detail::CountingSink<char> buf;
  std::vformat_to(buf.out(), loc, fmt.get(), std::make_format_args(args...));
  return buf.count();
}

// [format.range], formatting of ranges
// [format.range.fmtkind], variable template format_kind
enum class RangeFormat : uint8_t { Disabled, Map, Set, Sequence, String, DebugString };

/// @cond undocumented
template <typename Rg>
constexpr auto format_kind = not defined(format_kind<Rg>);

template <typename Tp>
consteval auto fmt_kind() -> RangeFormat {
  using ref = ranges::range_reference_t<Tp>;
  if constexpr (is_same_v<remove_cvref_t<ref>, Tp>)
    return RangeFormat::Disabled;
  else if constexpr (requires { typename Tp::key_type; }) {
    if constexpr (requires { typename Tp::mapped_type; }) {
      using up = remove_cvref_t<ref>;
      if constexpr (__is_pair<up>)
        return RangeFormat::Map;
      else if constexpr (std::is_specialization_of<up, tuple>)
        if constexpr (tuple_size_v<up> == 2) return RangeFormat::Map;
    }
    return RangeFormat::Set;
  } else
    return RangeFormat::Sequence;
}
/// @endcond

/// A constant determining how a range should be formatted.
template <ranges::input_range Rg>
  requires same_as<Rg, remove_cvref_t<Rg>>
constexpr RangeFormat format_kind<Rg> = fmt_kind<Rg>();

// [format.range.formatter], class template range_formatter
template <typename Tp, typename CharT = char>
  requires same_as<remove_cvref_t<Tp>, Tp> && formattable<Tp, CharT>
class RangeFormatter;  // TODO

/// @cond undocumented
namespace detail {
// [format.range.fmtdef], class template range-default-formatter
template <RangeFormat Kind, ranges::input_range Rg, typename CharT>
struct RangeDefaultFormatter;  // TODO
}  // namespace detail
/// @endcond

// [format.range.fmtmap], [format.range.fmtset], [format.range.fmtstr],
// specializations for maps, sets, and strings
template <ranges::input_range Rg, typename CharT>
  requires(format_kind<Rg> != RangeFormat::Disabled) && formattable<std::ranges::range_reference_t<Rg>, CharT>
struct Formatter<Rg, CharT> : detail::RangeDefaultFormatter<format_kind<Rg>, Rg, CharT> {};

}  // namespace std _GLIBCXX_VISIBILITY(default)
