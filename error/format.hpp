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

#include <algorithm>  // ranges::copy
#include <array>
#include <cassert>
#include <cctype>
#include <charconv>
#include <concepts>
#include <cstdint>
#include <cstring>
#include <limits>
#include <locale>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
#include <variant>  // monostate
#include <vector>

namespace nostd {

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

// Output iterator that writes to a type-erased character sink.
template <typename CharT>
class SinkIter;

template <typename CharT>
using format_context = BasicFormatContext<SinkIter<CharT>, CharT>;

template <typename CharT>
struct RuntimeFormatString {
  [[__gnu__::__always_inline__]]
  explicit RuntimeFormatString(std::basic_string_view<CharT> s) noexcept
      : str_(s) {}

  RuntimeFormatString(const RuntimeFormatString&) = delete;
  void operator=(const RuntimeFormatString&) = delete;

 private:
  std::basic_string_view<CharT> str_;

  template <typename, typename...>
  friend struct nostd::BasicFormatString;
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
    requires std::convertible_to<const Tp&, std::basic_string_view<CharT>>
  consteval BasicFormatString(const Tp& s);  // NOLINT(google-explicit-constructor)

  [[__gnu__::__always_inline__]]
  explicit BasicFormatString(detail::RuntimeFormatString<CharT> s) noexcept
      : str_(s._M_str) {}

  [[__gnu__::__always_inline__]]
  constexpr auto get() const noexcept -> std::basic_string_view<CharT> {
    return str_;
  }

 private:
  std::basic_string_view<CharT> str_;
};

template <typename... Args>
using format_string = BasicFormatString<char, std::type_identity_t<Args>...>;

// [format.formatter], formatter

/// The primary template of std::formatter is disabled.
template <typename Tp, typename CharT = char>
struct Formatter {
  Formatter() = delete;  // No std::formatter specialization for this type.
  Formatter(const Formatter&) = delete;
  auto operator=(const Formatter&) -> Formatter& = delete;
};

// [format.error], class format_error
class FormatError : public std::runtime_error {
 public:
  explicit FormatError(const std::string& what) : runtime_error(what) {}
  explicit FormatError(const char* what) : runtime_error(what) {}
};

/// @cond undocumented
[[noreturn]]
constexpr void throw_format_error(const char* what) {
  throw FormatError{what};
}

namespace detail {
// XXX use named functions for each constexpr error?

[[noreturn]]
constexpr void unmatched_left_brace_in_format_string() {
  throw_format_error("format error: unmatched '{' in format string");
}

[[noreturn]]
constexpr void unmatched_right_brace_in_format_string() {
  throw_format_error("format error: unmatched '}' in format string");
}

[[noreturn]]
constexpr void conflicting_indexing_in_format_string() {
  throw_format_error("format error: conflicting indexing style in format string");
}

[[noreturn]]
constexpr void invalid_arg_id_in_format_string() {
  throw_format_error("format error: invalid arg-id in format string");
}

[[noreturn]]
constexpr void failed_to_parse_format_spec() {
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
  using const_iterator = typename std::basic_string_view<CharT>::const_iterator;
  using iterator = const_iterator;

  constexpr explicit BasicFormatParseContext(std::basic_string_view<CharT> fmt, size_t num_args = 0) noexcept
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
constexpr auto parse_integer(const CharT* first, const CharT* last) -> std::pair<uint16_t, const CharT*> {
  if (first == last) std::unreachable();

  if constexpr (std::is_same_v<CharT, char>) {
    const auto start = first;
    uint16_t val = 0;
    // N.B. std::from_chars is not constexpr in C++20.
    if (std::from_chars(first, last, val, 10).ec == std::errc{} && first != start) [[likely]]
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
constexpr auto parse_arg_id(const CharT* first, const CharT* last) -> std::pair<uint16_t, const CharT*> {
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
  b = 2,
  B = 3,
  o = 4,
  x = 5,
  X = 6,
  c = 7,
  // Presentation types for floating-point types.
  a = 1,
  A = 2,
  e = 3,
  E = 4,
  f = 5,
  F = 6,
  g = 7,
  G = 8,
  p = 0,
  P = 1,      // For pointers.
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
  Minus,  // XXX does this need to be distinct from Default?
  Space,
};

template <std::integral Int>
constexpr auto operator||(Sign lhs, Int rhs) -> bool {
  return static_cast<std::underlying_type_t<Sign>>(lhs) || rhs;
}

enum class WidthPrec : std::uint8_t {
  None,    // No width/prec specified.
  Value,   // Fixed width/prec specified.
  FromArg  // Use a formatting argument for width/prec.
};

template <typename Context>
constexpr auto int_from_arg(const BasicFormatArg<Context>& arg) -> size_t;

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

  using iterator = typename std::basic_string_view<CharT>::iterator;

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
      if (last - first >= 2) {
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
    if (*first == '0') throw_format_error("format error: width must be non-zero in format string");

    bool arg_id = false;
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
  constexpr auto get_width(Context& ctx) const -> size_t {
    switch (width_kind) {
      case WidthPrec::Value:
        return width;
      case WidthPrec::FromArg:
        return detail::int_from_arg(ctx.arg(width));
      case WidthPrec::None:
        return 0;
    }
    std::unreachable();
  }

  template <typename Context>
  constexpr auto get_precision(Context& ctx) const -> size_t {
    switch (prec_kind) {
      case WidthPrec::Value:
        return prec;
      case WidthPrec::FromArg:
        return detail::int_from_arg(ctx.arg(prec));
      case WidthPrec::None:
        return std::numeric_limits<size_t>::max();
    }
    std::unreachable();
  }
};

template <typename Int>
constexpr auto put_sign(Int i, Sign sign, char* dest) noexcept -> char* {
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
  requires std::output_iterator<Out, const CharT&>
constexpr auto write(Out out, std::basic_string_view<CharT> str) -> Out {
  if constexpr (std::is_same_v<Out, SinkIter<CharT>>) {
    if (!str.empty()) out = str;
  } else {
    for (CharT c : str) *out++ = c;
  }
  return out;
}

// Write STR to OUT with NFILL copies of FILL_CHAR specified by ALIGN.
// pre: align != Align::Default
template <typename Out, typename CharT>
constexpr auto write_padded(Out out, std::basic_string_view<CharT> str, Align align, size_t nfill, char32_t fill_char)
    -> Out {
  assert(align != Align::Default);

  constexpr size_t buflen = 32;
  std::array<CharT, buflen> padding_chars;
  padding_chars[0] = CharT{};
  std::basic_string_view<CharT> padding{padding_chars.data(), padding_chars.size()};

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
  switch (align) {
    case Align::Centre:
      l = nfill / 2;
      r = l + (nfill & 1);
      max = r;
      break;
    case Align::Right:
      l = nfill;
      r = 0;
      max = l;
      break;
    case Align::Left:
      l = 0;
      r = nfill;
      max = r;
      break;
    default:
      std::unreachable();
  }

  if (max < buflen)
    padding.remove_suffix(buflen - max);
  else
    max = buflen;

  std::char_traits<CharT>::assign(padding_chars.data(), max, fill_char);
  pad(l, out);
  out = detail::write(std::move(out), str);
  pad(r, out);

  return out;
}

// Write STR to OUT, with alignment and padding as determined by SPEC.
// pre: spec.align != Align::Default || align != Align::Default
template <typename CharT, typename Out>
constexpr auto write_padded_as_spec(std::basic_string_view<std::type_identity_t<CharT>> str,
                                    std::size_t estimated_width, BasicFormatContext<Out, CharT>& ctx,
                                    const Spec<CharT>& spec, Align alignement = Align::Left) -> Out {
  size_t width = spec.get_width(ctx);

  if (width <= estimated_width) return detail::write(ctx.out(), str);

  const size_t nfill = width - estimated_width;

  if (spec.align != Align::Default) alignement = spec.align;

  return detail::write_padded(ctx.out(), str, alignement, nfill, spec.fill);
}

// A lightweight optional<locale>.
struct OptionalLocale {
  constexpr OptionalLocale() = default;

  constexpr explicit OptionalLocale(const std::locale& loc) noexcept : loc(loc), hasval(true) {}

  constexpr OptionalLocale(const OptionalLocale& l) noexcept : hasval(l.hasval) {
    if (hasval) std::construct_at(&loc, l.loc);
  }

  constexpr auto operator=(const OptionalLocale& l) noexcept -> OptionalLocale& {
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

  constexpr ~OptionalLocale() {
    if (hasval) loc.~locale();
  }

  constexpr auto operator=(std::locale&& iloc) noexcept -> OptionalLocale& {
    if (hasval)
      loc = iloc;
    else {
      std::construct_at(&loc, std::move(iloc));
      hasval = true;
    }
    return *this;
  }

  constexpr auto value() noexcept -> const std::locale& {
    if (!hasval) {
      std::construct_at(&loc);
      hasval = true;
    }
    return loc;
  }

  [[nodiscard]] constexpr auto has_value() const noexcept -> bool { return hasval; }

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

    auto finished = [&] -> bool {
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
  constexpr auto format(std::basic_string_view<CharT> sv, BasicFormatContext<Out, CharT>& fc) const -> Out {
    if (spec_.type == PresType::Esc) {
      // TODO: C++23 escaped string presentation
    }

    if (spec_.width_kind == WidthPrec::None && spec_.prec_kind == WidthPrec::None) return detail::write(fc.out(), sv);

    sv = sv.substr(0, spec_.get_precision(fc));
    std::size_t estimated_width = sv.size();

    return detail::write_padded_as_spec(sv, estimated_width, fc, spec_);
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
    if constexpr (std::is_same_v<Tp, bool>) {
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
          throw_format_error("format error: format-spec contains invalid formatting options for 'char'");
      return end;
    } else
      return do_parse(pc, as_integer);
  }

  template <typename Int, typename Out>
  constexpr auto format(Int i, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    if (spec_.type == PresType::c) return format_character(to_character(i), fc);

    std::array<char, (sizeof(Int) * __CHAR_BIT__) + 3> buf;
    std::to_chars_result res{};

    std::string_view base_prefix;
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
        res = std::to_chars(start, end, u, 2);
        break;
      case PresType::None:
        // Should not reach here with _Pres_none for bool or charT, so:
        [[fallthrough]];
      case PresType::d:
        res = std::to_chars(start, end, u, 10);
        break;
      case PresType::o:
        if (i != 0) base_prefix = "0";
        res = std::to_chars(start, end, u, 8);
        break;
      case PresType::x:
      case PresType::X:
        base_prefix = spec_.type == PresType::x ? "0x" : "0X";
        res = std::to_chars(start, end, u, 16);
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

    return format_int(std::string_view(start, res.ptr - start), start_digits - start, fc);
  }

  template <typename Out>
  constexpr auto format(bool i, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    if (spec_.type == PresType::c) return format_character(static_cast<unsigned char>(i), fc);
    if (spec_.type != PresType::s) return format(static_cast<unsigned char>(i), fc);

    std::basic_string<CharT> s;
    std::size_t est_width;
    if (spec_.localized) [[unlikely]] {
      auto& np = std::use_facet<std::numpunct<CharT>>(fc.locale());
      s = i ? np.truename() : np.falsename();
      est_width = s.size();  // TODO Unicode-aware estimate
    } else {
      if constexpr (std::is_same_v<char, CharT>)
        s = i ? "true" : "false";
      else
        s = i ? L"true" : L"false";
      est_width = s.size();
    }

    return detail::write_padded_as_spec(s, est_width, fc, spec_);
  }

  template <typename Out>
  constexpr auto format_character(CharT c, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    return detail::write_padded_as_spec({&c, 1U}, 1, fc, spec_);
  }

  template <typename Int>
  static constexpr auto to_character(Int i) -> CharT {
    using limits = std::numeric_limits<CharT>;
    using traits = __gnu_cxx::__int_traits<CharT>;
    static_assert(limits::max() == traits::__max && limits::min() == traits::__min,
                  "std::numeric_limits<CharT> inconsistent with __int_traits<CharT>");

    if constexpr (std::is_signed_v<Int> == std::is_signed_v<CharT>) {
      if (traits::__min <= i && i <= traits::__max) return static_cast<CharT>(i);
    } else if constexpr (std::is_signed_v<Int>) {
      if (i >= 0 && std::make_unsigned_t<Int>(i) <= traits::__max) return static_cast<CharT>(i);
    } else if (i <= std::make_unsigned_t<CharT>(traits::__max))
      return static_cast<CharT>(i);
    throw_format_error("format error: integer not representable as character");
  }

  template <typename Out>
  constexpr auto format_int(std::string_view narrow_str, size_t prefix_len, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    size_t width = spec_.get_width(fc);

    std::basic_string_view<CharT> str;
    if constexpr (std::is_same_v<char, CharT>) str = narrow_str;

    if (spec_.localized) {
      const auto& l = fc.locale();
      if (l.name() != "C") {
        auto& np = use_facet<std::numpunct<CharT>>(l);
        std::string grp = np.grouping();
        if (!grp.empty()) {
          size_t n = str.size() - prefix_len;
          auto* p = std::bit_cast<CharT*>(__builtin_alloca((2 * n * sizeof(CharT)) + prefix_len));
          auto s = str.data();
          std::char_traits<CharT>::copy(p, s, prefix_len);
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

// We can format a floating-point type iff it is usable with to_chars.
template <typename Tp>
concept formattable_float = std::is_same_v<std::remove_cv_t<Tp>, Tp> &&
                            requires(Tp t, char* p) { std::to_chars(p, p, t, std::chars_format::scientific, 6); };

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
  constexpr auto format(Fp v, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    std::string dynbuf;
    std::array<char, 128> buf;
    std::to_chars_result res{};

    size_t prec = 6;
    bool use_prec = spec_.prec_kind != WidthPrec::None;
    if (use_prec) prec = spec_.get_precision(fc);

    char* start = buf.begin() + 1;  // reserve space for sign
    char* end = buf.end();

    std::chars_format fmt{};
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
        fmt = std::chars_format::hex;
        break;
      case PresType::E:
        upper = true;
        expc = 'E';
        [[fallthrough]];
      case PresType::e:
        use_prec = true;
        fmt = std::chars_format::scientific;
        break;
      case PresType::F:
        upper = true;
        [[fallthrough]];
      case PresType::f:
        use_prec = true;
        fmt = std::chars_format::fixed;
        break;
      case PresType::G:
        upper = true;
        expc = 'E';
        [[fallthrough]];
      case PresType::g:
        trailing_zeros = true;
        use_prec = true;
        fmt = std::chars_format::general;
        break;
      case PresType::None:
        if (use_prec) fmt = std::chars_format::general;
        break;
      default:
        std::unreachable();
    }

    // Write value into buffer using std::to_chars.
    auto to_chars = [&](char* b, char* e) -> auto {
      if (use_prec) return std::to_chars(b, e, v, fmt, prec);
      if (fmt != std::chars_format{}) return std::to_chars(b, e, v, fmt);
      return std::to_chars(b, e, v);
    };

    // First try using stack buffer.
    res = to_chars(start, end);

    if (__builtin_expect(res.ec == std::errc::value_too_large, 0)) {
      // If the buffer is too small it's probably because of a large
      // precision, or a very large value in fixed format.
      size_t guess = 8 + prec;
      if (fmt == std::chars_format::fixed)  // +ddd.prec
      {
        if constexpr (std::is_same_v<Fp, float> || std::is_same_v<Fp, double> || std::is_same_v<Fp, long double>) {
          // The number of digits to the left of the decimal point
          // is floor(log10(max(abs(__v),1)))+1
          int exp{};
          if constexpr (std::is_same_v<Fp, float>)
            __builtin_frexpf(v, &exp);
          else if constexpr (std::is_same_v<Fp, double>)
            __builtin_frexp(v, &exp);
          else if constexpr (std::is_same_v<Fp, long double>)
            __builtin_frexpl(v, &exp);
          if (exp > 0) guess += 1U + (exp * 4004U / 13301U);  // log10(2) approx.
        } else
          guess += std::numeric_limits<Fp>::max_exponent10;
      }
      if (guess <= sizeof(buf)) [[unlikely]]
        guess = sizeof(buf) * 2;
      dynbuf.reserve(guess);

      do {
        auto overwrite = [&to_chars, &res](char* p, size_t n) -> auto {
          res = to_chars(p + 1, p + n - 1);
          return res.ec == std::errc{} ? res.ptr - p : 0;
        };

        dynbuf.__resize_and_overwrite(dynbuf.capacity() * 2, overwrite);
        start = dynbuf.data() + 1;  // reserve space for sign
        end = dynbuf.data() + dynbuf.size();
      } while (__builtin_expect(res.ec == std::errc::value_too_large, 0));
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

    std::string_view narrow_str(start, res.ptr - start);

    // Use alternate form. Ensure decimal point is always present,
    // and add trailing zeros (up to precision) for g and G forms.
    if (spec_.alt && __builtin_isfinite(v)) {
      std::string_view s = narrow_str;
      size_t sigfigs;                   // Number of significant figures.
      size_t z = 0;                     // Number of trailing zeros to add.
      size_t p;                         // Position of the exponent character (if any).
      size_t d = s.find('.');           // Position of decimal point.
      if (d != std::string_view::npos)  // Found decimal point.
      {
        p = s.find(expc, d + 1);
        if (p == std::string_view::npos) p = s.size();

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
        if (p == std::string_view::npos) p = s.size();
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

    std::basic_string<CharT> wstr;
    std::basic_string_view<CharT> str;
    if constexpr (std::is_same_v<CharT, char>) str = narrow_str;

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
  [[nodiscard]] auto localize(std::basic_string_view<CharT> str, char expc, const std::locale& loc) const
      -> std::basic_string<CharT> {
    std::basic_string<CharT> lstr;

    if (loc == std::locale::classic()) return lstr;  // Nothing to do.

    const auto& np = std::use_facet<std::numpunct<CharT>>(loc);
    const CharT point = np.decimal_point();
    const std::string grp = np.grouping();

    CharT dot;
    CharT exp;
    if constexpr (std::is_same_v<CharT, char>) {
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
          std::unreachable();
      }
    }

    if (grp.empty() && point == dot) return lstr;  // Locale uses '.' and no grouping.

    size_t d = str.find(dot);
    size_t e = std::min(d, str.find(exp));
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
  constexpr auto format(CharT u, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    if (f_.spec().type == detail::PresType::None || f_.spec().type == detail::PresType::c)
      return f_.format_character(u, fc);
    if (f_.spec().type == detail::PresType::Esc) {
      return fc.out();
    }
    return f_.format(static_cast<std::make_unsigned_t<CharT>>(u), fc);
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
  constexpr auto format(CharT* u, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
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
  constexpr auto format(const CharT* u, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    return f_.format(u, fc);
  }

  constexpr void set_debug_format() noexcept { f_.set_debug_format(); }

 private:
  detail::FormatterStr<CharT> f_;
};

template <detail::Char CharT, size_t Nm>
struct Formatter<CharT[Nm], CharT> {  // NOLINT(modernize-avoid-c-arrays)
  Formatter() = default;

  [[__gnu__::__always_inline__]]
  constexpr auto parse(BasicFormatParseContext<CharT>& pc) -> typename BasicFormatParseContext<CharT>::iterator {
    return f_.parse(pc);
  }

  template <typename Out>
  // NOLINTNEXTLINE(modernize-avoid-c-arrays)
  constexpr auto format(const CharT (&u)[Nm], BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    return f_.format({u, Nm}, fc);
  }

  constexpr void set_debug_format() noexcept { f_.set_debug_format(); }

 private:
  detail::FormatterStr<CharT> f_;
};

template <typename Traits, typename Alloc>
struct Formatter<std::basic_string<char, Traits, Alloc>, char> {
  Formatter() = default;

  [[__gnu__::__always_inline__]]
  constexpr auto parse(BasicFormatParseContext<char>& pc) -> typename BasicFormatParseContext<char>::iterator {
    return f_.parse(pc);
  }

  template <typename Out>
  constexpr auto format(const std::basic_string<char, Traits, Alloc>& u, BasicFormatContext<Out, char>& fc) const ->
      typename BasicFormatContext<Out, char>::iterator {
    return f_.format(u, fc);
  }

  constexpr void set_debug_format() noexcept { f_.set_debug_format(); }

 private:
  detail::FormatterStr<char> f_;
};

template <typename Traits>
struct Formatter<std::basic_string_view<char, Traits>, char> {
  Formatter() = default;

  [[__gnu__::__always_inline__]]
  constexpr auto parse(BasicFormatParseContext<char>& pc) -> typename BasicFormatParseContext<char>::iterator {
    return f_.parse(pc);
  }

  template <typename Out>
  constexpr auto format(std::basic_string_view<char, Traits> u, BasicFormatContext<Out, char>& fc) const ->
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
constexpr bool is_formattable_integer = std::is_integral_v<Tp>;

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
  constexpr auto format(Tp u, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
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
  constexpr auto format(Tp u, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
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
  constexpr auto format(const void* v, BasicFormatContext<Out, CharT>& fc) const ->
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

    std::basic_string_view<CharT> str;
    if constexpr (std::is_same_v<CharT, char>) str = std::string_view(buf.data(), n);

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
  constexpr auto format(void* v, BasicFormatContext<Out, CharT>& fc) const ->
      typename BasicFormatContext<Out, CharT>::iterator {
    return f_.format(v, fc);
  }

 private:
  Formatter<const void*, CharT> f_;
};

template <detail::Char CharT>
struct Formatter<std::nullptr_t, CharT> {
  Formatter() = default;

  [[__gnu__::__always_inline__]]
  constexpr auto parse(BasicFormatParseContext<CharT>& pc) -> typename BasicFormatParseContext<CharT>::iterator {
    return f_.parse(pc);
  }

  template <typename Out>
  constexpr auto format(std::nullptr_t, BasicFormatContext<Out, CharT>& fc) const ->
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
          typename Formatter = typename Context::template formatter_type<std::remove_const_t<Tp>>,
          typename ParseContext = BasicFormatParseContext<typename Context::char_type>>
concept parsable_with = std::semiregular<Formatter> && requires(Formatter f, ParseContext pc) {
  { f.parse(pc) } -> std::same_as<typename ParseContext::iterator>;
};

template <typename Tp, typename Context,
          typename Formatter = typename Context::template formatter_type<std::remove_const_t<Tp>>,
          typename ParseContext = BasicFormatParseContext<typename Context::char_type>>
concept formattable_with = std::semiregular<Formatter> && requires(const Formatter cf, Tp&& t, Context fc) {
  { cf.format(t, fc) } -> std::same_as<typename Context::iterator>;
};

// An unspecified output iterator type used in the `formattable` concept.
template <typename CharT>
using iter_for = std::back_insert_iterator<std::basic_string<CharT>>;

template <typename Tp, typename CharT, typename Context = BasicFormatContext<iter_for<CharT>, CharT>>
concept formattable_impl = parsable_with<Tp, Context> && formattable_with<Tp, Context>;

}  // namespace detail
/// @endcond

// Concept std::formattable was introduced by P2286R8 "Formatting Ranges",
// but we can't guard it with __cpp_lib_format_ranges until we define that!
// [format.formattable], concept formattable
template <typename Tp, typename CharT>
concept formattable = detail::formattable_impl<std::remove_reference_t<Tp>, CharT>;

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
  std::iter_difference_t<Out> size;
};

/// @cond undocumented
namespace detail {

template <typename CharT>
class SinkIter {
  Sink<CharT>* sink_ = nullptr;

 public:
  using iterator_category = std::output_iterator_tag;
  using value_type = void;
  using difference_type = ptrdiff_t;
  using pointer = void;
  using reference = void;

  SinkIter() = default;
  SinkIter(const SinkIter&) = default;
  auto operator=(const SinkIter&) -> SinkIter& = default;

  explicit constexpr SinkIter(Sink<CharT>& sink) : sink_(std::addressof(sink)) {}

  constexpr auto operator=(CharT c) -> SinkIter& {
    sink_->write(c);
    return *this;
  }

  constexpr auto operator=(std::basic_string_view<CharT> s) -> SinkIter& {
    sink_->write(s);
    return *this;
  }

  constexpr auto operator*() -> SinkIter& { return *this; }

  constexpr auto operator++() -> SinkIter& { return *this; }

  constexpr auto operator++(int) -> SinkIter { return *this; }

  [[nodiscard]] constexpr auto reserve(size_t n) const { return sink_->reserve(n); }
};

// Abstract base class for type-erased character sinks.
// All formatting and output is done via this type's iterator,
// to reduce the number of different template instantiations.
template <typename CharT>
class Sink {
  friend class SinkIter<CharT>;

  std::span<CharT> span_;
  std::span<CharT>::iterator next_;

  // Called when the span is full, to make more space available.
  // Precondition: _M_next != _M_span.begin()
  // Postcondition: _M_next != _M_span.end()
  // TODO: remove the precondition? could make overflow handle it.
  constexpr virtual void overflow() = 0;

 protected:
  // Precondition: __span.size() != 0
  constexpr explicit Sink(std::span<CharT> span) noexcept : span_{span}, next_{span.begin()} {}

  // The portion of the span that has been written to.
  [[nodiscard]] constexpr auto used() const noexcept -> std::span<CharT> {
    return span_.first(std::distance(span_.begin(), next_));
  }

  // The portion of the span that has not been written to.
  [[nodiscard]] constexpr auto unused() const noexcept -> std::span<CharT> {
    return span_.subspan(std::distance(span_.begin(), next_));
  }

  // Use the start of the span as the next write position.
  constexpr void rewind() noexcept { next_ = span_.begin(); }

  // Replace the current output range.
  constexpr void reset(std::span<CharT> s, size_t pos = 0) noexcept {
    span_ = s;
    next_ = s.begin() + pos;
  }

  // Called by the iterator for *it++ = c
  constexpr void write(CharT c) {
    *next_++ = c;
    if (next_ - span_.begin() == std::ssize(span_)) [[unlikely]]
      overflow();
  }

  constexpr void write(std::basic_string_view<CharT> s) {
    std::span to = unused();
    while (to.size() <= s.size()) {
      s.copy(to.data(), to.size());
      next_ += to.size();
      s.remove_prefix(to.size());
      overflow();
      to = unused();
    }
    if (!s.empty()) {
      s.copy(to.data(), s.size());
      next_ += s.size();
    }
  }

  // A successful _Reservation can be used to directly write
  // up to N characters to the sink to avoid unwanted buffering.
  struct Reservation {
    // True if the reservation was successful, false otherwise.
    constexpr explicit operator bool() const noexcept { return sink; }
    // A pointer to write directly to the sink.
    [[nodiscard]] constexpr auto get() const noexcept -> CharT* { return sink->next_.operator->(); }
    // Add n to the _M_next iterator for the sink.
    constexpr void bump(size_t n) { sink->bump(n); }

    Sink* sink;
  };

  // Attempt to reserve space to write n characters to the sink.
  // If anything is written to the reservation then there must be a call
  // to _M_bump(N2) before any call to another member function of *this,
  // where N2 is the number of characters written.
  constexpr virtual auto reserve(size_t n) -> Reservation {
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
  constexpr virtual void bump(size_t n) { next_ += n; }

 public:
  Sink(const Sink&) = delete;
  auto operator=(const Sink&) -> Sink& = delete;

  constexpr auto out() noexcept -> SinkIter<CharT> { return SinkIter<CharT>(*this); }
};

// A sink with an internal buffer. This is used to implement concrete sinks.
template <typename CharT>
class BufSink : public Sink<CharT> {
 protected:
  static constexpr std::size_t buf_size = 32 * sizeof(void*) / sizeof(CharT);
  std::array<CharT, buf_size> buf;

  constexpr BufSink() noexcept : Sink<CharT>(buf) {}
};

using std::vector;

// A sink that fills a sequence (e.g. std::string, std::vector, std::deque).
// Writes to a buffer then appends that to the sequence when it fills up.
template <typename Seq>
class SeqSink final : public BufSink<typename Seq::value_type> {
  using char_t = typename Seq::value_type;

  Seq seq_;

  // Transfer buffer contents to the sequence, so buffer can be refilled.
  constexpr void overflow() override {
    auto s = this->used();
    if (s.empty()) [[unlikely]]
      return;  // Nothing in the buffer to transfer to _M_seq.

    // If reserve was called then bump must have been called too.
    assert(s.data() != seq_.data());

    if constexpr (is_specialization_of<Seq, std::basic_string>)
      seq_.append(s.data(), s.size());
    else
      seq_.insert(seq_.end(), s.begin(), s.end());

    // Make the whole of buf_ available for the next write:
    this->rewind();
  }

  constexpr auto reserve(size_t n) -> typename Sink<char_t>::Reservation override {
    // We might already have n characters available in this->_M_unused(),
    // but the whole point of this function is to be an optimization for
    // the std::format("{}", x) case. We want to avoid writing to buf
    // and then copying that into a basic_string if possible, so this
    // function prefers to create space directly in seq_ rather than
    // using buf.

    if constexpr (is_specialization_of<Seq, std::basic_string> || is_specialization_of<Seq, std::vector>) {
      // Flush the buffer to seq_ first (should not be needed).
      if (this->used().size()) [[unlikely]]
        SeqSink::overflow();

      // Expand seq_ to make n new characters available:
      const auto sz = seq_.size();
      if constexpr (std::is_same_v<std::string, Seq> || std::is_same_v<std::wstring, Seq>)
        seq_.resize_and_overwrite(sz + n, [](auto, auto n2) -> auto { return n2; });
      else
        seq_.resize(sz + n);

      // Set used() to be a span over the original part of seq_
      // and unused() to be the extra capacity we just created:
      this->reset(seq_, sz);
      return {this};
    } else  // Try to use the base class' buffer.
      return Sink<char_t>::reserve(n);
  }

  constexpr void bump(size_t n) override {
    if constexpr (is_specialization_of<Seq, std::basic_string> || is_specialization_of<Seq, std::vector>) {
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
  constexpr SeqSink() noexcept(std::is_nothrow_default_constructible_v<Seq>) = default;

  constexpr explicit SeqSink(Seq&& s) noexcept(std::is_nothrow_move_constructible_v<Seq>) : seq_(std::move(s)) {}

  using Sink<char_t>::out;

  constexpr auto get() && -> Seq {
    if (this->used().size() != 0) SeqSink::overflow();
    return std::move(seq_);
  }

  // A writable span that views everything written to the sink.
  // Will be either a view over _M_seq or the used part of _M_buf.
  constexpr auto view() -> std::span<char_t> {
    auto s = this->used();
    if (seq_.size()) {
      if (s.size() != 0) SeqSink::overflow();
      return seq_;
    }
    return s;
  }
};

template <typename CharT, typename Alloc = std::allocator<CharT>>
using StrSink = SeqSink<std::basic_string<CharT, std::char_traits<CharT>, Alloc>>;

// template<typename _CharT, typename _Alloc = allocator<_CharT>>
// using _Vec_sink = _Seq_sink<vector<_CharT, _Alloc>>;

// A sink that writes to an output iterator.
// Writes to a fixed-size buffer and then flushes to the output iterator
// when the buffer fills up.
template <typename CharT, typename OutIter>
class IterSink : public BufSink<CharT> {
  OutIter out_;
  std::iter_difference_t<OutIter> max_;

  size_t count_ = 0;

 protected:
  [[nodiscard]] constexpr auto count() const noexcept -> size_t { return count_; }

  void overflow() override {
    auto s = this->used();
    if (max_ < 0)  // No maximum.
      out_ = std::ranges::copy(s, std::move(out_)).out;
    else if (count_ < static_cast<size_t>(max_)) {
      auto max = max_ - count_;
      std::span<CharT> first;
      if (max < s.size())
        first = s.first(static_cast<size_t>(max));
      else
        first = s;
      out_ = std::ranges::copy(first, std::move(out_)).out;
    }
    this->rewind();
    count_ += s.size();
  }

 public:
  [[__gnu__::__always_inline__]]
  constexpr explicit IterSink(OutIter out, std::iter_difference_t<OutIter> max = -1)
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
template <typename CharT, std::contiguous_iterator OutIter>
  requires std::same_as<std::iter_value_t<OutIter>, CharT>
class IterSink<CharT, OutIter> : public Sink<CharT> {
  OutIter first_;
  std::iter_difference_t<OutIter> max_ = -1;

  size_t count_ = 0;
  std::array<CharT, 64> buf_;  // Write here after outputting _M_max characters.

 protected:
  [[nodiscard]] constexpr auto count() const noexcept -> size_t { return count_; }

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
      if constexpr (!std::is_integral_v<std::iter_difference_t<OutIter>> || sizeof(n) > sizeof(size_t)) {
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

template <std::unsigned_integral T>
[[nodiscard]] constexpr auto operator|(ArgT lhs, T rhs) -> T {
  return std::to_underlying(lhs) | rhs;
}

template <std::unsigned_integral T>
[[nodiscard]] constexpr auto operator|(T lhs, ArgT rhs) -> T {
  return lhs | std::to_underlying(rhs);
}

template <typename Context>
struct ArgValue {
  using char_t = typename Context::char_type;

  struct HandleBase {
    const void* ptr;
    void (*func)();
  };

  union {
    std::monostate none;
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
    std::basic_string_view<char_t> sv;
    const void* ptr;
    HandleBase handle;
  };

  [[__gnu__::__always_inline__]]
  constexpr ArgValue()
      : none() {}

  template <typename Tp, typename Self>
  [[__gnu__::__always_inline__]]
  static constexpr auto get(Self& u) noexcept -> auto& {
    if constexpr (std::is_same_v<Tp, bool>)
      return u.boolean;
    else if constexpr (std::is_same_v<Tp, char_t>)
      return u.c;
    else if constexpr (std::is_same_v<Tp, int>)
      return u.i;
    else if constexpr (std::is_same_v<Tp, unsigned>)
      return u.u;
    else if constexpr (std::is_same_v<Tp, int64_t>)
      return u.ll;
    else if constexpr (std::is_same_v<Tp, uint64_t>)
      return u.ull;
    else if constexpr (std::is_same_v<Tp, float>)
      return u.flt;
    else if constexpr (std::is_same_v<Tp, double>)
      return u.dbl;
    else if constexpr (std::is_same_v<Tp, long double>)
      return u.ldbl;
    else if constexpr (std::is_same_v<Tp, const char_t*>)
      return u.str;
    else if constexpr (std::is_same_v<Tp, std::basic_string_view<char_t>>)
      return u.sv;
    else if constexpr (std::is_same_v<Tp, const void*>)
      return u.ptr;
    else if constexpr (std::derived_from<Tp, HandleBase>)
      return static_cast<Tp&>(u.handle);
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
  constexpr void set(Tp v) noexcept {
    if constexpr (std::derived_from<Tp, HandleBase>)
      std::construct_at(&handle, v);
    else {
      if constexpr (std::is_same_v<Tp, bool>)
        boolean = v;
      else if constexpr (std::is_same_v<Tp, char_t>)
        c = v;
      else if constexpr (std::is_same_v<Tp, int>)
        i = v;
      else if constexpr (std::is_same_v<Tp, unsigned>)
        u = v;
      else if constexpr (std::is_same_v<Tp, int64_t>)
        ll = v;
      else if constexpr (std::is_same_v<Tp, uint64_t>)
        ull = v;
      else if constexpr (std::is_same_v<Tp, float>)
        flt = v;
      else if constexpr (std::is_same_v<Tp, double>)
        dbl = v;
      else if constexpr (std::is_same_v<Tp, long double>)
        ldbl = v;
      else if constexpr (std::is_same_v<Tp, const char_t*>)
        str = v;
      else if constexpr (std::is_same_v<Tp, std::basic_string_view<char_t>>)
        sv = v;
      else if constexpr (std::is_same_v<Tp, const void*>)
        ptr = v;
      else if constexpr (std::derived_from<Tp, HandleBase>)
        static_cast<Tp&>(handle) = v;
      // Otherwise, ill-formed.
    }
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
    using maybe_const_t = std::conditional_t<formattable<const Tp>, const Tp, Tp>;

    template <typename Tq>
    static void format(BasicFormatParseContext<CharT>& parse_ctx, Context& format_ctx, const void* ptr) {
      using Td = std::remove_const_t<Tq>;
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
  constexpr BasicFormatArg() noexcept
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

  static_assert(std::is_trivially_copyable_v<detail::ArgValue<Context>>);

  detail::ArgValue<Context> val_;
  detail::ArgT type_;

  // Transform incoming argument type to the type stored in _Arg_value.
  // e.g. short -> int, std::string -> std::string_view,
  // char[3] -> const char*.
  template <typename Tp>
  static consteval auto to_arg_type() {
    using Td = std::remove_const_t<Tp>;
    if constexpr (std::is_same_v<Td, bool>) {
      return std::type_identity<bool>();
    } else if constexpr (std::is_same_v<Td, CharT>) {
      return std::type_identity<CharT>();
    } else if constexpr (std::is_same_v<Td, char> && std::is_same_v<CharT, wchar_t>) {
      return std::type_identity<CharT>();
    } else if constexpr (std::is_integral_v<Td> && std::is_signed_v<Td>) {
      if constexpr (sizeof(Td) <= sizeof(int)) {
        return std::type_identity<int>();
      } else if constexpr (sizeof(Td) <= sizeof(int64_t)) {
        return std::type_identity<int64_t>();
      }
    } else if constexpr (std::is_integral_v<Td> && std::is_unsigned_v<Td>) {
      if constexpr (sizeof(Td) <= sizeof(unsigned)) {
        return std::type_identity<unsigned>();
      } else if constexpr (sizeof(Td) <= sizeof(uint64_t)) {
        return std::type_identity<uint64_t>();
      }
    } else if constexpr (std::is_same_v<Td, float>) {
      return std::type_identity<float>();
    } else if constexpr (std::is_same_v<Td, double>) {
      return std::type_identity<double>();
    } else if constexpr (std::is_same_v<Td, long double>) {
      return std::type_identity<long double>();

#if defined(__FLT16_DIG__) && defined(_GLIBCXX_FLOAT_IS_IEEE_BINARY32)
    } else if constexpr (std::is_same_v<Td, _Float16>) {
      return std::type_identity<float>();
#endif

#ifdef __FLT32_DIG__
    } else if constexpr (std::is_same_v<Td, _Float32>) {
#ifdef _GLIBCXX_FLOAT_IS_IEEE_BINARY32
      return std::type_identity<float>();
#else
      return std::type_identity<_Float32>();
#endif
#endif
#ifdef __FLT64_DIG__
    } else if constexpr (std::is_same_v<Td, _Float64>) {
#ifdef _GLIBCXX_DOUBLE_IS_IEEE_BINARY64
      return std::type_identity<double>();
#else
      return std::type_identity<_Float64>();
#endif
#endif
    } else if constexpr (is_specialization_of<Td, std::basic_string_view> ||
                         is_specialization_of<Td, std::basic_string>) {
      if constexpr (std::is_same_v<typename Td::value_type, CharT>) {
        return std::type_identity<std::basic_string_view<CharT>>();
      } else {
        return std::type_identity<Handle>();
      }
    } else if constexpr (std::is_same_v<std::decay_t<Td>, const CharT*> || std::is_same_v<std::decay_t<Td>, CharT*>) {
      return std::type_identity<const CharT*>();
    } else if constexpr (std::is_void_v<std::remove_pointer_t<Td>> || std::is_same_v<Td, std::nullptr_t>) {
      return std::type_identity<const void*>();
    } else {
      return std::type_identity<Handle>();
    }
  }

  // Transform a formattable type to the appropriate storage type.
  template <typename Tp>
  using Normalize = typename decltype(to_arg_type<Tp>())::type;

  // Get the _Arg_t value corresponding to a normalized type.
  template <typename Tp>
  static consteval auto to_enum() -> detail::ArgT {
    using detail::ArgT;
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
    else if constexpr (std::is_same_v<Tp, std::basic_string_view<CharT>>)
      return ArgT::Sv;
    else if constexpr (std::is_same_v<Tp, const void*>)
      return ArgT::Ptr;
    else if constexpr (std::is_same_v<Tp, Handle>)
      return ArgT::Handle;
  }

  template <typename Tp>
  constexpr void set(Tp v) noexcept {
    type_ = to_enum<Tp>();
    val_.set(v);
  }

  template <typename Tp>
    requires detail::formattable_with<Tp, Context>
  constexpr explicit BasicFormatArg(Tp& v) noexcept {
    using Td = Normalize<Tp>;
    if constexpr (std::is_same_v<Td, std::basic_string_view<CharT>>)
      set(Td{v.data(), v.size()});
    else if constexpr (std::is_same_v<std::remove_const_t<Tp>, char> && std::is_same_v<CharT, wchar_t>)
      set(static_cast<Td>(static_cast<unsigned char>(v)));
    else
      set(static_cast<Td>(v));
  }

  template <typename Ctx, typename... Argz>
  friend constexpr auto make_format_args(Argz&...) noexcept;

  template <typename Visitor, typename Ctx>
  friend constexpr auto visit_format_arg(Visitor&& vis, BasicFormatArg<Ctx>) -> decltype(auto);

  template <typename Visitor>
  constexpr auto visit(Visitor&& vis, detail::ArgT type) -> decltype(auto) {
    using detail::ArgT;
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
      default:
        // _Arg_f16 etc.
        std::unreachable();
    }
  }
};

template <typename Visitor, typename Context>
constexpr auto visit_format_arg(Visitor&& vis, BasicFormatArg<Context> arg) -> decltype(auto) {
  return arg.visit(std::forward<Visitor>(vis), arg.type_);
}

/// @cond undocumented
namespace detail {

struct WidthPrecVisitor {
  template <typename Tp>
  auto operator()(Tp& arg) const -> size_t {
    if constexpr (std::is_same_v<Tp, std::monostate>) detail::invalid_arg_id_in_format_string();
    // _GLIBCXX_RESOLVE_LIB_DEFECTS
    // 3720. Restrict the valid types of arg-id for width and precision
    // 3721. Allow an arg-id with a value of zero for width
    else if constexpr (sizeof(Tp) <= sizeof(int64_t)) {
      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 3720. Restrict the valid types of arg-id for width and precision
      if constexpr (std::is_integral_v<Tp> && std::is_unsigned_v<Tp>)
        return arg;
      else if constexpr (std::is_integral_v<Tp> && std::is_signed_v<Tp>)
        if (arg >= 0) return arg;
    }
    throw_format_error("format error: argument used for width or precision must be a non-negative integer");
  }
};

template <typename Context>
constexpr auto int_from_arg(const BasicFormatArg<Context>& arg) -> size_t {
  return visit_format_arg(WidthPrecVisitor(), arg);
}

// Pack ArgT enum values into a single 60-bit integer.
template <int Bits, size_t Nm>
constexpr auto pack_arg_types(const std::array<ArgT, Nm>& types) {
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

  [[nodiscard]] constexpr auto size() const noexcept -> size_t {
    return (packed_size_ != 0U) ? packed_size_ : unpacked_size_;
  }

  [[nodiscard]] constexpr auto type(size_t i) const noexcept -> typename detail::ArgT {
    uint64_t t = unpacked_size_ >> (i * packed_type_bits);
    return static_cast<detail::ArgT>(t & packed_type_mask);
  }

  template <typename Ctx, typename... Args>
  friend constexpr auto make_format_args(Args&...) noexcept;

  // An array of _Arg_t enums corresponding to _Args...
  template <typename... Args>
  static consteval auto types_to_pack() -> std::array<detail::ArgT, sizeof...(Args)> {
    return {FormatArg::template to_enum<Args>()...};
  }

 public:
  template <typename... Args>
  constexpr BasicFormatArgs(const Store<Args...>& store) noexcept;  // NOLINT(google-explicit-constructor)

  [[nodiscard, __gnu__::__always_inline__]]
  constexpr auto get(size_t i) const noexcept -> BasicFormatArg<Context> {
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
constexpr auto make_format_args(Args&... fmt_args) noexcept;

// An array of type-erased formatting arguments.
template <typename Context, typename... Args>
class detail::ArgStore {
  friend nostd::BasicFormatArgs<Context>;

  template <typename Ctx, typename... Argz>
  friend constexpr auto nostd::make_format_args(Argz&...) noexcept;

  // For a sufficiently small number of arguments we only store values.
  // basic_format_args can get the types from the _Args pack.
  static constexpr bool values_only = sizeof...(Args) <= BasicFormatArgs<Context>::max_packed_args;

  using Element = std::conditional_t<values_only, detail::ArgValue<Context>, BasicFormatArg<Context>>;

  std::array<Element, sizeof...(Args)> args_;

  template <typename Tp>
  static constexpr auto make_element(Tp& v) -> Element {
    using Tq = std::remove_const_t<Tp>;
    using CharT = typename Context::char_type;
    static_assert(std::is_default_constructible_v<Formatter<Tq, CharT>>,
                  "std::formatter must be specialized for the type of each format arg");
    using detail::formattable_with;
    if constexpr (std::is_const_v<Tp>)
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
  [[__gnu__::__always_inline__]] constexpr explicit ArgStore(Tp&... a) noexcept : args_{make_element(a)...} {}
};

template <typename Context>
class detail::ArgStore<Context> {};

template <typename Context>
template <typename... Args>
constexpr BasicFormatArgs<Context>::BasicFormatArgs(const Store<Args...>& store) noexcept {
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
    values = store.args_.data();
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
constexpr auto make_format_args(Args&... fmt_args) noexcept {
  using fmt_arg = BasicFormatArg<Context>;
  using store = detail::ArgStore<Context, typename fmt_arg::template Normalize<Args>...>;
  return store{fmt_args...};
}

/// @cond undocumented
namespace detail {

template <typename Out, typename CharT, typename Context>
constexpr auto do_vformat_to(Out /*out*/, std::basic_string_view<CharT> /*fmt*/,
                             const BasicFormatArgs<Context>& /*args*/, const std::locale* /*loc*/ = nullptr) -> Out;

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
  static_assert(std::output_iterator<Out, const CharT&>);

  BasicFormatArgs<BasicFormatContext> args_;
  Out out_;
  detail::OptionalLocale loc_;

  constexpr BasicFormatContext(BasicFormatArgs<BasicFormatContext> args, Out out) : args_(args), out_(std::move(out)) {}

  constexpr BasicFormatContext(BasicFormatArgs<BasicFormatContext> args, Out out, const std::locale& loc)
      : args_(args), out_(std::move(out)), loc_(loc) {}

  template <typename Out2, typename CharT2, typename Context2>
  friend constexpr auto detail::do_vformat_to(Out2, std::basic_string_view<CharT2>, const BasicFormatArgs<Context2>&,
                                              const std::locale*) -> Out2;

 public:
  // _GLIBCXX_RESOLVE_LIB_DEFECTS
  // 4061. Should std::basic_format_context be
  //       default-constructible/copyable/movable?
  BasicFormatContext(const BasicFormatContext&) = delete;
  auto operator=(const BasicFormatContext&) -> BasicFormatContext& = delete;

  constexpr ~BasicFormatContext() = default;

  using iterator = Out;
  using char_type = CharT;
  template <typename Tp>
  using formatter_type = Formatter<Tp, CharT>;

  [[nodiscard]]
  constexpr auto arg(size_t id) const noexcept -> BasicFormatArg<BasicFormatContext> {
    return args_.get(id);
  }

  [[nodiscard]]
  constexpr auto locale() -> std::locale {
    return loc_.value();
  }

  [[nodiscard]]
  constexpr auto out() -> iterator {
    return std::move(out_);
  }

  constexpr void advance_to(iterator it) { out_ = std::move(it); }
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

  constexpr explicit Scanner(std::basic_string_view<CharT> str, size_t nargs = static_cast<size_t>(-1))
      : pc(str, nargs) {}

  [[nodiscard]] constexpr auto begin() const noexcept -> iterator { return pc.begin(); }
  [[nodiscard]] constexpr auto end() const noexcept -> iterator { return pc.end(); }

  constexpr void scan() {
    auto fmt = fmt_str();

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

  [[nodiscard]] constexpr auto fmt_str() const noexcept -> std::basic_string_view<CharT> { return {begin(), end()}; }

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
  constexpr DetailtingScanner(BasicFormatContext<Out, CharT>& fc, std::basic_string_view<CharT> str)
      : Scanner<CharT>(str), fc_(fc) {}

 private:
  BasicFormatContext<Out, CharT>& fc_;

  using iterator = typename Scanner<CharT>::iterator;

  constexpr void on_chars(iterator last) override {
    std::basic_string_view<CharT> str(this->begin(), last);
    fc_.advance_to(detail::write(fc_.out(), str));
  }

  constexpr void format_arg(size_t id) override {
    using Context = BasicFormatContext<Out, CharT>;
    using Handle = typename BasicFormatArg<Context>::Handle;

    visit_format_arg(
        [this](auto& arg) -> auto {
          using Type = std::remove_reference_t<decltype(arg)>;
          using Formatter = typename Context::template formatter_type<Type>;
          if constexpr (std::is_same_v<Type, std::monostate>)
            detail::invalid_arg_id_in_format_string();
          else if constexpr (std::is_same_v<Type, Handle>)
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
  static_assert((std::is_default_constructible_v<Formatter<Args, CharT>> && ...),
                "std::formatter must be specialized for each type being formatted");

 public:
  constexpr explicit CheckingScanner(std::basic_string_view<CharT> str) : Scanner<CharT>(str, sizeof...(Args)) {}

 private:
  constexpr void format_arg(size_t id) override {
    if constexpr (sizeof...(Args) != 0) {
      if (id < sizeof...(Args)) {
        parse_format_spec<Args...>(id);
        return;
      }
    }
    std::unreachable();
  }

  template <typename Tp, typename... OtherArgs>
  constexpr void parse_format_spec(size_t id) {
    if (id == 0) {
      Formatter<Tp, CharT> f;
      this->pc.advance_to(f.parse(this->pc));
    } else if constexpr (sizeof...(OtherArgs) != 0)
      parse_format_spec<OtherArgs...>(id - 1);
    else
      std::unreachable();
  }
};

// Compute the length of the character sequence needed to represent an unsigned integer value in base 10
template <std::unsigned_integral Tp>
constexpr auto to_chars_len(Tp value) noexcept -> std::size_t {
  constexpr std::size_t base = 10;
  constexpr std::size_t b2 = base * base;
  constexpr std::size_t b3 = b2 * base;
  constexpr std::size_t b4 = b3 * base;
  for (std::size_t n = 1;; n += 4) {
    if (value < base) return n;
    if (value < b2) return n + 1;
    if (value < b3) return n + 2;
    if (value < b4) return n + 3;
    value /= b4;
  }
}

template <typename Out, typename CharT, typename Context>
constexpr auto do_vformat_to(Out out, std::basic_string_view<CharT> fmt, const BasicFormatArgs<Context>& args,
                             const std::locale* loc) -> Out {
  IterSink<CharT, Out> sink(std::move(out));
  SinkIter<CharT> sink_out;

  if constexpr (std::is_same_v<Out, SinkIter<CharT>>)
    sink_out = out;  // Already a sink iterator, safe to use post-move.
  else
    sink_out = sink.out();

  if constexpr (std::is_same_v<CharT, char>)
    // Fast path for "{}" format strings and simple format arg types.
    if (fmt.size() == 2 && fmt[0] == '{' && fmt[1] == '}') {
      bool done = false;
      visit_format_arg(
          [&](auto& arg) -> auto {
            using tp = std::remove_cvref_t<decltype(arg)>;
            if constexpr (std::is_same_v<tp, bool>) {
              size_t len = 4 + !arg;
              std::array<const char*, 2> chars = {"false", "true"};
              if (auto res = sink_out.reserve(len)) {
                std::memcpy(res.get(), chars[arg], len);
                res.bump(len);
                done = true;
              }
            } else if constexpr (std::is_same_v<tp, char>) {
              if (auto res = sink_out.reserve(1)) {
                *res.get() = arg;
                res.bump(1);
                done = true;
              }
            } else if constexpr (std::is_integral_v<tp>) {
              std::make_unsigned_t<tp> uval;
              const bool neg = arg < 0;
              if (neg)
                uval = std::make_unsigned_t<tp>(~arg) + 1U;
              else
                uval = arg;
              const auto n = to_chars_len(uval);
              if (auto res = sink_out.reserve(n + neg)) {
                auto ptr = res.get();
                *ptr = '-';
                auto start = ptr + static_cast<std::size_t>(neg);
                std::to_chars(start, start + n, uval);
                res.bump(n + neg);
                done = true;
              }
            } else if constexpr (std::is_convertible_v<tp, std::string_view>) {
              std::string_view sv = arg;
              if (auto res = sink_out.reserve(sv.size())) {
                std::memcpy(res.get(), sv.data(), sv.size());
                res.bump(sv.size());
                done = true;
              }
            }
          },
          args.get(0));

      if (done) {
        if constexpr (std::is_same_v<Out, SinkIter<CharT>>)
          return sink_out;
        else
          return std::move(sink)._M_finish().out;
      }
    }

  auto ctx = loc == nullptr ? Context(args, sink_out) : Context(args, sink_out, *loc);
  DetailtingScanner<SinkIter<CharT>, CharT> scanner(ctx, fmt);
  scanner.scan();

  if constexpr (std::is_same_v<Out, SinkIter<CharT>>)
    return ctx.out();
  else
    return std::move(sink)._M_finish().out;
}

}  // namespace detail
/// @endcond

template <typename CharT, typename... Args>
template <typename Tp>
  requires std::convertible_to<const Tp&, std::basic_string_view<CharT>>
consteval BasicFormatString<CharT, Args...>::BasicFormatString(const Tp& s) : str_(s) {
  detail::CheckingScanner<CharT, std::remove_cvref_t<Args>...> scanner(str_);
  scanner.scan();
}

// [format.functions], formatting functions

template <typename Out>
  requires std::output_iterator<Out, const char&>
[[__gnu__::__always_inline__]]
constexpr auto vformat_to(Out out, std::string_view fmt, format_args args) -> Out {
  return detail::do_vformat_to(std::move(out), fmt, args);
}

template <typename Out>
  requires std::output_iterator<Out, const char&>
[[__gnu__::__always_inline__]]
constexpr auto vformat_to(Out out, const std::locale& loc, std::string_view fmt, format_args args) -> Out {
  return detail::do_vformat_to(std::move(out), fmt, args, &loc);
}

[[nodiscard]]
constexpr auto vformat(std::string_view fmt, format_args args) -> std::string {
  detail::StrSink<char> buf;
  vformat_to(buf.out(), fmt, args);
  return std::move(buf).get();
}

[[nodiscard]]
constexpr auto vformat(const std::locale& loc, std::string_view fmt, format_args args) -> std::string {
  detail::StrSink<char> buf;
  vformat_to(buf.out(), loc, fmt, args);
  return std::move(buf).get();
}

template <typename... Args>
[[nodiscard]]
constexpr auto format(format_string<Args...> fmt, Args&&... args) -> std::string {
  return vformat(fmt.get(), make_format_args(args...));
}

template <typename... Args>
[[nodiscard]]
constexpr auto format(const std::locale& loc, format_string<Args...> fmt, Args&&... args) -> std::string {
  return vformat(loc, fmt.get(), make_format_args(args...));
}

template <typename Out, typename... Args>
  requires std::output_iterator<Out, const char&>
constexpr auto format_to(Out out, format_string<Args...> fmt, Args&&... args) -> Out {
  return vformat_to(std::move(out), fmt.get(), make_format_args(args...));
}

template <typename Out, typename... Args>
  requires std::output_iterator<Out, const char&>
constexpr auto format_to(Out out, const std::locale& loc, format_string<Args...> fmt, Args&&... args) -> Out {
  return vformat_to(std::move(out), loc, fmt.get(), make_format_args(args...));
}

template <typename Out, typename... Args>
  requires std::output_iterator<Out, const char&>
constexpr auto format_to_n(Out out, std::iter_difference_t<Out> n, format_string<Args...> fmt, Args&&... args)
    -> FormatToNResult<Out> {
  detail::IterSink<char, Out> sink(std::move(out), n);
  vformat_to(sink.out(), fmt.get(), make_format_args(args...));
  return std::move(sink)._M_finish();
}

template <typename Out, typename... Args>
  requires std::output_iterator<Out, const char&>
constexpr auto format_to_n(Out out, std::iter_difference_t<Out> n, const std::locale& loc, format_string<Args...> fmt,
                           Args&&... args) -> FormatToNResult<Out> {
  detail::IterSink<char, Out> sink(std::move(out), n);
  vformat_to(sink.out(), loc, fmt.get(), make_format_args(args...));
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
constexpr auto formatted_size(format_string<Args...> fmt, Args&&... args) -> size_t {
  detail::CountingSink<char> buf;
  vformat_to(buf.out(), fmt.get(), make_format_args(args...));
  return buf.count();
}

template <typename... Args>
[[nodiscard]]
constexpr auto formatted_size(const std::locale& loc, format_string<Args...> fmt, Args&&... args) -> size_t {
  detail::CountingSink<char> buf;
  vformat_to(buf.out(), loc, fmt.get(), make_format_args(args...));
  return buf.count();
}

// [format.range], formatting of ranges
// [format.range.fmtkind], variable template format_kind
// enum class RangeFormat : uint8_t { Disabled, Map, Set, Sequence, String, DebugString };
//
// /// @cond undocumented
// template <typename Rg>
// constexpr auto format_kind = not defined(format_kind<Rg>);
//
// template <typename Tp>
// consteval auto fmt_kind() -> RangeFormat {
//   using ref = std::ranges::range_reference_t<Tp>;
//   if constexpr (std::is_same_v<std::remove_cvref_t<ref>, Tp>) {
//     return RangeFormat::Disabled;
//   } else if constexpr (requires { typename Tp::key_type; }) {
//     if constexpr (requires { typename Tp::mapped_type; }) {
//       using up = std::remove_cvref_t<ref>;
//       if constexpr (is_specialization_of<up, std::pair>) {
//         return RangeFormat::Map;
//       } else if constexpr (is_specialization_of<up, std::tuple>) {
//         if constexpr (std::tuple_size_v<up> == 2) return RangeFormat::Map;
//       }
//     }
//     return RangeFormat::Set;
//   } else {
//     return RangeFormat::Sequence;
//   }
// }
// /// @endcond
//
// /// A constant determining how a range should be formatted.
// template <std::ranges::input_range Rg>
//   requires std::same_as<Rg, std::remove_cvref_t<Rg>>
// constexpr RangeFormat format_kind<Rg> = fmt_kind<Rg>();
//
// // [format.range.formatter], class template range_formatter
// template <typename Tp, typename CharT = char>
//   requires std::same_as<std::remove_cvref_t<Tp>, Tp> && formattable<Tp, CharT>
// class RangeFormatter;  // TODO
//
// /// @cond undocumented
// namespace detail {
// // [format.range.fmtdef], class template range-default-formatter
// template <RangeFormat Kind, std::ranges::input_range Rg, typename CharT>
// struct RangeDefaultFormatter;  // TODO
// }  // namespace detail
// /// @endcond
//
// // [format.range.fmtmap], [format.range.fmtset], [format.range.fmtstr],
// // specializations for maps, sets, and strings
// template <std::ranges::input_range Rg, typename CharT>
//   requires(format_kind<Rg> != RangeFormat::Disabled) && formattable<std::ranges::range_reference_t<Rg>, CharT>
// struct Formatter<Rg, CharT> : detail::RangeDefaultFormatter<format_kind<Rg>, Rg, CharT> {};

}  // namespace nostd
