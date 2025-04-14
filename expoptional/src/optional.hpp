///
// optional - An implementation of std::optional with extensions
// Written in 2017 by Sy Brand (tartanllama@gmail.com, @TartanLlama)
//
// Documentation available at https://tl.tartanllama.xyz/
//
// To the extent possible under law, the author(s) have dedicated all
// copyright and related and neighboring rights to this software to the
// public domain worldwide. This software is distributed without any warranty.
//
// You should have received a copy of the CC0 Public Domain Dedication
// along with this software. If not, see
// <http://creativecommons.org/publicdomain/zero/1.0/>.
///

#ifndef TL_OPTIONAL_HPP
#define TL_OPTIONAL_HPP

#include <exception>
#include <functional>
#include <type_traits>
#include <utility>
#include <variant>

namespace tl {

template <class T>
class optional;

namespace detail {

// Trait for checking if a type is a tl::optional
template <class T>
struct is_optional_impl : std::false_type {};
template <class T>
struct is_optional_impl<optional<T>> : std::true_type {};
template <class T>
using is_optional = is_optional_impl<std::decay_t<T>>;
template <class T>
inline constexpr bool is_optional_v = is_optional<T>::value;

// Change void to tl::monostate
template <class U>
using fixup_void = std::conditional_t<std::is_void_v<U>, std::monostate, U>;

template <class F, class U, class = std::invoke_result_t<F, U>>
using get_map_return = optional<fixup_void<std::invoke_result_t<F, U>>>;

// Check if invoking F for some Us returns void
template <class F, class = void, class... U>
struct returns_void_impl;
template <class F, class... U>
struct returns_void_impl<F, std::void_t<std::invoke_result_t<F, U...>>, U...>
    : std::is_void<std::invoke_result_t<F, U...>> {};
template <class F, class... U>
using returns_void = returns_void_impl<F, void, U...>;

template <class T, class... U>
using enable_if_ret_void = std::enable_if_t<returns_void<T&&, U...>::value>;

template <class T, class... U>
using disable_if_ret_void = std::enable_if_t<!returns_void<T&&, U...>::value>;

template <class T, class U>
using enable_forward_value =
    std::enable_if_t<std::is_constructible_v<T, U&&> && !std::is_same_v<std::decay_t<U>, std::in_place_t> &&
                     !std::is_same_v<optional<T>, std::decay_t<U>>>;

template <class T, class U, class Other>
using enable_from_other =
    std::enable_if_t<std::is_constructible_v<T, Other> && !std::is_constructible_v<T, optional<U>&> &&
                     !std::is_constructible_v<T, optional<U>&&> && !std::is_constructible_v<T, const optional<U>&> &&
                     !std::is_constructible_v<T, const optional<U>&&> && !std::is_convertible_v<optional<U>&, T> &&
                     !std::is_convertible_v<optional<U>&&, T> && !std::is_convertible_v<const optional<U>&, T> &&
                     !std::is_convertible_v<const optional<U>&&, T>>;

template <class T, class U>
using enable_assign_forward =
    std::enable_if_t<!std::is_same_v<optional<T>, std::decay_t<U>> &&
                     !std::conjunction_v<std::is_scalar<T>, std::is_same<T, std::decay_t<U>>> &&
                     std::is_constructible_v<T, U> && std::is_assignable_v<T&, U>>;

template <class T, class U, class Other>
using enable_assign_from_other =
    std::enable_if_t<std::is_constructible_v<T, Other> && std::is_assignable_v<T&, Other> &&
                     !std::is_constructible_v<T, optional<U>&> && !std::is_constructible_v<T, optional<U>&&> &&
                     !std::is_constructible_v<T, const optional<U>&> &&
                     !std::is_constructible_v<T, const optional<U>&&> && !std::is_convertible_v<optional<U>&, T> &&
                     !std::is_convertible_v<optional<U>&&, T> && !std::is_convertible_v<const optional<U>&, T> &&
                     !std::is_convertible_v<const optional<U>&&, T> && !std::is_assignable_v<T&, optional<U>&> &&
                     !std::is_assignable_v<T&, optional<U>&&> && !std::is_assignable_v<T&, const optional<U>&> &&
                     !std::is_assignable_v<T&, const optional<U>&&>>;

// The storage base manages the actual storage, and correctly propagates
// trivial destruction from T. This case is for when T is not trivially
// destructible.
template <class T, bool = ::std::is_trivially_destructible_v<T>>
struct optional_storage_base {
  constexpr optional_storage_base() noexcept : m_dummy(), m_has_value(false) {}

  template <class... U>
  constexpr optional_storage_base(std::in_place_t /*unused*/, U&&... u)  // NOLINT(google-explicit-constructor)
      : m_value(std::forward<U>(u)...), m_has_value(true) {}

  ~optional_storage_base() {
    if (m_has_value) {
      m_value.~T();
      m_has_value = false;
    }
  }

  struct dummy {};
  union {
    dummy m_dummy;
    T m_value;
  };

  bool m_has_value;
};

// This case is for when T is trivially destructible.
template <class T>
struct optional_storage_base<T, true> {
  constexpr optional_storage_base() noexcept : m_dummy() {}

  template <class... U>
  constexpr optional_storage_base(std::in_place_t /*unused*/, U&&... u)  // NOLINT(google-explicit-constructor)
      : m_value(std::forward<U>(u)...), m_has_value(true) {}

  // No destructor, so this class is trivially destructible

  struct dummy {};
  union {
    dummy m_dummy;
    T m_value;
  };

  bool m_has_value = false;
};

// This base class provides some handy member functions which can be used in
// further derived classes
template <class T>
struct optional_operations_base : optional_storage_base<T> {
  using optional_storage_base<T>::optional_storage_base;

  constexpr void hard_reset() noexcept {
    get().~T();
    this->m_has_value = false;
  }

  template <class... Args>
  constexpr void construct(Args&&... args) {
    new (std::addressof(this->m_value)) T(std::forward<Args>(args)...);
    this->m_has_value = true;
  }

  template <class Opt>
  constexpr void assign(Opt&& rhs) {
    if (this->has_value()) {
      if (rhs.has_value()) {
        this->m_value = std::forward<Opt>(rhs).get();
      } else {
        this->m_value.~T();
        this->m_has_value = false;
      }
    } else if (rhs.has_value()) {
      construct(std::forward<Opt>(rhs).get());
    }
  }

  [[nodiscard]] constexpr auto has_value() const -> bool { return this->m_has_value; }

  [[nodiscard]] constexpr auto get() & -> T& { return this->m_value; }
  [[nodiscard]] constexpr auto get() const& -> const T& { return this->m_value; }
  [[nodiscard]] constexpr auto get() && -> T&& { return std::move(this->m_value); }
  [[nodiscard]] constexpr auto get() const&& -> const T&& { return std::move(this->m_value); }
};

// This class manages conditionally having a trivial copy constructor
// This specialization is for when T is trivially copy constructible
template <class T, bool = std ::is_trivially_copy_constructible_v<T>>
struct optional_copy_base : optional_operations_base<T> {
  using optional_operations_base<T>::optional_operations_base;
};

// This specialization is for when T is not trivially copy constructible
template <class T>
struct optional_copy_base<T, false> : optional_operations_base<T> {
  using optional_operations_base<T>::optional_operations_base;

  optional_copy_base() = default;
  optional_copy_base(const optional_copy_base& rhs) : optional_operations_base<T>() {
    if (rhs.has_value()) {
      this->construct(rhs.get());
    } else {
      this->m_has_value = false;
    }
  }

  optional_copy_base(optional_copy_base&& rhs) = default;
  auto operator=(const optional_copy_base& rhs) -> optional_copy_base& = default;
  auto operator=(optional_copy_base&& rhs) -> optional_copy_base& = default;
};

// This class manages conditionally having a trivial move constructor
// Unfortunately there's no way to achieve this in GCC < 5 AFAIK, since it
// doesn't implement an analogue to std::is_trivially_move_constructible. We
// have to make do with a non-trivial move constructor even if T is trivially
// move constructible
template <class T, bool = std::is_trivially_move_constructible_v<T>>
struct optional_move_base : optional_copy_base<T> {
  using optional_copy_base<T>::optional_copy_base;
};
template <class T>
struct optional_move_base<T, false> : optional_copy_base<T> {
  using optional_copy_base<T>::optional_copy_base;

  optional_move_base() = default;
  optional_move_base(const optional_move_base& rhs) = default;

  optional_move_base(optional_move_base&& rhs) noexcept(std::is_nothrow_move_constructible_v<T>) {
    if (rhs.has_value()) {
      this->construct(std::move(rhs.get()));
    } else {
      this->m_has_value = false;
    }
  }
  auto operator=(const optional_move_base& rhs) -> optional_move_base& = default;
  auto operator=(optional_move_base&& rhs) -> optional_move_base& = default;
};

// This class manages conditionally having a trivial copy assignment operator
template <class T, bool = std ::is_trivially_copy_assignable_v<T> && std ::is_trivially_copy_constructible_v<T> &&
                          std ::is_trivially_destructible_v<T>>
struct optional_copy_assign_base : optional_move_base<T> {
  using optional_move_base<T>::optional_move_base;
};

template <class T>
struct optional_copy_assign_base<T, false> : optional_move_base<T> {
  using optional_move_base<T>::optional_move_base;

  optional_copy_assign_base() = default;
  optional_copy_assign_base(const optional_copy_assign_base& rhs) = default;

  optional_copy_assign_base(optional_copy_assign_base&& rhs) = default;
  auto operator=(const optional_copy_assign_base& rhs) -> optional_copy_assign_base& {
    this->assign(rhs);
    return *this;
  }
  auto operator=(optional_copy_assign_base&& rhs) -> optional_copy_assign_base& = default;
};

// This class manages conditionally having a trivial move assignment operator
// Unfortunately there's no way to achieve this in GCC < 5 AFAIK, since it
// doesn't implement an analogue to std::is_trivially_move_assignable. We have
// to make do with a non-trivial move assignment operator even if T is trivially
// move assignable
template <class T, bool = std::is_trivially_destructible_v<T> && std::is_trivially_move_constructible_v<T> &&
                          std::is_trivially_move_assignable_v<T>>
struct optional_move_assign_base : optional_copy_assign_base<T> {
  using optional_copy_assign_base<T>::optional_copy_assign_base;
};

template <class T>
struct optional_move_assign_base<T, false> : optional_copy_assign_base<T> {
  using optional_copy_assign_base<T>::optional_copy_assign_base;

  optional_move_assign_base() = default;
  optional_move_assign_base(const optional_move_assign_base& rhs) = default;

  optional_move_assign_base(optional_move_assign_base&& rhs) = default;

  auto operator=(const optional_move_assign_base& rhs) -> optional_move_assign_base& = default;

  auto operator=(optional_move_assign_base&& rhs) noexcept(std::is_nothrow_move_constructible_v<T> &&
                                                           std::is_nothrow_move_assignable_v<T>)
      -> optional_move_assign_base& {
    this->assign(std::move(rhs));
    return *this;
  }
};

// optional_delete_ctor_base will conditionally delete copy and move
// constructors depending on whether T is copy/move constructible
template <class T, bool EnableCopy = std::is_copy_constructible_v<T>, bool EnableMove = std::is_move_constructible_v<T>>
struct optional_delete_ctor_base {
  optional_delete_ctor_base() = default;
  optional_delete_ctor_base(const optional_delete_ctor_base&) = default;
  optional_delete_ctor_base(optional_delete_ctor_base&&) noexcept = default;
  auto operator=(const optional_delete_ctor_base&) -> optional_delete_ctor_base& = default;
  auto operator=(optional_delete_ctor_base&&) noexcept -> optional_delete_ctor_base& = default;
};

template <class T>
struct optional_delete_ctor_base<T, true, false> {
  optional_delete_ctor_base() = default;
  optional_delete_ctor_base(const optional_delete_ctor_base&) = default;
  optional_delete_ctor_base(optional_delete_ctor_base&&) noexcept = delete;
  auto operator=(const optional_delete_ctor_base&) -> optional_delete_ctor_base& = default;
  auto operator=(optional_delete_ctor_base&&) noexcept -> optional_delete_ctor_base& = default;
};

template <class T>
struct optional_delete_ctor_base<T, false, true> {
  optional_delete_ctor_base() = default;
  optional_delete_ctor_base(const optional_delete_ctor_base&) = delete;
  optional_delete_ctor_base(optional_delete_ctor_base&&) noexcept = default;
  auto operator=(const optional_delete_ctor_base&) -> optional_delete_ctor_base& = default;
  auto operator=(optional_delete_ctor_base&&) noexcept -> optional_delete_ctor_base& = default;
};

template <class T>
struct optional_delete_ctor_base<T, false, false> {
  optional_delete_ctor_base() = default;
  optional_delete_ctor_base(const optional_delete_ctor_base&) = delete;
  optional_delete_ctor_base(optional_delete_ctor_base&&) noexcept = delete;
  auto operator=(const optional_delete_ctor_base&) -> optional_delete_ctor_base& = default;
  auto operator=(optional_delete_ctor_base&&) noexcept -> optional_delete_ctor_base& = default;
};

// optional_delete_assign_base will conditionally delete copy and move
// constructors depending on whether T is copy/move constructible + assignable
template <class T, bool EnableCopy = (std::is_copy_constructible_v<T> && std::is_copy_assignable_v<T>),
          bool EnableMove = (std::is_move_constructible_v<T> && std::is_move_assignable_v<T>)>
struct optional_delete_assign_base {
  optional_delete_assign_base() = default;
  optional_delete_assign_base(const optional_delete_assign_base&) = default;
  optional_delete_assign_base(optional_delete_assign_base&&) noexcept = default;
  auto operator=(const optional_delete_assign_base&) -> optional_delete_assign_base& = default;
  auto operator=(optional_delete_assign_base&&) noexcept -> optional_delete_assign_base& = default;
};

template <class T>
struct optional_delete_assign_base<T, true, false> {
  optional_delete_assign_base() = default;
  optional_delete_assign_base(const optional_delete_assign_base&) = default;
  optional_delete_assign_base(optional_delete_assign_base&&) noexcept = default;
  auto operator=(const optional_delete_assign_base&) -> optional_delete_assign_base& = default;
  auto operator=(optional_delete_assign_base&&) noexcept -> optional_delete_assign_base& = delete;
};

template <class T>
struct optional_delete_assign_base<T, false, true> {
  optional_delete_assign_base() = default;
  optional_delete_assign_base(const optional_delete_assign_base&) = default;
  optional_delete_assign_base(optional_delete_assign_base&&) noexcept = default;
  auto operator=(const optional_delete_assign_base&) -> optional_delete_assign_base& = delete;
  auto operator=(optional_delete_assign_base&&) noexcept -> optional_delete_assign_base& = default;
};

template <class T>
struct optional_delete_assign_base<T, false, false> {
  optional_delete_assign_base() = default;
  optional_delete_assign_base(const optional_delete_assign_base&) = default;
  optional_delete_assign_base(optional_delete_assign_base&&) noexcept = default;
  auto operator=(const optional_delete_assign_base&) -> optional_delete_assign_base& = delete;
  auto operator=(optional_delete_assign_base&&) noexcept -> optional_delete_assign_base& = delete;
};

}  // namespace detail

/// A tag type to represent an empty optional
struct nullopt_t {
  struct do_not_use {};
  constexpr explicit nullopt_t(do_not_use /*unused*/, do_not_use /*unused*/) noexcept {}
};
/// Represents an empty optional
static constexpr nullopt_t nullopt{nullopt_t::do_not_use{}, nullopt_t::do_not_use{}};

class bad_optional_access : public std::exception {
 public:
  bad_optional_access() = default;
  [[nodiscard]] auto what() const noexcept -> const char* override { return "Optional has no value"; }
};

/// An optional object is an object that contains the storage for another
/// object and manages the lifetime of this contained object, if any. The
/// contained object may be initialized after the optional object has been
/// initialized, and may be destroyed before the optional object has been
/// destroyed. The initialization state of the contained object is tracked by
/// the optional object.
template <class T>
class optional : private detail::optional_move_assign_base<T>,
                 private detail::optional_delete_ctor_base<T>,
                 private detail::optional_delete_assign_base<T> {
  using base = detail::optional_move_assign_base<T>;

  static_assert(!std::is_same_v<T, std::in_place_t>, "instantiation of optional with in_place_t is ill-formed");
  static_assert(!std::is_same_v<std::decay_t<T>, nullopt_t>, "instantiation of optional with nullopt_t is ill-formed");

 public:
  /// Carries out some operation which returns an optional on the stored
  /// object if there is one.
  template <class F>
  constexpr auto and_then(F&& f) & {
    using result = std::invoke_result_t<F, T&>;
    static_assert(detail::is_optional<result>::value, "F must return an optional");

    return has_value() ? std::invoke(std::forward<F>(f), **this) : result(nullopt);
  }

  template <class F>
  constexpr auto and_then(F&& f) && {
    using result = std::invoke_result_t<F, T&&>;
    static_assert(detail::is_optional<result>::value, "F must return an optional");

    return has_value() ? std::invoke(std::forward<F>(f), std::move(**this)) : result(nullopt);
  }

  template <class F>
  constexpr auto and_then(F&& f) const& {
    using result = std::invoke_result_t<F, const T&>;
    static_assert(detail::is_optional<result>::value, "F must return an optional");

    return has_value() ? std::invoke(std::forward<F>(f), **this) : result(nullopt);
  }

  template <class F>
  constexpr auto and_then(F&& f) const&& {
    using result = std::invoke_result_t<F, const T&&>;
    static_assert(detail::is_optional<result>::value, "F must return an optional");

    return has_value() ? std::invoke(std::forward<F>(f), std::move(**this)) : result(nullopt);
  }

  /// Carries out some operation on the stored object if there is one.
  template <class F>
  constexpr auto map(F&& f) & {
    return optional_map_impl(*this, std::forward<F>(f));
  }

  template <class F>
  constexpr auto map(F&& f) && {
    return optional_map_impl(std::move(*this), std::forward<F>(f));
  }

  template <class F>
  constexpr auto map(F&& f) const& {
    return optional_map_impl(*this, std::forward<F>(f));
  }

  template <class F>
  constexpr auto map(F&& f) const&& {
    return optional_map_impl(std::move(*this), std::forward<F>(f));
  }

  /// Carries out some operation on the stored object if there is one.
  template <class F>
  constexpr auto transform(F&& f) & {
    return optional_map_impl(*this, std::forward<F>(f));
  }

  template <class F>
  constexpr auto transform(F&& f) && {
    return optional_map_impl(std::move(*this), std::forward<F>(f));
  }

  template <class F>
  constexpr auto transform(F&& f) const& {
    return optional_map_impl(*this, std::forward<F>(f));
  }

  template <class F>
  constexpr auto transform(F&& f) const&& {
    return optional_map_impl(std::move(*this), std::forward<F>(f));
  }

  /// Calls `f` if the optional is empty
  template <class F, detail::enable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) & -> optional<T> {
    if (has_value()) return *this;

    std::forward<F>(f)();
    return nullopt;
  }

  template <class F, detail::disable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) & -> optional<T> {
    return has_value() ? *this : std::forward<F>(f)();
  }

  template <class F, detail::enable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) && -> optional<T> {
    if (has_value()) return std::move(*this);

    std::forward<F>(f)();
    return nullopt;
  }

  template <class F, detail::disable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) && -> optional<T> {
    return has_value() ? std::move(*this) : std::forward<F>(f)();
  }

  template <class F, detail::enable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) const& -> optional<T> {
    if (has_value()) return *this;

    std::forward<F>(f)();
    return nullopt;
  }

  template <class F, detail::disable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) const& -> optional<T> {
    return has_value() ? *this : std::forward<F>(f)();
  }

  template <class F, detail::enable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) const&& -> optional<T> {
    if (has_value()) return std::move(*this);

    std::forward<F>(f)();
    return nullopt;
  }

  template <class F, detail::disable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) const&& -> optional<T> {
    return has_value() ? std::move(*this) : std::forward<F>(f)();
  }

  /// Maps the stored value with `f` if there is one, otherwise returns `u`.
  template <class F, class U>
  constexpr auto map_or(F&& f, U&& u) & -> U {
    return has_value() ? std::invoke(std::forward<F>(f), **this) : std::forward<U>(u);
  }

  template <class F, class U>
  constexpr auto map_or(F&& f, U&& u) && -> U {
    return has_value() ? std::invoke(std::forward<F>(f), std::move(**this)) : std::forward<U>(u);
  }

  template <class F, class U>
  constexpr auto map_or(F&& f, U&& u) const& -> U {
    return has_value() ? std::invoke(std::forward<F>(f), **this) : std::forward<U>(u);
  }

  template <class F, class U>
  constexpr auto map_or(F&& f, U&& u) const&& -> U {
    return has_value() ? std::invoke(std::forward<F>(f), std::move(**this)) : std::forward<U>(u);
  }

  /// Maps the stored value with `f` if there is one, otherwise calls
  /// `u` and returns the result.
  template <class F, class U>
  constexpr auto map_or_else(F&& f, U&& u) & -> std::invoke_result_t<U> {
    return has_value() ? std::invoke(std::forward<F>(f), **this) : std::forward<U>(u)();
  }

  template <class F, class U>
  constexpr auto map_or_else(F&& f, U&& u) && -> std::invoke_result_t<U> {
    return has_value() ? std::invoke(std::forward<F>(f), std::move(**this)) : std::forward<U>(u)();
  }

  template <class F, class U>
  constexpr auto map_or_else(F&& f, U&& u) const& -> std::invoke_result_t<U> {
    return has_value() ? std::invoke(std::forward<F>(f), **this) : std::forward<U>(u)();
  }

  template <class F, class U>
  constexpr auto map_or_else(F&& f, U&& u) const&& -> std::invoke_result_t<U> {
    return has_value() ? std::invoke(std::forward<F>(f), std::move(**this)) : std::forward<U>(u)();
  }

  /// Returns `u` if `*this` has a value, otherwise an empty optional.
  template <class U>
  constexpr auto conjunction(U&& u) const -> optional<std::decay_t<U>> {
    using result = optional<std::decay_t<U>>;
    return has_value() ? result{u} : result{nullopt};
  }

  /// Returns `rhs` if `*this` is empty, otherwise the current value.
  [[nodiscard]] constexpr auto disjunction(const optional& rhs) & -> optional { return has_value() ? *this : rhs; }

  [[nodiscard]] constexpr auto disjunction(const optional& rhs) const& -> optional { return has_value() ? *this : rhs; }

  [[nodiscard]] constexpr auto disjunction(const optional& rhs) && -> optional {
    return has_value() ? std::move(*this) : rhs;
  }

  [[nodiscard]] constexpr auto disjunction(const optional& rhs) const&& -> optional {
    return has_value() ? std::move(*this) : rhs;
  }

  [[nodiscard]] constexpr auto disjunction(optional&& rhs) & -> optional {
    return has_value() ? *this : std::move(rhs);
  }

  [[nodiscard]] constexpr auto disjunction(optional&& rhs) const& -> optional {
    return has_value() ? *this : std::move(rhs);
  }

  [[nodiscard]] constexpr auto disjunction(optional&& rhs) && -> optional {
    return has_value() ? std::move(*this) : std::move(rhs);
  }

  [[nodiscard]] constexpr auto disjunction(optional&& rhs) const&& -> optional {
    return has_value() ? std::move(*this) : std::move(rhs);
  }

  /// Takes the value out of the optional, leaving it empty
  [[nodiscard]] constexpr auto take() -> optional {
    optional ret = std::move(*this);
    reset();
    return ret;
  }

  using value_type = T;

  /// Constructs an optional that does not contain a value.
  constexpr optional() noexcept = default;

  constexpr optional(nullopt_t /*unused*/) noexcept {}  // NOLINT(google-explicit-constructor)

  /// Copy constructor
  ///
  /// If `rhs` contains a value, the stored value is direct-initialized with
  /// it. Otherwise, the constructed optional is empty.
  constexpr optional(const optional& rhs) = default;

  /// Move constructor
  ///
  /// If `rhs` contains a value, the stored value is direct-initialized with
  /// it. Otherwise, the constructed optional is empty.
  constexpr optional(optional&& rhs) = default;

  /// Constructs the stored value in-place using the given arguments.
  template <class... Args>
    requires std::is_constructible_v<T, Args...>
  constexpr explicit optional(std::in_place_t /*unused*/, Args&&... args)
      : base(std::in_place, std::forward<Args>(args)...) {}

  template <class U, class... Args>
    requires std::is_constructible_v<T, std::initializer_list<U>&, Args&&...>
  constexpr explicit optional(std::in_place_t /*unused*/, std::initializer_list<U> il, Args&&... args) {
    this->construct(il, std::forward<Args>(args)...);
  }

  /// Constructs the stored value with `u`.
  template <class U = T, detail::enable_forward_value<T, U>* = nullptr>
    requires std::is_convertible_v<U&&, T>
  constexpr optional(U&& u) : base(std::in_place, std::forward<U>(u)) {}  // NOLINT(google-explicit-constructor)

  template <class U = T, detail::enable_forward_value<T, U>* = nullptr>
    requires(!std::is_convertible_v<U &&, T>)
  constexpr explicit optional(U&& u) : base(std::in_place, std::forward<U>(u)) {}

  /// Converting copy constructor.
  template <class U, detail::enable_from_other<T, U, const U&>* = nullptr>
    requires std::is_convertible_v<const U&, T>
  optional(const optional<U>& rhs)  // NOLINT(google-explicit-constructor)
  {
    if (rhs.has_value()) {
      this->construct(*rhs);
    }
  }

  template <class U, detail::enable_from_other<T, U, const U&>* = nullptr>
    requires(!std::is_convertible_v<const U&, T>)
  explicit optional(const optional<U>& rhs) {
    if (rhs.has_value()) {
      this->construct(*rhs);
    }
  }

  /// Converting move constructor.
  template <class U, detail::enable_from_other<T, U, U&&>* = nullptr>
    requires std::is_convertible_v<U&&, T>
  optional(optional<U>&& rhs) {  // NOLINT(google-explicit-constructor)
    if (rhs.has_value()) {
      this->construct(std::move(*rhs));
    }
  }

  template <class U, detail::enable_from_other<T, U, U&&>* = nullptr>
  explicit optional(optional<U>&& rhs)
    requires(!std::is_convertible_v<U &&, T>)
  {
    if (rhs.has_value()) {
      this->construct(std::move(*rhs));
    }
  }

  /// Destroys the stored value if there is one.
  ~optional() = default;

  /// Assignment to empty.
  ///
  /// Destroys the current value if there is one.
  auto operator=(nullopt_t /*unused*/) noexcept -> optional& {
    if (has_value()) {
      this->m_value.~T();
      this->m_has_value = false;
    }

    return *this;
  }

  /// Copy assignment.
  ///
  /// Copies the value from `rhs` if there is one. Otherwise resets the stored
  /// value in `*this`.
  auto operator=(const optional& rhs) -> optional& = default;

  /// Move assignment.
  ///
  /// Moves the value from `rhs` if there is one. Otherwise resets the stored
  /// value in `*this`.
  auto operator=(optional&& rhs) -> optional& = default;

  /// Assigns the stored value from `u`, destroying the old value if there was
  /// one.
  template <class U = T, detail::enable_assign_forward<T, U>* = nullptr>
  auto operator=(U&& u) -> optional& {
    if (has_value()) {
      this->m_value = std::forward<U>(u);
    } else {
      this->construct(std::forward<U>(u));
    }

    return *this;
  }

  /// Converting copy assignment operator.
  ///
  /// Copies the value from `rhs` if there is one. Otherwise resets the stored
  /// value in `*this`.
  template <class U, detail::enable_assign_from_other<T, U, const U&>* = nullptr>
  auto operator=(const optional<U>& rhs) -> optional& {
    if (has_value()) {
      if (rhs.has_value()) {
        this->m_value = *rhs;
      } else {
        this->hard_reset();
      }
    }

    else if (rhs.has_value()) {
      this->construct(*rhs);
    }

    return *this;
  }

  // TODO check exception guarantee
  /// Converting move assignment operator.
  ///
  /// Moves the value from `rhs` if there is one. Otherwise resets the stored
  /// value in `*this`.
  template <class U, detail::enable_assign_from_other<T, U, U>* = nullptr>
  auto operator=(optional<U>&& rhs) -> optional& {
    if (has_value()) {
      if (rhs.has_value()) {
        this->m_value = std::move(*rhs);
      } else {
        this->hard_reset();
      }
    }

    else if (rhs.has_value()) {
      this->construct(std::move(*rhs));
    }

    return *this;
  }

  /// Constructs the value in-place, destroying the current one if there is
  /// one.
  template <class... Args>
  auto emplace(Args&&... args) -> T& {
    static_assert(std::is_constructible_v<T, Args&&...>, "T must be constructible with Args");

    *this = nullopt;
    this->construct(std::forward<Args>(args)...);
    return value();
  }

  template <class U, class... Args>
  auto emplace(std::initializer_list<U> il, Args&&... args) -> T&
    requires std::is_constructible_v<T, std::initializer_list<U>&, Args&&...>
  {
    *this = nullopt;
    this->construct(il, std::forward<Args>(args)...);
    return value();
  }

  /// Swaps this optional with the other.
  ///
  /// If neither optionals have a value, nothing happens.
  /// If both have a value, the values are swapped.
  /// If one has a value, it is moved to the other and the movee is left
  /// valueless.
  void swap(optional& rhs) noexcept(std::is_nothrow_move_constructible_v<T> && std::is_nothrow_swappable_v<T>) {
    using std::swap;
    if (has_value()) {
      if (rhs.has_value()) {
        swap(**this, *rhs);
      } else {
        new (std::addressof(rhs.m_value)) T(std::move(this->m_value));
        this->m_value.T::~T();
      }
    } else if (rhs.has_value()) {
      new (std::addressof(this->m_value)) T(std::move(rhs.m_value));
      rhs.m_value.T::~T();
    }
    swap(this->m_has_value, rhs.m_has_value);
  }

  /// Returns a pointer to the stored value
  [[nodiscard]] constexpr auto operator->() const -> const T* { return std::addressof(this->m_value); }

  [[nodiscard]] constexpr auto operator->() -> T* { return std::addressof(this->m_value); }

  /// Returns the stored value
  [[nodiscard]] constexpr auto operator*() & -> T& { return this->m_value; }

  [[nodiscard]] constexpr auto operator*() const& -> const T& { return this->m_value; }

  [[nodiscard]] constexpr auto operator*() && -> T&& { return std::move(this->m_value); }

  [[nodiscard]] constexpr auto operator*() const&& -> const T&& { return std::move(this->m_value); }

  /// Returns whether or not the optional has a value
  [[nodiscard]] constexpr auto has_value() const noexcept -> bool { return this->m_has_value; }

  constexpr explicit operator bool() const noexcept { return this->m_has_value; }

  /// Returns the contained value if there is one, otherwise throws
  /// bad_optional_access
  [[nodiscard]] constexpr auto value() & -> T& {
    if (has_value()) return this->m_value;
    throw bad_optional_access();
  }
  [[nodiscard]] constexpr auto value() const& -> const T& {
    if (has_value()) return this->m_value;
    throw bad_optional_access();
  }
  [[nodiscard]] constexpr auto value() && -> T&& {
    if (has_value()) return std::move(this->m_value);
    throw bad_optional_access();
  }

  [[nodiscard]] constexpr auto value() const&& -> const T&& {
    if (has_value()) return std::move(this->m_value);
    throw bad_optional_access();
  }

  /// Returns the stored value if there is one, otherwise returns `u`
  template <class U>
  [[nodiscard]] constexpr auto value_or(U&& u) const& -> T {
    static_assert(std::is_copy_constructible_v<T> && std::is_convertible_v<U&&, T>,
                  "T must be copy constructible and convertible from U");
    return has_value() ? **this : static_cast<T>(std::forward<U>(u));
  }

  template <class U>
  [[nodiscard]] constexpr auto value_or(U&& u) && -> T {
    static_assert(std::is_move_constructible_v<T> && std::is_convertible_v<U&&, T>,
                  "T must be move constructible and convertible from U");
    return has_value() ? std::move(**this) : static_cast<T>(std::forward<U>(u));
  }

  /// Destroys the stored value if one exists, making the optional empty
  void reset() noexcept {
    if (has_value()) {
      this->m_value.~T();
      this->m_has_value = false;
    }
  }
};  // namespace tl

/// Compares two optional objects
template <class T, class U>
constexpr auto operator==(const optional<T>& lhs, const optional<U>& rhs) -> bool {
  return lhs.has_value() == rhs.has_value() && (!lhs.has_value() || *lhs == *rhs);
}
template <class T, class U>
constexpr auto operator!=(const optional<T>& lhs, const optional<U>& rhs) -> bool {
  return lhs.has_value() != rhs.has_value() || (lhs.has_value() && *lhs != *rhs);
}
template <class T, class U>
constexpr auto operator<(const optional<T>& lhs, const optional<U>& rhs) -> bool {
  return rhs.has_value() && (!lhs.has_value() || *lhs < *rhs);
}
template <class T, class U>
constexpr auto operator>(const optional<T>& lhs, const optional<U>& rhs) -> bool {
  return lhs.has_value() && (!rhs.has_value() || *lhs > *rhs);
}
template <class T, class U>
constexpr auto operator<=(const optional<T>& lhs, const optional<U>& rhs) -> bool {
  return !lhs.has_value() || (rhs.has_value() && *lhs <= *rhs);
}
template <class T, class U>
constexpr auto operator>=(const optional<T>& lhs, const optional<U>& rhs) -> bool {
  return !rhs.has_value() || (lhs.has_value() && *lhs >= *rhs);
}

/// Compares an optional to a `nullopt`
template <class T>
constexpr auto operator==(const optional<T>& lhs, nullopt_t /*unused*/) noexcept -> bool {
  return !lhs.has_value();
}
template <class T>
constexpr auto operator==(nullopt_t /*unused*/, const optional<T>& rhs) noexcept -> bool {
  return !rhs.has_value();
}
template <class T>
constexpr auto operator!=(const optional<T>& lhs, nullopt_t /*unused*/) noexcept -> bool {
  return lhs.has_value();
}
template <class T>
constexpr auto operator!=(nullopt_t /*unused*/, const optional<T>& rhs) noexcept -> bool {
  return rhs.has_value();
}
template <class T>
constexpr auto operator<(const optional<T>& /*unused*/, nullopt_t /*unused*/) noexcept -> bool {
  return false;
}
template <class T>
constexpr auto operator<(nullopt_t /*unused*/, const optional<T>& rhs) noexcept -> bool {
  return rhs.has_value();
}
template <class T>
constexpr auto operator<=(const optional<T>& lhs, nullopt_t /*unused*/) noexcept -> bool {
  return !lhs.has_value();
}
template <class T>
constexpr auto operator<=(nullopt_t /*unused*/, const optional<T>& /*unused*/) noexcept -> bool {
  return true;
}
template <class T>
constexpr auto operator>(const optional<T>& lhs, nullopt_t /*unused*/) noexcept -> bool {
  return lhs.has_value();
}
template <class T>
constexpr auto operator>(nullopt_t /*unused*/, const optional<T>& /*unused*/) noexcept -> bool {
  return false;
}
template <class T>
constexpr auto operator>=(const optional<T>& /*unused*/, nullopt_t /*unused*/) noexcept -> bool {
  return true;
}
template <class T>
constexpr auto operator>=(nullopt_t /*unused*/, const optional<T>& rhs) noexcept -> bool {
  return !rhs.has_value();
}

/// Compares the optional with a value.
template <class T, class U>
constexpr auto operator==(const optional<T>& lhs, const U& rhs) -> bool {
  return lhs.has_value() ? *lhs == rhs : false;
}
template <class T, class U>
constexpr auto operator==(const U& lhs, const optional<T>& rhs) -> bool {
  return rhs.has_value() ? lhs == *rhs : false;
}
template <class T, class U>
constexpr auto operator!=(const optional<T>& lhs, const U& rhs) -> bool {
  return lhs.has_value() ? *lhs != rhs : true;
}
template <class T, class U>
constexpr auto operator!=(const U& lhs, const optional<T>& rhs) -> bool {
  return rhs.has_value() ? lhs != *rhs : true;
}
template <class T, class U>
constexpr auto operator<(const optional<T>& lhs, const U& rhs) -> bool {
  return lhs.has_value() ? *lhs < rhs : true;
}
template <class T, class U>
constexpr auto operator<(const U& lhs, const optional<T>& rhs) -> bool {
  return rhs.has_value() ? lhs < *rhs : false;
}
template <class T, class U>
constexpr auto operator<=(const optional<T>& lhs, const U& rhs) -> bool {
  return lhs.has_value() ? *lhs <= rhs : true;
}
template <class T, class U>
constexpr auto operator<=(const U& lhs, const optional<T>& rhs) -> bool {
  return rhs.has_value() ? lhs <= *rhs : false;
}
template <class T, class U>
constexpr auto operator>(const optional<T>& lhs, const U& rhs) -> bool {
  return lhs.has_value() ? *lhs > rhs : false;
}
template <class T, class U>
constexpr auto operator>(const U& lhs, const optional<T>& rhs) -> bool {
  return rhs.has_value() ? lhs > *rhs : true;
}
template <class T, class U>
constexpr auto operator>=(const optional<T>& lhs, const U& rhs) -> bool {
  return lhs.has_value() ? *lhs >= rhs : false;
}
template <class T, class U>
constexpr auto operator>=(const U& lhs, const optional<T>& rhs) -> bool {
  return rhs.has_value() ? lhs >= *rhs : true;
}

template <class T>
void swap(optional<T>& lhs, optional<T>& rhs) noexcept(noexcept(lhs.swap(rhs)))
  requires(std::is_swappable_v<T> && std::is_move_constructible_v<T>)
{
  return lhs.swap(rhs);
}

namespace detail {
struct i_am_secret {};
}  // namespace detail

template <class T = detail::i_am_secret, class U,
          class Ret = std::conditional_t<std::is_same_v<T, detail::i_am_secret>, std::decay_t<U>, T>>
constexpr auto make_optional(U&& v) -> optional<Ret> {
  return optional<Ret>(std::forward<U>(v));
}

template <class T, class... Args>
constexpr auto make_optional(Args&&... args) -> optional<T> {
  return optional<T>(std::in_place, std::forward<Args>(args)...);
}
template <class T, class U, class... Args>
constexpr auto make_optional(std::initializer_list<U> il, Args&&... args) -> optional<T> {
  return optional<T>(std::in_place, il, std::forward<Args>(args)...);
}

template <class T>
optional(T) -> optional<T>;

/// \exclude
namespace detail {
template <class Opt, class F, class Ret = decltype(std::invoke(std::declval<F>(), *std::declval<Opt>()))>
constexpr auto optional_map_impl(Opt&& opt, F&& f)
  requires(!std::is_void_v<Ret>)
{
  return opt.has_value() ? std::invoke(std::forward<F>(f), *std::forward<Opt>(opt)) : optional<Ret>(nullopt);
}

template <class Opt, class F, class Ret = decltype(std::invoke(std::declval<F>(), *std::declval<Opt>()))>
auto optional_map_impl(Opt&& opt, F&& f)
  requires std::is_void_v<Ret>
{
  if (opt.has_value()) {
    std::invoke(std::forward<F>(f), *std::forward<Opt>(opt));
    return make_optional(std::monostate{});
  }

  return optional<std::monostate>(nullopt);
}
}  // namespace detail

/// Specialization for when `T` is a reference. `optional<T&>` acts similarly
/// to a `T*`, but provides more operations and shows intent more clearly.
template <class T>
class optional<T&> {
 public:
  /// Carries out some operation which returns an optional on the stored
  /// object if there is one.
  template <class F>
  constexpr auto and_then(F&& f) & {
    using result = std::invoke_result_t<F, T&>;
    static_assert(detail::is_optional<result>::value, "F must return an optional");

    return has_value() ? std::invoke(std::forward<F>(f), **this) : result(nullopt);
  }

  template <class F>
  constexpr auto and_then(F&& f) && {
    using result = std::invoke_result_t<F, T&>;
    static_assert(detail::is_optional<result>::value, "F must return an optional");

    return has_value() ? std::invoke(std::forward<F>(f), **this) : result(nullopt);
  }

  template <class F>
  constexpr auto and_then(F&& f) const& {
    using result = std::invoke_result_t<F, const T&>;
    static_assert(detail::is_optional<result>::value, "F must return an optional");

    return has_value() ? std::invoke(std::forward<F>(f), **this) : result(nullopt);
  }

  template <class F>
  constexpr auto and_then(F&& f) const&& {
    using result = std::invoke_result_t<F, const T&>;
    static_assert(detail::is_optional<result>::value, "F must return an optional");

    return has_value() ? std::invoke(std::forward<F>(f), **this) : result(nullopt);
  }

  /// Carries out some operation on the stored object if there is one.
  template <class F>
  constexpr auto map(F&& f) & {
    return detail::optional_map_impl(*this, std::forward<F>(f));
  }

  template <class F>
  constexpr auto map(F&& f) && {
    return detail::optional_map_impl(std::move(*this), std::forward<F>(f));
  }

  template <class F>
  constexpr auto map(F&& f) const& {
    return detail::optional_map_impl(*this, std::forward<F>(f));
  }

  template <class F>
  constexpr auto map(F&& f) const&& {
    return detail::optional_map_impl(std::move(*this), std::forward<F>(f));
  }

  /// Carries out some operation on the stored object if there is one.
  template <class F>
  constexpr auto transform(F&& f) & {
    return detail::optional_map_impl(*this, std::forward<F>(f));
  }

  template <class F>
  constexpr auto transform(F&& f) && {
    return detail::optional_map_impl(std::move(*this), std::forward<F>(f));
  }

  template <class F>
  constexpr auto transform(F&& f) const& {
    return detail::optional_map_impl(*this, std::forward<F>(f));
  }

  template <class F>
  constexpr auto transform(F&& f) const&& {
    return detail::optional_map_impl(std::move(*this), std::forward<F>(f));
  }

  /// Calls `f` if the optional is empty
  template <class F, detail::enable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) & -> optional<T> {
    if (has_value()) return *this;

    std::forward<F>(f)();
    return nullopt;
  }

  template <class F, detail::disable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) & -> optional<T> {
    return has_value() ? *this : std::forward<F>(f)();
  }

  template <class F, detail::enable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) && -> optional<T> {
    if (has_value()) return std::move(*this);

    std::forward<F>(f)();
    return nullopt;
  }

  template <class F, detail::disable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) && -> optional<T> {
    return has_value() ? std::move(*this) : std::forward<F>(f)();
  }

  template <class F, detail::enable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) const& -> optional<T> {
    if (has_value()) return *this;

    std::forward<F>(f)();
    return nullopt;
  }

  template <class F, detail::disable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) const& -> optional<T> {
    return has_value() ? *this : std::forward<F>(f)();
  }

  template <class F, detail::enable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) const&& -> optional<T> {
    if (has_value()) return std::move(*this);

    std::forward<F>(f)();
    return nullopt;
  }

  template <class F, detail::disable_if_ret_void<F>* = nullptr>
  constexpr auto or_else(F&& f) const&& -> optional<T> {
    return has_value() ? std::move(*this) : std::forward<F>(f)();
  }

  /// Maps the stored value with `f` if there is one, otherwise returns `u`
  template <class F, class U>
  constexpr auto map_or(F&& f, U&& u) & -> U {
    return has_value() ? std::invoke(std::forward<F>(f), **this) : std::forward<U>(u);
  }

  template <class F, class U>
  constexpr auto map_or(F&& f, U&& u) && -> U {
    return has_value() ? std::invoke(std::forward<F>(f), std::move(**this)) : std::forward<U>(u);
  }

  template <class F, class U>
  constexpr auto map_or(F&& f, U&& u) const& -> U {
    return has_value() ? std::invoke(std::forward<F>(f), **this) : std::forward<U>(u);
  }

  template <class F, class U>
  constexpr auto map_or(F&& f, U&& u) const&& -> U {
    return has_value() ? std::invoke(std::forward<F>(f), std::move(**this)) : std::forward<U>(u);
  }

  /// Maps the stored value with `f` if there is one, otherwise calls
  /// `u` and returns the result.
  template <class F, class U>
  constexpr auto map_or_else(F&& f, U&& u) & -> std::invoke_result_t<U> {
    return has_value() ? std::invoke(std::forward<F>(f), **this) : std::forward<U>(u)();
  }

  template <class F, class U>
  constexpr auto map_or_else(F&& f, U&& u) && -> std::invoke_result_t<U> {
    return has_value() ? std::invoke(std::forward<F>(f), std::move(**this)) : std::forward<U>(u)();
  }

  template <class F, class U>
  constexpr auto map_or_else(F&& f, U&& u) const& -> std::invoke_result_t<U> {
    return has_value() ? std::invoke(std::forward<F>(f), **this) : std::forward<U>(u)();
  }

  template <class F, class U>
  constexpr auto map_or_else(F&& f, U&& u) const&& -> std::invoke_result_t<U> {
    return has_value() ? std::invoke(std::forward<F>(f), std::move(**this)) : std::forward<U>(u)();
  }

  /// Returns `u` if `*this` has a value, otherwise an empty optional.
  template <class U>
  constexpr auto conjunction(U&& u) const -> optional<std::decay_t<U>> {
    using result = optional<std::decay_t<U>>;
    return has_value() ? result{u} : result{nullopt};
  }

  /// Returns `rhs` if `*this` is empty, otherwise the current value.
  constexpr auto disjunction(const optional& rhs) & -> optional { return has_value() ? *this : rhs; }

  constexpr auto disjunction(const optional& rhs) const& -> optional { return has_value() ? *this : rhs; }

  constexpr auto disjunction(const optional& rhs) && -> optional { return has_value() ? std::move(*this) : rhs; }

  constexpr auto disjunction(const optional& rhs) const&& -> optional { return has_value() ? std::move(*this) : rhs; }

  constexpr auto disjunction(optional&& rhs) & -> optional { return has_value() ? *this : std::move(rhs); }

  constexpr auto disjunction(optional&& rhs) const& -> optional { return has_value() ? *this : std::move(rhs); }

  constexpr auto disjunction(optional&& rhs) && -> optional { return has_value() ? std::move(*this) : std::move(rhs); }

  constexpr auto disjunction(optional&& rhs) const&& -> optional {
    return has_value() ? std::move(*this) : std::move(rhs);
  }

  /// Takes the value out of the optional, leaving it empty
  constexpr auto take() -> optional {
    optional ret = std::move(*this);
    reset();
    return ret;
  }

  using value_type = T&;

  /// Constructs an optional that does not contain a value.
  constexpr optional() noexcept : value_(nullptr) {}

  constexpr optional(nullopt_t /*unused*/) noexcept : value_(nullptr) {}  // NOLINT(google-explicit-constructor)

  /// Copy constructor
  ///
  /// If `rhs` contains a value, the stored value is direct-initialized with
  /// it. Otherwise, the constructed optional is empty.
  constexpr optional(const optional& rhs) noexcept = default;

  /// Move constructor
  ///
  /// If `rhs` contains a value, the stored value is direct-initialized with
  /// it. Otherwise, the constructed optional is empty.
  constexpr optional(optional&& rhs) = default;

  /// Constructs the stored value with `u`.
  template <class U = T>
    requires(!detail::is_optional_v<std::decay_t<U>>)
  constexpr optional(U&& u) noexcept : value_(std::addressof(u)) {  // NOLINT(google-explicit-constructor)
    static_assert(std::is_lvalue_reference_v<U>, "U must be an lvalue");
  }

  template <class U>
  constexpr explicit optional(const optional<U>& rhs) noexcept : optional(*rhs) {}

  /// No-op
  ~optional() = default;

  /// Assignment to empty.
  ///
  /// Destroys the current value if there is one.
  auto operator=(nullopt_t /*unused*/) noexcept -> optional& {
    value_ = nullptr;
    return *this;
  }

  /// Copy assignment.
  ///
  /// Rebinds this optional to the referee of `rhs` if there is one. Otherwise
  /// resets the stored value in `*this`.
  auto operator=(const optional& rhs) -> optional& = default;

  /// Rebinds this optional to `u`.
  template <class U = T>
    requires(!detail::is_optional_v<std::decay_t<U>>)
  auto operator=(U&& u) -> optional& {
    static_assert(std::is_lvalue_reference_v<U>, "U must be an lvalue");
    value_ = std::addressof(u);
    return *this;
  }

  /// Converting copy assignment operator.
  ///
  /// Rebinds this optional to the referee of `rhs` if there is one. Otherwise
  /// resets the stored value in `*this`.
  template <class U>
  auto operator=(const optional<U>& rhs) noexcept -> optional& {
    value_ = std::addressof(rhs.value());
    return *this;
  }

  /// Rebinds this optional to `u`.
  template <class U = T>
    requires(!detail::is_optional_v<std::decay_t<U>>)
  auto emplace(U&& u) noexcept -> optional& {
    return *this = std::forward<U>(u);
  }

  void swap(optional& rhs) noexcept { std::swap(value_, rhs.value_); }

  /// Returns a pointer to the stored value
  [[nodiscard]] constexpr auto operator->() const noexcept -> const T* { return value_; }

  [[nodiscard]] constexpr auto operator->() noexcept -> T* { return value_; }

  /// Returns the stored value
  [[nodiscard]] constexpr auto operator*() noexcept -> T& { return *value_; }

  [[nodiscard]] constexpr auto operator*() const noexcept -> const T& { return *value_; }

  [[nodiscard]] constexpr auto has_value() const noexcept -> bool { return value_ != nullptr; }

  [[nodiscard]] constexpr explicit operator bool() const noexcept { return value_ != nullptr; }

  /// Returns the contained value if there is one, otherwise throws
  /// bad_optional_access
  [[nodiscard]] constexpr auto value() -> T& {
    if (has_value()) return *value_;
    throw bad_optional_access();
  }
  [[nodiscard]] constexpr auto value() const -> const T& {
    if (has_value()) return *value_;
    throw bad_optional_access();
  }

  /// Returns the stored value if there is one, otherwise returns `u`
  template <class U>
  [[nodiscard]] constexpr auto value_or(U&& u) const& noexcept -> T {
    static_assert(std::is_copy_constructible_v<T> && std::is_convertible_v<U&&, T>,
                  "T must be copy constructible and convertible from U");
    return has_value() ? **this : static_cast<T>(std::forward<U>(u));
  }

  /// \group value_or
  template <class U>
  [[nodiscard]] constexpr auto value_or(U&& u) && noexcept -> T {
    static_assert(std::is_move_constructible_v<T> && std::is_convertible_v<U&&, T>,
                  "T must be move constructible and convertible from U");
    return has_value() ? **this : static_cast<T>(std::forward<U>(u));
  }

  /// Destroys the stored value if one exists, making the optional empty
  void reset() noexcept { value_ = nullptr; }

 private:
  T* value_;
};  // namespace tl

}  // namespace tl

namespace std {
// TODO SFINAE
template <class T>
struct hash<tl::optional<T>> {
  auto operator()(const tl::optional<T>& o) const -> ::std::size_t {
    if (!o.has_value()) return 0;

    return std::hash<std::remove_const_t<T>>()(*o);
  }
};
}  // namespace std

#endif
