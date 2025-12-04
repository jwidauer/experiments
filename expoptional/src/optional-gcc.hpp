#ifndef OPTIONAL_GCC
#define OPTIONAL_GCC

#include <bits/enable_special_members.h>
#include <bits/exception_defines.h>
#include <bits/functional_hash.h>
#include <bits/stl_construct.h>  // _Construct

#include <cassert>
#include <compare>
#include <concepts>
#include <exception>
#include <functional>
#include <initializer_list>
#include <optional>
#include <type_traits>

namespace nostd {

template <typename Tp>
class Optional;

template <typename Fn>
struct OptionalFunc {
  Fn& func;
};

/**
 *  @brief Exception class thrown when a disengaged optional object is
 *  dereferenced.
 *  @ingroup exceptions
 */
class BadOptionalAccess : public std::exception {
 public:
  BadOptionalAccess() = default;
  ~BadOptionalAccess() override = default;

  [[nodiscard]] auto what() const noexcept -> const char* override { return "bad optional access"; }
};

// XXX Does not belong here.
[[noreturn]] inline void throw_bad_optional_access() { _GLIBCXX_THROW_OR_ABORT(BadOptionalAccess()); }

// This class template manages construction/destruction of
// the contained value for a std::optional.
template <typename Tp>
struct OptionalPayloadBase {
  using StoredType = std::remove_const_t<Tp>;

  OptionalPayloadBase() = default;
  ~OptionalPayloadBase() = default;

  template <typename... Args>
  constexpr OptionalPayloadBase(std::in_place_t tag, Args&&... args)  // NOLINT(google-explicit-constructor)
      : payload(tag, std::forward<Args>(args)...), engaged(true) {}

  template <typename Up, typename... Args>
  constexpr OptionalPayloadBase(std::initializer_list<Up> il, Args&&... args)
      : payload(il, std::forward<Args>(args)...), engaged(true) {}

  // Constructor used by OptionalBase copy constructor when the
  // contained value is not trivially copy constructible.
  constexpr OptionalPayloadBase(bool /* engaged */, const OptionalPayloadBase& other) {
    if (other.engaged) this->construct(other.get());
  }

  // Constructor used by OptionalBase move constructor when the
  // contained value is not trivially move constructible.
  constexpr OptionalPayloadBase(bool /* engaged */, OptionalPayloadBase&& other) {
    if (other.engaged) this->construct(std::move(other.get()));
  }

  // Copy constructor is only used to when the contained value is
  // trivially copy constructible.
  OptionalPayloadBase(const OptionalPayloadBase&) = default;

  // Move constructor is only used to when the contained value is
  // trivially copy constructible.
  OptionalPayloadBase(OptionalPayloadBase&&) = default;

  auto operator=(const OptionalPayloadBase&) -> OptionalPayloadBase& = default;

  auto operator=(OptionalPayloadBase&&) -> OptionalPayloadBase& = default;

  // used to perform non-trivial copy assignment.
  constexpr void copy_assign(const OptionalPayloadBase& other) {
    if (this->engaged && other.engaged)
      this->get() = other.get();
    else {
      if (other.engaged)
        this->construct(other.get());
      else
        this->reset();
    }
  }

  // used to perform non-trivial move assignment.
  constexpr void move_assign(OptionalPayloadBase&& other) noexcept(
      std::conjunction_v<std::is_nothrow_move_constructible<Tp>, std::is_nothrow_move_assignable<Tp>>) {
    if (this->engaged && other.engaged)
      this->get() = std::move(other.get());
    else {
      if (other.engaged)
        this->construct(std::move(other.get()));
      else
        this->reset();
    }
  }

  struct EmptyByte {};

  template <typename Up, bool = std::is_trivially_destructible_v<Up>>
  union Storage {
    constexpr Storage() noexcept : empty() {}

    template <typename... Args>
    constexpr Storage(std::in_place_t /*unused*/, Args&&... args)  // NOLINT(google-explicit-constructor)
        : value(std::forward<Args>(args)...) {}

    template <typename Vp, typename... Args>
    constexpr Storage(std::initializer_list<Vp> il, Args&&... args) : value(il, std::forward<Args>(args)...) {}

    template <typename Fn, typename Arg>
    constexpr Storage(OptionalFunc<Fn> f, Arg&& arg)
        : value(std::invoke(std::forward<Fn>(f.func), std::forward<Arg>(arg))) {}

    EmptyByte empty;
    Up value;
  };

  template <typename Up>
  union Storage<Up, false> {
    constexpr Storage() noexcept : empty() {}

    template <typename... Args>
    constexpr Storage(std::in_place_t /*unused*/, Args&&... args)  // NOLINT(google-explicit-constructor)
        : value(std::forward<Args>(args)...) {}

    template <typename Vp, typename... Args>
    constexpr Storage(std::initializer_list<Vp> il, Args&&... args) : value(il, std::forward<Args>(args)...) {}

    template <typename Fn, typename Arg>
    constexpr Storage(OptionalFunc<Fn> f, Arg&& arg)
        : value(std::__invoke(std::forward<Fn>(f._M_f), std::forward<Arg>(arg))) {}

    // User-provided destructor is needed when _Up has non-trivial dtor.
    constexpr ~Storage() {}

    EmptyByte empty;
    Up value;
  };

  Storage<StoredType> payload;
  bool engaged = false;

  template <typename... Args>
  constexpr void construct(Args&&... args) noexcept(std::is_nothrow_constructible_v<StoredType, Args...>) {
    std::_Construct(std::__addressof(this->payload._M_value), std::forward<Args>(args)...);
    this->engaged = true;
  }

  constexpr void destroy() noexcept {
    engaged = false;
    payload._M_value.~StoredType();
  }

  template <typename Fn, typename Up>
  constexpr void apply(OptionalFunc<Fn> f, Up&& x) {
    std::construct_at(std::__addressof(this->payload), f, std::forward<Up>(x));
    engaged = true;
  }

  // The _M_get() operations have _M_engaged as a precondition.
  // They exist to access the contained value with the appropriate
  // const-qualification, because _M_payload has had the const removed.

  constexpr auto get() noexcept -> Tp& { return this->payload._M_value; }

  constexpr auto get() const noexcept -> const Tp& { return this->payload._M_value; }

  // _M_reset is a 'safe' operation with no precondition.
  constexpr void reset() noexcept {
    if (this->engaged)
      destroy();
    else  // This seems redundant but improves codegen, see PR 112480.
      this->engaged = false;
  }
};

// Class template that manages the payload for optionals.
template <
    typename Tp, bool /* HasTrivialDestructor*/ = std::is_trivially_destructible_v<Tp>,
    bool /* HasTrivialCopy */ = std::is_trivially_copy_assignable_v<Tp> && std::is_trivially_copy_constructible_v<Tp>,
    bool /* HasTrivialMove */ = std::is_trivially_move_assignable_v<Tp> && std::is_trivially_move_constructible_v<Tp>>
struct OptionalPayload;

// Payload for potentially-constexpr optionals (trivial copy/move/destroy).
template <typename T>
struct OptionalPayload<T, true, true, true> : OptionalPayloadBase<T> {
  using OptionalPayloadBase<T>::OptionalPayloadBase;

  OptionalPayload() = default;
};

// Payload for optionals with non-trivial copy construction/assignment.
template <typename T>
struct OptionalPayload<T, true, false, true> : OptionalPayloadBase<T> {
  using OptionalPayloadBase<T>::OptionalPayloadBase;

  OptionalPayload() = default;
  ~OptionalPayload() = default;
  OptionalPayload(const OptionalPayload&) = default;
  OptionalPayload(OptionalPayload&&) = default;
  auto operator=(OptionalPayload&&) -> OptionalPayload& = default;

  // Non-trivial copy assignment.
  constexpr auto operator=(const OptionalPayload& other) -> OptionalPayload& {
    this->copy_assign(other);
    return *this;
  }
};

// Payload for optionals with non-trivial move construction/assignment.
template <typename Tp>
struct OptionalPayload<Tp, true, true, false> : OptionalPayloadBase<Tp> {
  using OptionalPayloadBase<Tp>::OptionalPayloadBase;

  OptionalPayload() = default;
  ~OptionalPayload() = default;
  OptionalPayload(const OptionalPayload&) = default;
  OptionalPayload(OptionalPayload&&) = default;
  auto operator=(const OptionalPayload&) -> OptionalPayload& = default;

  // Non-trivial move assignment.
  constexpr auto operator=(OptionalPayload&& other) noexcept(
      std::conjunction_v<std::is_nothrow_move_constructible<Tp>, std::is_nothrow_move_assignable<Tp>>)
      -> OptionalPayload& {
    this->move_assign(std::move(other));
    return *this;
  }
};

// Payload for optionals with non-trivial copy and move assignment.
template <typename Tp>
struct OptionalPayload<Tp, true, false, false> : OptionalPayloadBase<Tp> {
  using OptionalPayloadBase<Tp>::OptionalPayloadBase;

  OptionalPayload() = default;
  ~OptionalPayload() = default;
  OptionalPayload(const OptionalPayload&) = default;
  OptionalPayload(OptionalPayload&&) = default;

  // Non-trivial copy assignment.
  constexpr auto operator=(const OptionalPayload& other) -> OptionalPayload& {
    this->copy_assign(other);
    return *this;
  }

  // Non-trivial move assignment.
  constexpr auto operator=(OptionalPayload&& other) noexcept(
      std::conjunction_v<std::is_nothrow_move_constructible<Tp>, std::is_nothrow_move_assignable<Tp>>)
      -> OptionalPayload& {
    this->move_assign(std::move(other));
    return *this;
  }
};

// Payload for optionals with non-trivial destructors.
template <typename Tp, bool Copy, bool Move>
struct OptionalPayload<Tp, false, Copy, Move> : OptionalPayload<Tp, true, false, false> {
  // Base class implements all the constructors and assignment operators:
  using OptionalPayload<Tp, true, false, false>::OptionalPayload;

  OptionalPayload() = default;
  OptionalPayload(const OptionalPayload&) = default;
  OptionalPayload(OptionalPayload&&) = default;
  auto operator=(const OptionalPayload&) -> OptionalPayload& = default;
  auto operator=(OptionalPayload&&) -> OptionalPayload& = default;

  // Destructor needs to destroy the contained value:
  constexpr ~OptionalPayload() { this->reset(); }
};

// Common base class for OptionalBase<T> to avoid repeating these
// member functions in each specialization.
template <typename Tp, typename Dp>
class OptionalBaseImpl {
 protected:
  using StoredType = std::remove_const_t<Tp>;

  // The _M_construct operation has !_M_engaged as a precondition
  // while _M_destruct has _M_engaged as a precondition.
  template <typename... Args>
  constexpr void construct(Args&&... args) noexcept(std::is_nothrow_constructible_v<StoredType, Args...>) {
    static_cast<Dp*>(this)->payload.construct(std::forward<Args>(args)...);
  }

  constexpr void destruct() noexcept { static_cast<Dp*>(this)->payload.destroy(); }

  // _M_reset is a 'safe' operation with no precondition.
  constexpr void reset() noexcept { static_cast<Dp*>(this)->payload.reset(); }

  [[nodiscard]] constexpr auto is_engaged() const noexcept -> bool {
    return static_cast<const Dp*>(this)->payload.engaged;
  }

  // The _M_get operations have _M_engaged as a precondition.
  constexpr auto get() noexcept -> Tp& {
    assert(this->is_engaged());
    return static_cast<Dp*>(this)->payload.get();
  }

  constexpr auto get() const noexcept -> const Tp& {
    assert(this->is_engaged());
    return static_cast<const Dp*>(this)->payload.get();
  }
};

/**
 * @brief Class template that provides copy/move constructors of optional.
 *
 * Such a separate base class template is necessary in order to
 * conditionally make copy/move constructors trivial.
 *
 * When the contained value is trivially copy/move constructible,
 * the copy/move constructors of _Optional_base will invoke the
 * trivial copy/move constructor of _Optional_payload. Otherwise,
 * they will invoke _Optional_payload(bool, const _Optional_payload&)
 * or _Optional_payload(bool, _Optional_payload&&) to initialize
 * the contained value, if copying/moving an engaged optional.
 *
 * Whether the other special members are trivial is determined by the
 * _Optional_payload<_Tp> specialization used for the _M_payload member.
 *
 * @see optional, _Enable_special_members
 */
template <typename Tp, bool = std::is_trivially_copy_constructible_v<Tp>,
          bool = std::is_trivially_move_constructible_v<Tp>>
struct OptionalBase : OptionalBaseImpl<Tp, OptionalBase<Tp>> {
  // Constructors for disengaged optionals.
  constexpr OptionalBase() = default;

  // Constructors for engaged optionals.
  template <typename... Args>
    requires(std::is_constructible_v<Tp, Args...>)
  constexpr explicit OptionalBase(std::in_place_t /*unused*/, Args&&... args)
      : payload(std::in_place, std::forward<Args>(args)...) {}

  template <typename Up, typename... Args>
    requires(std::is_constructible_v<Tp, std::initializer_list<Up>&, Args...>)
  constexpr explicit OptionalBase(std::in_place_t /*unused*/, std::initializer_list<Up> il, Args&&... args)
      : payload(std::in_place, il, std::forward<Args>(args)...) {}

  // Copy and move constructors.
  constexpr OptionalBase(const OptionalBase& other) : payload(other.payload.engaged, other.payload) {}

  constexpr OptionalBase(OptionalBase&& other) noexcept(std::is_nothrow_move_constructible_v<Tp>)
      : payload(other.payload.engaged, std::move(other.payload)) {}

  // Assignment operators.
  auto operator=(const OptionalBase&) -> OptionalBase& = default;
  auto operator=(OptionalBase&&) -> OptionalBase& = default;

  OptionalPayload<Tp> payload;
};

template <typename Tp>
struct OptionalBase<Tp, false, true> : OptionalBaseImpl<Tp, OptionalBase<Tp>> {
  // Constructors for disengaged optionals.
  constexpr OptionalBase() = default;

  // Constructors for engaged optionals.
  template <typename... Args>
    requires(std::is_constructible_v<Tp, Args...>)
  constexpr explicit OptionalBase(std::in_place_t /*unused*/, Args&&... args)
      : payload(std::in_place, std::forward<Args>(args)...) {}

  template <typename Up, typename... Args>
    requires(std::is_constructible_v<Tp, std::initializer_list<Up>&, Args...>)
  constexpr explicit OptionalBase(std::in_place_t /*unused*/, std::initializer_list<Up> il, Args... args)
      : payload(std::in_place, il, std::forward<Args>(args)...) {}

  // Copy and move constructors.
  constexpr OptionalBase(const OptionalBase& other) : payload(other.payload.engaged, other.payload) {}

  constexpr OptionalBase(OptionalBase&& other) = default;

  // Assignment operators.
  auto operator=(const OptionalBase&) -> OptionalBase& = default;
  auto operator=(OptionalBase&&) -> OptionalBase& = default;

  OptionalPayload<Tp> payload;
};

template <typename Tp>
struct OptionalBase<Tp, true, false> : OptionalBaseImpl<Tp, OptionalBase<Tp>> {
  // Constructors for disengaged optionals.
  constexpr OptionalBase() = default;

  // Constructors for engaged optionals.
  template <typename... Args>
    requires(std::is_constructible_v<Tp, Args...>)
  constexpr explicit OptionalBase(std::in_place_t /*unused*/, Args&&... args)
      : payload(std::in_place, std::forward<Args>(args)...) {}

  template <typename Up, typename... Args>
    requires(std::is_constructible_v<Tp, std::initializer_list<Up>&, Args...>)
  constexpr explicit OptionalBase(std::in_place_t /*unused*/, std::initializer_list<Up> il, Args&&... args)
      : payload(std::in_place, il, std::forward<Args>(args)...) {}

  // Copy and move constructors.
  constexpr OptionalBase(const OptionalBase& other) = default;

  constexpr OptionalBase(OptionalBase&& other) noexcept(std::is_nothrow_move_constructible_v<Tp>)
      : payload(other.payload.engaged, std::move(other.payload)) {}

  // Assignment operators.
  auto operator=(const OptionalBase&) -> OptionalBase& = default;
  auto operator=(OptionalBase&&) -> OptionalBase& = default;

  OptionalPayload<Tp> payload;
};

template <typename Tp>
struct OptionalBase<Tp, true, true> : OptionalBaseImpl<Tp, OptionalBase<Tp>> {
  // Constructors for disengaged optionals.
  constexpr OptionalBase() = default;

  // Constructors for engaged optionals.
  template <typename... Args>
    requires(std::is_constructible_v<Tp, Args...>)
  constexpr explicit OptionalBase(std::in_place_t /*unused*/, Args&&... args)
      : payload(std::in_place, std::forward<Args>(args)...) {}

  template <typename Up, typename... Args>
    requires(std::is_constructible_v<Tp, std::initializer_list<Up>&, Args...>)
  constexpr explicit OptionalBase(std::in_place_t /*unused*/, std::initializer_list<Up> il, Args&&... args)
      : payload(std::in_place, il, std::forward<Args>(args)...) {}

  // Copy and move constructors.
  constexpr OptionalBase(const OptionalBase& other) = default;
  constexpr OptionalBase(OptionalBase&& other) = default;

  // Assignment operators.
  auto operator=(const OptionalBase&) -> OptionalBase& = default;
  auto operator=(OptionalBase&&) -> OptionalBase& = default;

  OptionalPayload<Tp> payload;
};

template <typename Tp>
class Optional;

template <typename Tp>
inline constexpr bool is_optional_v = false;
template <typename Tp>
inline constexpr bool is_optional_v<Optional<Tp>> = true;

template <typename Tp, typename Up>
using ConvertsFromOptional =
    std::disjunction<std::is_constructible<Tp, const Optional<Up>&>, std::is_constructible<Tp, Optional<Up>&>,
                     std::is_constructible<Tp, const Optional<Up>&&>, std::is_constructible<Tp, Optional<Up>&&>,
                     std::is_convertible<const Optional<Up>&, Tp>, std::is_convertible<Optional<Up>&, Tp>,
                     std::is_convertible<const Optional<Up>&&, Tp>, std::is_convertible<Optional<Up>&&, Tp>>;

template <typename Tp, typename Up>
inline constexpr bool is_convertible_from_optional_v =
    std::is_constructible_v<Tp, const Optional<Up>&> || std::is_constructible_v<Tp, Optional<Up>&> ||
    std::is_constructible_v<Tp, const Optional<Up>&&> || std::is_constructible_v<Tp, Optional<Up>&&> ||
    std::is_convertible_v<const Optional<Up>&, Tp> || std::is_convertible_v<Optional<Up>&, Tp> ||
    std::is_convertible_v<const Optional<Up>&&, Tp> || std::is_convertible_v<Optional<Up>&&, Tp>;

template <typename Tp, typename Up>
using AssignsFromOptional =
    std::disjunction<std::is_assignable<Tp&, const Optional<Up>&>, std::is_assignable<Tp&, Optional<Up>&>,
                     std::is_assignable<Tp&, const Optional<Up>&&>, std::is_assignable<Tp&, Optional<Up>&&>>;

template <typename Tp, typename Up>
inline constexpr bool is_assignable_from_optional_v =
    std::is_assignable_v<Tp&, const Optional<Up>&> || std::is_assignable_v<Tp&, Optional<Up>&> ||
    std::is_assignable_v<Tp&, const Optional<Up>&&> || std::is_assignable_v<Tp&, Optional<Up>&&>;

/**
 * @brief Class template for optional values.
 */
template <typename Tp>
class Optional : private OptionalBase<Tp>,
                 private std::_Enable_copy_move<
                     // Copy constructor.
                     std::is_copy_constructible_v<Tp>,
                     // Copy assignment.
                     std::conjunction_v<std::is_copy_constructible<Tp>, std::is_copy_assignable<Tp>>,
                     // Move constructor.
                     std::is_move_constructible_v<Tp>,
                     // Move assignment.
                     std::conjunction_v<std::is_move_constructible<Tp>, std::is_move_assignable<Tp>>,
                     // Unique tag type.
                     Optional<Tp>> {
  static_assert(!std::is_same_v<std::remove_cv_t<Tp>, std::nullopt_t>);
  static_assert(!std::is_same_v<std::remove_cv_t<Tp>, std::in_place_t>);
  static_assert(std::is_object_v<Tp> && !std::is_array_v<Tp>);

 private:
  using Base = OptionalBase<Tp>;

  // SFINAE helpers
  template <typename Up>
  using NotSelf = std::negation<std::is_same<Optional, std::remove_cvref_t<Up>>>;

  template <typename Up>
  static constexpr bool is_self = std::is_same_v<Optional, std::remove_cvref_t<Up>>;
  template <typename Up>
  static constexpr bool is_not_self = !is_self<Up>;

  template <typename Up>
  using NotTag = std::negation<std::is_same<std::in_place_t, std::remove_cvref_t<Up>>>;
  template <typename Up>
  static constexpr bool is_tag = std::is_same_v<std::in_place_t, std::remove_cvref_t<Up>>;
  template <typename Up>
  static constexpr bool is_not_tag = !is_tag<Up>;

  template <typename... Cond>
  using Requires = std::enable_if_t<std::conjunction_v<Cond...>, bool>;

 public:
  using value_type = Tp;  // NOLINT(readability-identifier-naming)

  constexpr Optional() noexcept = default;

  constexpr Optional(std::nullopt_t /*unused*/) noexcept {}  // NOLINT(google-explicit-constructor)

  // Converting constructors for engaged optionals.
  template <typename Up = Tp>
    requires(is_not_self<Up> && is_not_tag<Up> && std::is_constructible_v<Tp, Up> && std::is_convertible_v<Up, Tp>)
  constexpr Optional(Up&& t) noexcept(std::is_nothrow_constructible_v<Tp, Up>)  // NOLINT(google-explicit-constructor)
      : Base(std::in_place, std::forward<Up>(t)) {}

  template <typename Up = Tp>
    requires(is_not_self<Up> && is_not_tag<Up> && std::is_constructible_v<Tp, Up> && !std::is_convertible_v<Up, Tp>)
  explicit constexpr Optional(Up&& t) noexcept(std::is_nothrow_constructible_v<Tp, Up>)
      : Base(std::in_place, std::forward<Up>(t)) {}

  template <typename Up>
    requires(!std::is_same_v<Tp, Up> && std::is_constructible_v<Tp, const Up&> &&
             std::is_convertible_v<const Up&, Tp> && !is_convertible_from_optional_v<Tp, Up>)
  constexpr Optional(const Optional<Up>& t) noexcept(  // NOLINT(google-explicit-constructor
      std::is_nothrow_constructible_v<Tp, const Up&>) {
    if (t) emplace(*t);
  }

  template <typename Up, Requires<std::negation<std::is_same<Tp, Up>>, std::is_constructible<Tp, const Up&>,
                                  std::negation<std::is_convertible<const Up&, Tp>>,
                                  std::negation<ConvertsFromOptional<Tp, Up>>> = false>
  explicit constexpr Optional(const Optional<Up>& t) noexcept(std::is_nothrow_constructible_v<Tp, const Up&>) {
    if (t) emplace(*t);
  }

  template <typename Up, Requires<std::negation<std::is_same<Tp, Up>>, std::is_constructible<Tp, Up>,
                                  std::is_convertible<Up, Tp>, std::negation<ConvertsFromOptional<Tp, Up>>> = true>
  constexpr Optional(Optional<Up>&& t) noexcept(  // NOLINT(google-explicit-constructor)
      std::is_nothrow_constructible_v<Tp, Up>) {
    if (t) emplace(std::move(*t));
  }

  template <typename Up,
            Requires<std::negation<std::is_same<Tp, Up>>, std::is_constructible<Tp, Up>,
                     std::negation<std::is_convertible<Up, Tp>>, std::negation<ConvertsFromOptional<Tp, Up>>> = false>
  explicit constexpr Optional(Optional<Up>&& t) noexcept(std::is_nothrow_constructible_v<Tp, Up>) {
    if (t) emplace(std::move(*t));
  }

  template <typename... Args, Requires<std::is_constructible<Tp, Args...>> = false>
  explicit constexpr Optional(std::in_place_t /*unused*/,
                              Args&&... args) noexcept(std::is_nothrow_constructible_v<Tp, Args...>)
      : Base(std::in_place, std::forward<Args>(args)...) {}

  template <typename Up, typename... Args,
            Requires<std::is_constructible<Tp, std::initializer_list<Up>&, Args...>> = false>
  explicit constexpr Optional(std::in_place_t /*unused*/, std::initializer_list<Up> il, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<Tp, std::initializer_list<Up>&, Args...>)
      : Base(std::in_place, il, std::forward<Args>(args)...) {}

  // Assignment operators.
  constexpr auto operator=(std::nullopt_t /*unused*/) noexcept -> Optional& {
    this->reset();
    return *this;
  }

  template <typename Up = Tp>
    requires(std::conjunction_v<NotSelf<Up>,
                                std::negation<std::conjunction<std::is_scalar<Tp>, std::is_same<Tp, std::decay_t<Up>>>>,
                                std::is_constructible<Tp, Up>, std::is_assignable<Tp&, Up>>)
  constexpr auto operator=(Up&& u) noexcept(
      std::conjunction_v<std::is_nothrow_constructible<Tp, Up>, std::is_nothrow_assignable<Tp&, Up>>) -> Optional& {
    if (this->is_engaged())
      this->get() = std::forward<Up>(u);
    else
      this->construct(std::forward<Up>(u));

    return *this;
  }

  template <typename Up>
    requires(std::conjunction_v<std::negation<std::is_same<Tp, Up>>, std::is_constructible<Tp, const Up&>,
                                std::is_assignable<Tp&, const Up&>, std::negation<ConvertsFromOptional<Tp, Up>>,
                                std::negation<AssignsFromOptional<Tp, Up>>>)
  constexpr auto operator=(const Optional<Up>& u) noexcept(
      std::conjunction_v<std::is_nothrow_constructible<Tp, const Up&>, std::is_nothrow_assignable<Tp&, const Up&>>)
      -> Optional& {
    if (u) {
      if (this->is_engaged())
        this->get() = *u;
      else
        this->construct(*u);
    } else {
      this->reset();
    }
    return *this;
  }

  template <typename Up>
    requires(std::conjunction_v<std::negation<std::is_same<Tp, Up>>, std::is_constructible<Tp, Up>,
                                std::is_assignable<Tp&, Up>, std::negation<ConvertsFromOptional<Tp, Up>>,
                                std::negation<AssignsFromOptional<Tp, Up>>>)
  constexpr auto operator=(Optional<Up>&& u) noexcept(
      std::conjunction_v<std::is_nothrow_constructible<Tp, Up>, std::is_nothrow_assignable<Tp&, Up>>) -> Optional& {
    if (u) {
      if (this->is_engaged())
        this->get() = std::move(*u);
      else
        this->construct(std::move(*u));
    } else {
      this->reset();
    }

    return *this;
  }

  template <typename... Args>
    requires(std::is_constructible_v<Tp, Args...>)
  constexpr auto emplace(Args&&... args) noexcept(std::is_nothrow_constructible_v<Tp, Args...>) -> Tp& {
    this->reset();
    this->construct(std::forward<Args>(args)...);
    return this->get();
  }

  template <typename Up, typename... Args>
  constexpr auto emplace(std::initializer_list<Up> il, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<Tp, std::initializer_list<Up>&, Args...>) -> Tp&
    requires(std::is_constructible_v<Tp, std::initializer_list<Up>&, Args...>)
  {
    this->reset();
    this->construct(il, std::forward<Args>(args)...);
    return this->get();
  }

  // Destructor is implicit, implemented in _Optional_base.

  // Swap.
  constexpr void swap(Optional& other) noexcept(std::is_nothrow_move_constructible_v<Tp> &&
                                                std::is_nothrow_swappable_v<Tp>) {
    using std::swap;

    if (this->is_engaged() && other.is_engaged())
      swap(this->get(), other.get());
    else if (this->is_engaged()) {
      other.construct(std::move(this->get()));
      this->destruct();
    } else if (other.is_engaged()) {
      this->construct(std::move(other.get()));
      other.destruct();
    }
  }

  // Observers.
  constexpr auto operator->() const noexcept -> const Tp* { return std::addressof(this->get()); }

  constexpr auto operator->() noexcept -> Tp* { return std::addressof(this->get()); }

  constexpr auto operator*() const& noexcept -> const Tp& { return this->get(); }

  constexpr auto operator*() & noexcept -> Tp& { return this->get(); }

  constexpr auto operator*() && noexcept -> Tp&& { return std::move(this->get()); }

  constexpr auto operator*() const&& noexcept -> const Tp&& { return std::move(this->get()); }

  constexpr explicit operator bool() const noexcept { return this->is_engaged(); }

  [[nodiscard]] constexpr auto has_value() const noexcept -> bool { return this->is_engaged(); }

  constexpr auto value() const& -> const Tp& {
    if (this->is_engaged()) return this->get();
    throw_bad_optional_access();
  }

  constexpr auto value() & -> Tp& {
    if (this->is_engaged()) return this->get();
    throw_bad_optional_access();
  }

  constexpr auto value() && -> Tp&& {
    if (this->is_engaged()) return std::move(this->get());
    throw_bad_optional_access();
  }

  constexpr auto value() const&& -> const Tp&& {
    if (this->is_engaged()) return std::move(this->get());
    throw_bad_optional_access();
  }

  template <typename Up>
  constexpr auto value_or(Up&& u) const& -> Tp {
    static_assert(std::is_copy_constructible_v<Tp>);
    static_assert(std::is_convertible_v<Up&&, Tp>);

    if (this->is_engaged()) return this->get();
    return static_cast<Tp>(std::forward<Up>(u));
  }

  template <typename Up>
  constexpr auto value_or(Up&& u) && -> Tp {
    static_assert(std::is_move_constructible_v<Tp>);
    static_assert(std::is_convertible_v<Up&&, Tp>);

    if (this->is_engaged()) return std::move(this->get());
    return static_cast<Tp>(std::forward<Up>(u));
  }

  // [optional.monadic]

  template <typename Fn>
  constexpr auto and_then(Fn&& f) & {
    using Up = std::remove_cvref_t<std::invoke_result_t<Fn, Tp&>>;
    static_assert(is_optional_v<std::remove_cvref_t<Up>>,
                  "the function passed to std::optional<T>::and_then must return a std::optional");
    if (has_value()) return std::invoke(std::forward<Fn>(f), **this);
    return Up();
  }

  template <typename Fn>
  constexpr auto and_then(Fn&& f) const& {
    using Up = std::remove_cvref_t<std::invoke_result_t<Fn, const Tp&>>;
    static_assert(is_optional_v<Up>, "the function passed to std::optional<T>::and_then must return a std::optional");
    if (has_value()) return std::invoke(std::forward<Fn>(f), **this);
    return Up();
  }

  template <typename Fn>
  constexpr auto and_then(Fn&& f) && {
    using Up = std::remove_cvref_t<std::invoke_result_t<Fn, Tp>>;
    static_assert(is_optional_v<std::remove_cvref_t<Up>>,
                  "the function passed to std::optional<T>::and_then must return a std::optional");
    if (has_value()) return std::invoke(std::forward<Fn>(f), std::move(**this));
    return Up();
  }

  template <typename Fn>
  constexpr auto and_then(Fn&& f) const&& {
    using Up = std::remove_cvref_t<std::invoke_result_t<Fn, const Tp>>;
    static_assert(is_optional_v<std::remove_cvref_t<Up>>,
                  "the function passed to std::optional<T>::and_then must return a std::optional");
    if (has_value()) return std::invoke(std::forward<Fn>(f), std::move(**this));
    return Up();
  }

  template <typename Fn>
  constexpr auto map(Fn&& f) & {
    using Up = std::remove_cv_t<std::invoke_result_t<Fn, Tp&>>;
    if (has_value()) return Optional<Up>(OptionalFunc<Fn>{f}, **this);
    return Optional<Up>();
  }

  template <typename Fn>
  constexpr auto map(Fn&& f) const& {
    using Up = std::remove_cv_t<std::invoke_result_t<Fn, const Tp&>>;
    if (has_value()) return Optional<Up>(OptionalFunc<Fn>{f}, **this);
    return Optional<Up>();
  }

  template <typename Fn>
  constexpr auto map(Fn&& f) && {
    using Up = std::remove_cv_t<std::invoke_result_t<Fn, Tp>>;
    if (has_value()) return Optional<Up>(OptionalFunc<Fn>{f}, std::move(**this));
    return Optional<Up>();
  }

  template <typename Fn>
  constexpr auto map(Fn&& f) const&& {
    using Up = std::remove_cv_t<std::invoke_result_t<Fn, const Tp>>;
    if (has_value()) return Optional<Up>(OptionalFunc<Fn>{f}, std::move(**this));
    return Optional<Up>();
  }

  template <typename Fn>
    requires std::invocable<Fn> && std::copy_constructible<Tp>
  constexpr auto or_else(Fn&& f) const& -> Optional {
    using Up = std::invoke_result_t<Fn>;
    static_assert(std::is_same_v<std::remove_cvref_t<Up>, Optional>,
                  "the function passed to std::optional<T>::or_else must return a std::optional<T>");

    if (has_value()) return *this;
    return std::forward<Fn>(f)();
  }

  template <typename Fn>
    requires std::invocable<Fn> && std::move_constructible<Tp>
  constexpr auto or_else(Fn&& f) && -> Optional {
    using Up = std::invoke_result_t<Fn>;
    static_assert(std::is_same_v<std::remove_cvref_t<Up>, Optional>,
                  "the function passed to std::optional<T>::or_else must return a std::optional<T>");

    if (has_value()) return std::move(*this);
    return std::forward<Fn>(f)();
  }

  constexpr void reset() noexcept { this->reset(); }

 private:
  template <typename Up>
  friend class optional;

  template <typename Fn, typename Value>
  explicit constexpr Optional(OptionalFunc<Fn> f, Value&& v) {
    this->m_payload.m_apply(f, std::forward<Value>(v));
  }
};

template <typename Tp>
using OptionalRelopT = std::enable_if_t<std::is_convertible_v<Tp, bool>, bool>;

template <typename Tp, typename Up>
using OptionalEqT = OptionalRelopT<decltype(std::declval<const Tp&>() == std::declval<const Up&>())>;

template <typename Tp, typename Up>
using OptionalNeT = OptionalRelopT<decltype(std::declval<const Tp&>() != std::declval<const Up&>())>;

template <typename Tp, typename Up>
using OptionalLtT = OptionalRelopT<decltype(std::declval<const Tp&>() < std::declval<const Up&>())>;

template <typename Tp, typename Up>
using OptionalGtT = OptionalRelopT<decltype(std::declval<const Tp&>() > std::declval<const Up&>())>;

template <typename Tp, typename Up>
using OptionalLeT = OptionalRelopT<decltype(std::declval<const Tp&>() <= std::declval<const Up&>())>;

template <typename Tp, typename Up>
using OptionalGeT = OptionalRelopT<decltype(std::declval<const Tp&>() >= std::declval<const Up&>())>;

// Comparisons between optional values.
template <typename Tp, typename Up>
constexpr auto operator==(const Optional<Tp>& lhs, const Optional<Up>& rhs) -> OptionalEqT<Tp, Up> {
  return static_cast<bool>(lhs) == static_cast<bool>(rhs) && (!lhs || *lhs == *rhs);
}

template <typename Tp, typename Up>
constexpr auto operator!=(const Optional<Tp>& lhs, const Optional<Up>& rhs) -> OptionalNeT<Tp, Up> {
  return static_cast<bool>(lhs) != static_cast<bool>(rhs) || (static_cast<bool>(lhs) && *lhs != *rhs);
}

template <typename Tp, typename Up>
constexpr auto operator<(const Optional<Tp>& lhs, const Optional<Up>& rhs) -> OptionalLtT<Tp, Up> {
  return static_cast<bool>(rhs) && (!lhs || *lhs < *rhs);
}

template <typename Tp, typename Up>
constexpr auto operator>(const Optional<Tp>& lhs, const Optional<Up>& rhs) -> OptionalGtT<Tp, Up> {
  return static_cast<bool>(lhs) && (!rhs || *lhs > *rhs);
}

template <typename Tp, typename Up>
constexpr auto operator<=(const Optional<Tp>& lhs, const Optional<Up>& rhs) -> OptionalLeT<Tp, Up> {
  return !lhs || (static_cast<bool>(rhs) && *lhs <= *rhs);
}

template <typename Tp, typename Up>
constexpr auto operator>=(const Optional<Tp>& lhs, const Optional<Up>& rhs) -> OptionalGeT<Tp, Up> {
  return !rhs || (static_cast<bool>(lhs) && *lhs >= *rhs);
}

template <typename Tp, std::three_way_comparable_with<Tp> Up>
constexpr auto operator<=>(const Optional<Tp>& x, const Optional<Up>& y) -> std::compare_three_way_result_t<Tp, Up> {
  return x && y ? *x <=> *y : bool(x) <=> bool(y);
}

// Comparisons with nullopt.
template <typename Tp>
constexpr auto operator==(const Optional<Tp>& lhs, std::nullopt_t /*unused*/) noexcept -> bool {
  return !lhs;
}

template <typename Tp>
constexpr auto operator<=>(const Optional<Tp>& x, std::nullopt_t /*unused*/) noexcept -> std::strong_ordering {
  return bool(x) <=> false;
}

// Comparisons with value type.
template <typename Tp, typename Up>
constexpr auto operator==(const Optional<Tp>& lhs, const Up& rhs) -> OptionalEqT<Tp, Up> {
  return lhs && *lhs == rhs;
}

template <typename Tp, typename Up>
constexpr auto operator==(const Up& lhs, const Optional<Tp>& rhs) -> OptionalEqT<Up, Tp> {
  return rhs && lhs == *rhs;
}

template <typename Tp, typename Up>
constexpr auto operator!=(const Optional<Tp>& lhs, const Up& rhs) -> OptionalNeT<Tp, Up> {
  return !lhs || *lhs != rhs;
}

template <typename Tp, typename Up>
constexpr auto operator!=(const Up& lhs, const Optional<Tp>& rhs) -> OptionalNeT<Up, Tp> {
  return !rhs || lhs != *rhs;
}

template <typename Tp, typename Up>
constexpr auto operator<(const Optional<Tp>& lhs, const Up& rhs) -> OptionalLtT<Tp, Up> {
  return !lhs || *lhs < rhs;
}

template <typename Tp, typename Up>
constexpr auto operator<(const Up& lhs, const Optional<Tp>& rhs) -> OptionalLtT<Up, Tp> {
  return rhs && lhs < *rhs;
}

template <typename Tp, typename Up>
constexpr auto operator>(const Optional<Tp>& lhs, const Up& rhs) -> OptionalGtT<Tp, Up> {
  return lhs && *lhs > rhs;
}

template <typename Tp, typename Up>
constexpr auto operator>(const Up& lhs, const Optional<Tp>& rhs) -> OptionalGtT<Up, Tp> {
  return !rhs || lhs > *rhs;
}

template <typename Tp, typename Up>
constexpr auto operator<=(const Optional<Tp>& lhs, const Up& rhs) -> OptionalLeT<Tp, Up> {
  return !lhs || *lhs <= rhs;
}

template <typename Tp, typename Up>
constexpr auto operator<=(const Up& lhs, const Optional<Tp>& rhs) -> OptionalLeT<Up, Tp> {
  return rhs && lhs <= *rhs;
}

template <typename Tp, typename Up>
constexpr auto operator>=(const Optional<Tp>& lhs, const Up& rhs) -> OptionalGeT<Tp, Up> {
  return lhs && *lhs >= rhs;
}

template <typename Tp, typename Up>
constexpr auto operator>=(const Up& lhs, const Optional<Tp>& rhs) -> OptionalGeT<Up, Tp> {
  return !rhs || lhs >= *rhs;
}

template <typename Tp, typename Up>
  requires(!is_optional_v<Up>) && std::three_way_comparable_with<Up, Tp>
constexpr auto operator<=>(const Optional<Tp>& x, const Up& v) -> std::compare_three_way_result_t<Tp, Up> {
  return bool(x) ? *x <=> v : std::strong_ordering::less;
}

// Swap and creation functions.

// _GLIBCXX_RESOLVE_LIB_DEFECTS
// 2748. swappable traits for optionals
template <typename Tp>
  requires(std::is_move_constructible_v<Tp> && std::is_swappable_v<Tp>)
constexpr void swap(Optional<Tp>& lhs, Optional<Tp>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
  lhs.swap(rhs);
}

template <typename Tp>
  requires(!(std::is_move_constructible_v<Tp> && std::is_swappable_v<Tp>))
void swap(Optional<Tp>&, Optional<Tp>&) = delete;

template <typename Tp>
  requires(std::is_constructible_v<std::decay_t<Tp>, Tp>)
constexpr auto make_optional(Tp&& t) noexcept(std::is_nothrow_constructible_v<Optional<std::decay_t<Tp>>, Tp>)
    -> Optional<std::decay_t<Tp>> {
  return Optional<std::decay_t<Tp>>{std::forward<Tp>(t)};
}

template <typename Tp, typename... Args>
  requires(std::is_constructible_v<Tp, Args...>)
constexpr auto make_optional(Args&&... args) noexcept(std::is_nothrow_constructible_v<Tp, Args...>) -> Optional<Tp> {
  return Optional<Tp>{std::in_place, std::forward<Args>(args)...};
}

template <typename Tp, typename Up, typename... Args>
  requires(std::is_constructible_v<Tp, std::initializer_list<Up>&, Args...>)
constexpr auto make_optional(std::initializer_list<Up> il, Args&&... args) noexcept(
    std::is_nothrow_constructible_v<Tp, std::initializer_list<Up>&, Args...>) -> Optional<Tp> {
  return Optional<Tp>{std::in_place, il, std::forward<Args>(args)...};
}

// Hash.

template <typename Tp, typename Up = std::remove_const_t<Tp>, bool = std::__poison_hash<Up>::__enable_hash_call>
struct OptionalHashCallBase {
  auto operator()(const Optional<Tp>& t) const noexcept(noexcept(std::hash<Up>{}(*t))) -> size_t {
    // We pick an arbitrary hash for disengaged optionals which hopefully
    // usual values of _Tp won't typically hash to.
    constexpr auto magic_disengaged_hash = static_cast<size_t>(-3333);
    return t ? std::hash<Up>{}(*t) : magic_disengaged_hash;
  }
};

template <typename Tp, typename Up>
struct OptionalHashCallBase<Tp, Up, false> {};

/// @}

template <typename Tp>
Optional(Tp) -> Optional<Tp>;

}  // namespace nostd

namespace std {

template <typename Tp>
struct hash<nostd::Optional<Tp>> : private __poison_hash<remove_const_t<Tp>>, public nostd::OptionalHashCallBase<Tp> {};

template <typename Tp>
struct __is_fast_hash<std::hash<nostd::Optional<Tp>>> : std::__is_fast_hash<std::hash<Tp>> {};

}  // namespace std

#endif  // _GLIBCXX_OPTIONAL
