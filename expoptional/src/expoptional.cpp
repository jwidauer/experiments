#include <expected>
#include <functional>
#include <iostream>
#include <optional>

template <typename T>
class Optional;

template <typename T>
inline constexpr bool is_optional_v = false;
template <typename T>
inline constexpr bool is_optional_v<Optional<T>> = true;

template <typename T>
class Optional : private std::optional<T> {
 public:
  using value_type = T;

  // constructors and assignment operators
  using std::optional<T>::optional;
  using std::optional<T>::operator=;

  // Observers
  using std::optional<T>::operator->;
  using std::optional<T>::operator*;
  using std::optional<T>::operator bool;
  using std::optional<T>::has_value;
  using std::optional<T>::value;
  using std::optional<T>::value_or;

  // Monadic operations
  template <typename _Fn>
  constexpr auto transform(_Fn&& __f) & {
    using _Up = std::remove_cv_t<std::invoke_result_t<_Fn, T&>>;
    if (has_value())
      return Optional<_Up>(_Optional_func<_Fn>{__f}, **this);
    else
      return Optional<_Up>();
  }

  template <typename _Fn>
  constexpr auto transform(_Fn&& __f) const& {
    using _Up = std::remove_cv_t<std::invoke_result_t<_Fn, const T&>>;
    if (has_value())
      return Optional<_Up>(_Optional_func<_Fn>{__f}, **this);
    else
      return Optional<_Up>();
  }

  template <typename _Fn>
  constexpr auto transform(_Fn&& __f) && {
    using _Up = std::remove_cv_t<std::invoke_result_t<_Fn, T>>;
    if (has_value())
      return Optional<_Up>(_Optional_func<_Fn>{__f}, std::move(**this));
    else
      return Optional<_Up>();
  }

  template <typename _Fn>
  constexpr auto transform(_Fn&& __f) const&& {
    using _Up = std::remove_cv_t<std::invoke_result_t<_Fn, const T>>;
    if (has_value())
      return Optional<_Up>(_Optional_func<_Fn>{__f}, std::move(**this));
    else
      return Optional<_Up>();
  }

  template <typename Fn>
  constexpr auto and_then(Fn&& f) & {
    using U = std::remove_cvref_t<std::invoke_result_t<Fn, T&>>;
    static_assert(is_optional_v<U>,
                  "the function passed to Optional<T>::and_then "
                  "must return a Optional");
    if (has_value())
      return std::invoke(std::forward<Fn>(f), **this);
    else
      return U();
  }

  template <typename Fn>
  constexpr auto and_then(Fn&& f) const& {
    using U = std::remove_cvref_t<std::invoke_result_t<Fn, const T&>>;
    static_assert(is_optional_v<U>,
                  "the function passed to Optional<T>::and_then "
                  "must return a Optional");
    if (has_value())
      return std::invoke(std::forward<Fn>(f), **this);
    else
      return U();
  }

  template <typename Fn>
  constexpr auto and_then(Fn&& f) && {
    using U = std::remove_cvref_t<std::invoke_result_t<Fn, T>>;
    static_assert(is_optional_v<U>,
                  "the function passed to Optional<T>::and_then "
                  "must return a Optional");
    if (has_value())
      return std::invoke(std::forward<Fn>(f), std::move(**this));
    else
      return U();
  }

  template <typename F>
  constexpr auto and_then(F&& f) const&& {
    using U = std::remove_cvref_t<std::invoke_result_t<F, const T>>;
    static_assert(is_optional_v<U>,
                  "the function passed to Optional<T>::and_then "
                  "must return a Optional");
    if (has_value())
      return std::invoke(std::forward<F>(f), std::move(**this));
    else
      return U();
  }

  using std::optional<T>::or_else;

  // Modifiers
  using std::optional<T>::swap;
  using std::optional<T>::reset;
  using std::optional<T>::emplace;
};

int main(int /*argc*/, char** /*argv*/) {
  Optional<int> opt;

  opt.emplace(42);

  auto opt2 = opt.transform([](int x) { return x * 2; }).and_then([](int x) {
    return Optional<int>(x + 1);
  });

  std::cout << *opt2 << std::endl;

  return 0;
}
