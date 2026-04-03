#pragma once

#include <cstddef>
#include <tuple>
#include <utility>

namespace asyncli {

struct Ignore {
  template <typename... Args>
  void operator()(Args&&... /*unused*/) const {}
};

inline constexpr Ignore ignore{};

template <typename T, typename... CapturedArgs>
struct Construct {
  constexpr explicit Construct(CapturedArgs&&... args) : args_{std::forward<CapturedArgs>(args)...} {}

  template <typename... CallArgs>
  constexpr auto operator()(CallArgs&&... call_args) const -> T {
    auto args = std::tuple_cat(args_, std::forward_as_tuple(std::forward<CallArgs>(call_args)...));
    return apply(std::move(args), std::make_index_sequence<std::tuple_size_v<decltype(args)>>{});
  }

 private:
  template <typename Tuple, std::size_t... Is>
  static constexpr auto apply(Tuple&& args, std::index_sequence<Is...> /*unused*/) -> T {
    return T{std::get<Is>(std::forward<Tuple>(args))...};
  }

  [[no_unique_address]] std::tuple<CapturedArgs...> args_;
};

template <typename T, typename... Args>
[[nodiscard]] constexpr auto construct(Args&&... args) -> Construct<T, Args...> {
  return Construct<T, Args...>{std::forward<Args>(args)...};
}

}  // namespace asyncli
