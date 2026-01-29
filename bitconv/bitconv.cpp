#include "bitconv.hpp"

#include <print>

struct Example {
  int a;
  char d;
  std::array<std::byte, 3> padding{};
  float b;
};

static constexpr auto operator==(const Example& lhs, const Example& rhs) -> bool {
  return lhs.a == rhs.a && lhs.b == rhs.b && lhs.d == rhs.d;
}

auto main() -> int {
  constexpr Example ex{.a = 42, .d = 'x', .b = 3.14F};
  auto bytes = bitconv::as_bytes(ex);
  auto byte_span = std::span{bytes};
  auto ex_copy = bitconv::from_bytes<Example>(byte_span);

  bool are_equal = ex == ex_copy;
  std::println("Are equal: {}", are_equal);

  return 0;
}
