#include <expected>
#include <iostream>

#include "optional.hpp"

auto main(int /*argc*/, char** /*argv*/) -> int {
  tl::optional<int> opt;

  opt.emplace(42);

  auto opt2 = opt.transform([](int x) { return x * 2; }).and_then([](int x) { return tl::optional<int>(x + 1); });

  std::cout << *opt2 << '\n';

  return 0;
}
