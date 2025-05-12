#include <print>

auto main() -> int {
  static char __attribute__((section(".defmt.data"))) world[] = "world";
  std::println("Hello, {}!", world);

  return 0;
}
