#include <format>
#include <print>

namespace {

template <typename... Args>
void deprint(std::format_string<Args...> fmt, Args&&... args) {
  auto str = fmt.get();
  auto fmt_args = std::make_format_args(args...);
}

}  // namespace

auto main() -> int {
  static std::array __attribute__((section(".defmt.data"))) world = std::to_array("world");
  deprint("Hello, {}!", world.data());

  return 0;
}
