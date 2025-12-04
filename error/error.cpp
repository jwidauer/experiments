#include <expected>
#include <format>
#include <print>
#include <string>
#include <vector>

struct Error {
  template <class... Args>
  [[nodiscard]] static auto create(std::format_string<Args...> fmt, Args&&... args) -> std::unexpected<Error> {
    return std::unexpected<Error>{std::in_place, std::format(fmt, std::forward<Args>(args)...)};
  }

  std::string message;
};

template <typename T>
struct std::formatter<std::vector<T>> : std::formatter<std::string> {
  template <typename FmtContext>
  auto format(const std::vector<T>& vec, FmtContext& ctx) const -> FmtContext::iterator {
    auto iter = ctx.out();
    iter = std::format_to(iter, "{{");
    for (std::string_view sep{}; auto elem : vec) {
      iter = std::format_to(iter, "{}{}", sep, elem);
      sep = ", ";
    }
    iter = std::format_to(iter, "}}");
    return iter;
  }
};

auto main(int /*argc*/, char** /*argv*/) -> int {
  auto str = std::string{"name"};
  auto err = Error::create("An error occurred with {}", str);

  std::vector<std::string> vec{"one", "two", "three"};

  std::print("{}", vec);

  return 0;
}
