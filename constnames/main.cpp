#include <algorithm>
#include <print>
#include <string_view>

#include "consthash.hpp"
#include "hash_bytes.hpp"

namespace {

template <std::size_t N>
// NOLINTNEXTLINE (modernize-avoid-c-arrays)
[[nodiscard]] constexpr auto to_byte_array(const char (&str)[N]) -> std::array<std::byte, N - 1> {
  std::array<std::byte, N - 1> result{};
  std::ranges::transform(std::begin(str), std::end(str) - 1, result.begin(),
                         [](char c) -> auto { return std::bit_cast<std::byte>(c); });
  return result;
}

}  // namespace

namespace constname {

[[nodiscard]] consteval auto root_namespace(std::string_view sv) -> std::string_view {
  const auto paren_pos = sv.find('(');
  assert(paren_pos != std::string_view::npos);

  const auto space_pos = sv.rfind(' ', paren_pos);
  assert(space_pos != std::string_view::npos);

  const auto colons_pos = sv.find("::", space_pos);
  assert(colons_pos != std::string_view::npos);

  return sv.substr(space_pos + 1, colons_pos - space_pos - 1);
}

#define CONSTNAME() constname::root_namespace(__PRETTY_FUNCTION__)

}  // namespace constname

namespace printer {

// NOLINTBEGIN (misc-use-anonymous-namespace)
static void easy() { std::println("'{}'", CONSTNAME()); }

static auto harder [[nodiscard]] () -> int {
  std::println("'{}'", CONSTNAME());
  return 42;
}

template <typename T>
static void template_function() {
  std::println("'{}'", CONSTNAME());
}

namespace nested {
static void inside_nested() { std::println("'{}'", CONSTNAME()); }

}  // namespace nested

struct Example {
  Example() { std::println("'{}'", __PRETTY_FUNCTION__); }

  void method() { std::println("'{}'", CONSTNAME()); }
};

// NOLINTEND

}  // namespace printer

auto main(int /*argc*/, char** /*argv*/) -> int {
  using namespace std::literals;

  constexpr auto data = to_byte_array("Hello, World!");
  constexpr auto hash = consthash::hash("Hello, World!"sv);

  const auto runtime_hash = hash_bytes(data.data(), static_cast<uint32_t>(data.size()), 0xc70f6907);

  std::println("Compile-time hash: {:x}", hash);
  std::println("Runtime hash:      {:x}", runtime_hash);
  std::println();
  assert(hash == runtime_hash);

  printer::easy();
  std::ignore = printer::harder();
  printer::template_function<int>();
  printer::nested::inside_nested();
  auto example = printer::Example{};
  example.method();

  return 0;
}
