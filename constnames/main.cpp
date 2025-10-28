#include <print>
#include <span>

#include "consthash.hpp"
#include "hash_bytes.hpp"

template <std::size_t N>
[[nodiscard]] constexpr auto to_byte_array(const char (&str)[N]) -> std::array<std::byte, N - 1> {
  std::array<std::byte, N - 1> result{};
  for (std::size_t i = 0; i < N - 1; ++i) {
    result[i] = std::byte{static_cast<unsigned char>(str[i])};
  }
  return result;
}

auto main(int /*argc*/, char** /*argv*/) -> int {
  constexpr auto data = to_byte_array("Hello, World!");
  constexpr auto hash = consthash::hash(data);

  const auto runtime_hash = hash_bytes(data.data(), static_cast<uint32_t>(data.size()), 0);

  std::println("Compile-time hash: {:x}", hash);
  std::println("Runtime hash:      {:x}", runtime_hash);
  std::println();
  assert(hash == runtime_hash);

  return 0;
}
