#include <spdlog/spdlog.h>

#include <nlohmann/json.hpp>

void init()
{
  // Set log pattern to be json format
  spdlog::set_pattern(
      R"({"time":"%Y-%m-%d %H:%M:%S.%e","process":%P,"thread_id":%t,"level":"%l","message":"%v})");
}

template <>
struct fmt::formatter<nlohmann::json> : fmt::formatter<std::string> {
  auto format(const nlohmann::json& json, format_context& ctx) -> decltype(ctx.out())
  {
    // On empty json, do nothing, but close the message
    if (json.empty() || json.is_null()) {
      return format_to(ctx.out(), "\"");
    }

    return format_to(ctx.out(), R"(","data":{})", json.dump());
  }
};

int main()
{
  using J = nlohmann::json;
  init();

  spdlog::info("Hello, {}!\"", "World");
  spdlog::info("Hello, World!{}", J{});
  spdlog::info("Hello, World!{}", J{1});
  spdlog::info("Hello, World!{}", J{"string"});
  spdlog::info("Hello, World!{}", J{true});
  spdlog::info("Hello, World!{}", J{1, 2, 3});
  spdlog::info("Hello, World!{}", J{{"key", "value"}});
  spdlog::info("Hello, World!{}", J{{"key", "value"}, {"key2", "value2"}});

  return 0;
}
