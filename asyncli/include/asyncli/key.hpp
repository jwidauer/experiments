#pragma once

#include <cstddef>
#include <functional>

#include "events.hpp"
#include "file_descriptor.hpp"

namespace asyncli {

struct Key {
  FileDescriptor fd;
  Event event;

  [[nodiscard]] friend auto operator==(const Key& lhs, const Key& rhs) -> bool = default;
};

}  // namespace asyncli

template <>
struct std::hash<asyncli::Key> {
  constexpr auto operator()(const asyncli::Key& key) const -> std::size_t {
    const auto h1 = std::hash<asyncli::FileDescriptor>{}(key.fd);
    const auto h2 = std::hash<asyncli::Event>{}(key.event);
    return h1 ^ (h2 << 1);
  }
};
