#pragma once

#include <expected>
#include <system_error>

namespace asyncli {

template <typename T>
using Result = std::expected<T, std::error_condition>;

template <typename T>
using Error = std::unexpected<T>;

}  // namespace asyncli
