#pragma once

#include <system_error>

#include "result.hpp"

namespace asyncli {

template <auto Fn, typename... Args>
auto syscall(Args&&... args) -> Result<decltype(Fn(std::forward<Args>(args)...))> {
  auto result = Fn(std::forward<Args>(args)...);
  if (result < 0) {
    auto cond = std::system_category().default_error_condition(errno);
    return Error{cond};
  }
  return result;
}

}  // namespace asyncli
