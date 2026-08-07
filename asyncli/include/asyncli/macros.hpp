#pragma once

#include "result.hpp"

#define ASYNCLI_TRY(expr)                           \
  ({                                                \
    auto res = (expr);                              \
    if (!res) return Error{std::move(res).error()}; \
    std::move(res).value();                         \
  })
