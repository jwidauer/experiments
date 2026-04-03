#pragma once

#include "result.hpp"

#define ASYNCLI_RETURN_IF_ERROR(expr) \
  if (auto res = (expr); !res) {      \
    return Error{res.error()};        \
  }
