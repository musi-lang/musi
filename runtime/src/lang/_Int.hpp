#pragma once

#include <cstdint>

namespace musi::intrinsics::lang {

  class Int {
public:
    static auto from_value(int64_t value) -> int64_t;
  };

}  // namespace musi::intrinsics::lang
