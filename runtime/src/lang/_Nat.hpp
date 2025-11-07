#pragma once

#include <cstdint>

namespace musi {

  class Nat {
public:
    static auto from_value(uint64_t value) -> uint64_t;
  };

}  // namespace musi
