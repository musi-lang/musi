#pragma once

#include <cstdint>

namespace musi::intrinsics::lang {

  class Nat {
public:
    static auto from_value(uint64_t value) -> uint64_t;
  };

}  // namespace musi::intrinsics::lang
