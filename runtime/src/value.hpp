#pragma once

#include <cstdint>
#include <variant>

namespace musi {

  struct Unit {};
  struct Object;

  using Value = std::variant<int32_t, bool, Unit, Object*>;

}  // namespace musi
