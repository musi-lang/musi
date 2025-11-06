#pragma once

#include "types.hpp"

namespace musi {

  auto load_bytecode(std::string_view path) -> Expected<std::vector<uint8_t>>;

}  // namespace musi
