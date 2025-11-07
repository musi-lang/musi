#pragma once

#include <expected>
#include <span>
#include <string>
#include <vector>

#include "value.hpp"

namespace musi {

  using Bytecode = std::span<const uint8_t>;
  using ConstPool = std::vector<std::string>;

  using ValueList = std::vector<Value>;

  using ExportEntryList = std::vector<struct ExportEntry>;
  using LinkEntryList = std::vector<struct LinkEntry>;
  using ProcTable = std::vector<struct ProcDesc>;

  template<typename T>
  using Expected = std::expected<T, std::string>;

}  // namespace musi
