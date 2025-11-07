#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include "header.hpp"
#include "types.hpp"

namespace musi {

  struct Module {
    std::string path;
    std::vector<uint8_t> bytecode;
    Header header;
    ConstPool const_pool;
    ProcTable proc_table;
    std::unordered_map<std::string, uint32_t> exports;
  };

  class ModuleRegistry {
    std::unordered_map<std::string, Module> m_modules;

public:
    auto load_module(std::string_view path) -> Expected<Module*>;
    auto resolve_symbol(std::string_view module_path, std::string_view symbol)
        -> Expected<uint32_t>;
    auto get_module(std::string_view path) -> Module*;
  };

}  // namespace musi
