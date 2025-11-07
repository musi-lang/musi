#include "module.hpp"

#include <format>

#include "header.hpp"
#include "loader.hpp"
#include "spdlog/spdlog.h"

namespace musi {

  namespace {
    constexpr size_t UINT32_SIZE = 4;
    constexpr uint32_t BYTE_SHIFT_8 = 8U;
    constexpr uint32_t BYTE_SHIFT_16 = 16U;
    constexpr uint32_t BYTE_SHIFT_24 = 24U;

    auto read_u32(const std::span<const uint8_t>& data, const size_t offset)
        -> uint32_t {
      return static_cast<uint32_t>(data[offset])
             | (static_cast<uint32_t>(data[offset + 1U]) << BYTE_SHIFT_8)
             | (static_cast<uint32_t>(data[offset + 2U]) << BYTE_SHIFT_16)
             | (static_cast<uint32_t>(data[offset + 3U]) << BYTE_SHIFT_24);
    }

    auto read_u16(const std::span<const uint8_t>& data, const size_t offset)
        -> uint16_t {
      // yes, I effing hate this cast hell
      return static_cast<uint16_t>(data[offset])
             | static_cast<uint16_t>(
                 static_cast<uint16_t>(data[offset + 1U]) << BYTE_SHIFT_8);
    }

    auto parse_exports(const std::span<const uint8_t>& data, size_t offset)
        -> Expected<std::unordered_map<std::string, uint32_t>> {
      if (offset + UINT32_SIZE > data.size()) {
        return std::unexpected("export table offset out of bounds");
      }
      const auto count = read_u32(data, offset);
      offset += UINT32_SIZE;

      std::unordered_map<std::string, uint32_t> exports;
      for (auto i = 0U; i < count; ++i) {
        if (offset + 2 > data.size()) {
          return std::unexpected("export entry out of bounds");
        }
        const auto name_len = read_u16(data, offset);
        offset += 2;

        if (offset + name_len + 2 > data.size()) {
          return std::unexpected("export name out of bounds");
        }
        std::string name(name_len, '\0');
        for (auto j = 0U; j < name_len; ++j) {
          name[j] = static_cast<char>(data[offset + j]);
        }
        offset += name_len;

        const auto proc_id = read_u16(data, offset);
        offset += 2;

        exports[name] = proc_id;
      }
      return exports;
    }
  }  // namespace

  auto ModuleRegistry::load_module(std::string_view path) -> Expected<Module*> {
    const std::string path_str {path};
    if (m_modules.contains(path_str)) {
      return &m_modules[path_str];
    }

    spdlog::info("loading module: {}", path_str);

    const auto bc_res = load_bytecode(path);
    if (!bc_res) {
      return std::unexpected(
          std::format("failed to load {}: {}", path_str, bc_res.error()));
    }
    const auto& bc = bc_res.value();

    const auto hdr_res = parse_header(bc);
    if (!hdr_res) {
      return std::unexpected(
          std::format("failed to parse header: {}", hdr_res.error()));
    }
    const auto& hdr = hdr_res.value();

    const auto pool_res = parse_const_pool(bc, hdr.metadata_offset);
    if (!pool_res) {
      return std::unexpected(
          std::format("failed to parse const pool: {}", pool_res.error()));
    }

    auto const_pool_end = static_cast<uint32_t>(hdr.metadata_offset);
    if (const_pool_end + UINT32_SIZE <= bc.size()) {
      const auto count = read_u32(bc, const_pool_end);
      const_pool_end += UINT32_SIZE;
      for (auto i = 0U; i < count; ++i) {
        if (const_pool_end >= bc.size()) {
          break;
        }
        const auto tag = bc[const_pool_end++];
        if (tag == 0x05) {
          if (const_pool_end + UINT32_SIZE > bc.size()) {
            break;
          }
          const auto len = read_u32(bc, const_pool_end);
          const_pool_end += UINT32_SIZE + len;
        }
      }
    }

    const auto proc_res = parse_proc_table(bc, const_pool_end);
    if (!proc_res) {
      return std::unexpected(
          std::format("failed to parse proc table: {}", proc_res.error()));
    }

    auto export_offset = const_pool_end;
    const auto proc_count = read_u32(bc, export_offset);
    export_offset += UINT32_SIZE + static_cast<size_t>(proc_count) * 13U;

    const auto exports_res = parse_exports(bc, export_offset);
    if (!exports_res) {
      return std::unexpected(
          std::format("failed to parse exports: {}", exports_res.error()));
    }

    Module module {
        .path = path_str,
        .bytecode = bc_res.value(),
        .header = hdr_res.value(),
        .const_pool = pool_res.value(),
        .proc_table = proc_res.value(),
        .exports = exports_res.value()};

    m_modules[path_str] = std::move(module);
    return &m_modules[path_str];
  }

  auto ModuleRegistry::resolve_symbol(
      std::string_view module_path,
      std::string_view symbol) -> Expected<uint32_t> {
    const std::string path_str {module_path};
    if (!m_modules.contains(path_str)) {
      return std::unexpected(std::format("module not loaded: {}", module_path));
    }

    const auto& module = m_modules[path_str];
    const std::string symbol_str {symbol};
    if (!module.exports.contains(symbol_str)) {
      return std::unexpected(std::format(
          "symbol '{}' not found in module '{}'",
          symbol,
          module_path));
    }

    return module.exports.at(symbol_str);
  }

  auto ModuleRegistry::get_module(std::string_view path) -> Module* {
    const std::string path_str {path};
    if (!m_modules.contains(path_str)) {
      return nullptr;
    }
    return &m_modules[path_str];
  }

}  // namespace musi
