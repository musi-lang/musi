#pragma once

#include "types.hpp"

namespace musi {

  struct Header {
    std::array<char, 4> magic;
    uint32_t version;
    uint32_t bc_offset;
    uint32_t bc_size;
    uint32_t metadata_offset;
    uint32_t metadata_size;
    uint64_t reserved;

    static constexpr size_t SIZE = 32;
    static constexpr std::string_view MAGIC = "MUSI";
  };

  struct ExportEntry {
    std::string name;
    uint32_t proc_id;
  };

  struct LinkEntry {
    uint32_t proc_id;
    std::string link_key;
  };

  struct ProcDesc {
    uint32_t bytecode_offset;
    uint32_t bytecode_length;
    uint32_t param_count;
    bool is_extern;
  };

  auto parse_header(std::span<const uint8_t> data) -> Expected<Header>;
  auto parse_export_table(std::span<const uint8_t> data, size_t offset)
      -> Expected<std::vector<ExportEntry>>;
  auto parse_link_table(
      std::span<const uint8_t> data,
      size_t offset,
      uint32_t count) -> Expected<std::vector<LinkEntry>>;
  auto parse_const_pool(std::span<const uint8_t> data, size_t offset)
      -> Expected<ConstPool>;
  auto parse_proc_table(std::span<const uint8_t> data, size_t offset)
      -> Expected<ProcTable>;

}  // namespace musi
