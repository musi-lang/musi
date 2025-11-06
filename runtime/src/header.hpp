#pragma once

#include "types.hpp"

namespace musi {

  struct Header {
    std::array<char, 4> magic;
    uint16_t major_version;
    uint16_t minor_version;
    uint32_t flags;
    uint32_t const_pool_offset;
    uint32_t type_table_offset;
    uint32_t proc_table_offset;
    uint32_t debug_info_offset;
    uint32_t entry_proc_id;

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

  auto parse_header(std::span<const uint8_t> data) -> Expected<Header>;
  auto parse_export_table(std::span<const uint8_t> data, size_t offset)
      -> Expected<std::vector<ExportEntry>>;
  auto parse_link_table(std::span<const uint8_t> data, size_t offset)
      -> Expected<std::vector<LinkEntry>>;

}  // namespace musi
