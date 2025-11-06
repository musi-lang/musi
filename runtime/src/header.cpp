#include "header.hpp"

#include <format>

#include "types.hpp"

namespace musi {

  namespace {
    template<typename T>
    auto read_le(std::span<const uint8_t> data, const size_t offset) -> T {
      T value {};
      const auto bytes = data.subspan(offset, sizeof(T));
      for (auto i = 0U; i < sizeof(T); ++i) {
        // what in casting f*ck?
        value |= static_cast<T>(static_cast<uint8_t>(bytes[i]) << (i * 8));
      }
      return value;
    }
  }  // namespace

  auto parse_header(std::span<const uint8_t> data) -> Expected<Header> {
    if (data.size() < Header::SIZE) {
      return std::unexpected(std::format(
          "file is {} bytes, need at least {}",
          data.size(),
          Header::SIZE));
    }

    Header hdr {};
    hdr.magic[0] = static_cast<char>(data[0]);
    hdr.magic[1] = static_cast<char>(data[1]);
    hdr.magic[2] = static_cast<char>(data[2]);
    hdr.magic[3] = static_cast<char>(data[3]);

    if (std::string_view(hdr.magic.data(), 4) != Header::MAGIC) {
      return std::unexpected(std::format(
          "expected magic 'MUSI', got '{}{}{}{}'",
          hdr.magic[0],
          hdr.magic[1],
          hdr.magic[2],
          hdr.magic[3]));
    }

    hdr.major_version = read_le<uint16_t>(data, 4);
    hdr.minor_version = read_le<uint16_t>(data, 6);
    hdr.flags = read_le<uint32_t>(data, 8);
    hdr.const_pool_offset = read_le<uint32_t>(data, 12);
    hdr.type_table_offset = read_le<uint32_t>(data, 16);
    hdr.proc_table_offset = read_le<uint32_t>(data, 20);
    hdr.debug_info_offset = read_le<uint32_t>(data, 24);
    hdr.entry_proc_id = read_le<uint32_t>(data, 28);

    return hdr;
  }

  auto parse_export_table(std::span<const uint8_t> data, size_t offset)
      -> Expected<std::vector<ExportEntry>> {
    if (offset + 4 > data.size()) {
      return std::unexpected("export table offset out of bounds");
    }

    const auto count = read_le<uint32_t>(data, offset);
    offset += 4;

    std::vector<ExportEntry> exports;
    exports.reserve(count);

    for (uint32_t i = 0; i < count; ++i) {
      if (offset + 4 > data.size()) {
        return std::unexpected("export entry name length out of bounds");
      }

      const auto name_len = read_le<uint32_t>(data, offset);
      offset += 4;

      if (offset + name_len + 4 > data.size()) {
        return std::unexpected("export entry data out of bounds");
      }

      std::string name(name_len, '\0');
      for (uint32_t j = 0; j < name_len; ++j) {
        name[j] = static_cast<char>(data[offset + j]);
      }
      offset += name_len;

      const auto proc_id = read_le<uint32_t>(data, offset);
      offset += 4;

      exports.push_back({std::move(name), proc_id});
    }

    return exports;
  }

  auto parse_link_table(std::span<const uint8_t> data, size_t offset)
      -> Expected<std::vector<LinkEntry>> {
    if (offset + 4 > data.size()) {
      return std::unexpected("link table offset out of bounds");
    }

    const auto count = read_le<uint32_t>(data, offset);
    offset += 4;

    std::vector<LinkEntry> links;
    links.reserve(count);

    for (uint32_t i = 0; i < count; ++i) {
      if (offset + 8 > data.size()) {
        return std::unexpected("link entry header out of bounds");
      }

      const auto proc_id = read_le<uint32_t>(data, offset);
      offset += 4;

      const auto key_len = read_le<uint32_t>(data, offset);
      offset += 4;

      if (offset + key_len > data.size()) {
        return std::unexpected("link entry key out of bounds");
      }

      std::string link_key(key_len, '\0');
      for (uint32_t j = 0; j < key_len; ++j) {
        link_key[j] = static_cast<char>(data[offset + j]);
      }
      offset += key_len;

      links.push_back({proc_id, std::move(link_key)});
    }

    return links;
  }

}  // namespace musi
