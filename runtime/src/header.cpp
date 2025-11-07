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

    hdr.version = read_le<uint32_t>(data, 4);
    hdr.bc_offset = read_le<uint32_t>(data, 8);
    hdr.bc_size = read_le<uint32_t>(data, 12);
    hdr.export_offset = read_le<uint32_t>(data, 16);
    hdr.export_count = read_le<uint32_t>(data, 20);
    hdr.link_offset = read_le<uint32_t>(data, 24);
    hdr.link_count = read_le<uint32_t>(data, 28);

    return hdr;
  }

  auto parse_export_table(std::span<const uint8_t> data, size_t offset)
      -> Expected<ExportEntryList> {
    if (offset + 4 > data.size()) {
      return std::unexpected("export table offset out of bounds");
    }

    const auto count = read_le<uint32_t>(data, offset);
    offset += 4;

    ExportEntryList exports;
    exports.reserve(count);

    for (auto i = 0U; i < count; ++i) {
      if (offset + 4 > data.size()) {
        return std::unexpected("export entry name length out of bounds");
      }

      const auto name_len = read_le<uint32_t>(data, offset);
      offset += 4;

      if (offset + name_len + 4 > data.size()) {
        return std::unexpected("export entry data out of bounds");
      }

      std::string name(name_len, '\0');
      for (auto j = 0U; j < name_len; ++j) {
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
      -> Expected<LinkEntryList> {
    if (offset + 4 > data.size()) {
      return std::unexpected("link table offset out of bounds");
    }

    const auto count = read_le<uint32_t>(data, offset);
    offset += 4;

    LinkEntryList links;
    links.reserve(count);

    for (auto i = 0U; i < count; ++i) {
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
      for (auto j = 0U; j < key_len; ++j) {
        link_key[j] = static_cast<char>(data[offset + j]);
      }
      offset += key_len;

      links.push_back({proc_id, std::move(link_key)});
    }

    return links;
  }

  auto parse_const_pool(std::span<const uint8_t> data, size_t offset)
      -> Expected<ConstPool> {
    if (offset + 4 > data.size()) {
      return std::unexpected("constant pool offset out of bounds");
    }

    const auto count = read_le<uint32_t>(data, offset);
    offset += 4;

    ConstPool pool;
    pool.reserve(count);

    for (auto i = 0U; i < count; ++i) {
      if (offset >= data.size()) {
        return std::unexpected("constant pool entry out of bounds");
      }

      const auto tag = data[offset++];
      if (tag == 0x05) {
        if (offset + 4 > data.size()) {
          return std::unexpected("constant string length out of bounds");
        }

        const auto len = read_le<uint32_t>(data, offset);
        offset += 4;

        if (offset + len > data.size()) {
          return std::unexpected("constant string data out of bounds");
        }

        std::string str(len, '\0');
        for (auto j = 0U; j < len; ++j) {
          str[j] = static_cast<char>(data[offset + j]);
        }
        offset += len;

        pool.push_back(std::move(str));
      } else {
        return std::unexpected(
            std::format("unsupported constant tag '0x{:02X}'", tag));
      }
    }

    return pool;
  }

  auto parse_proc_table(std::span<const uint8_t> data, size_t offset)
      -> Expected<ProcTable> {
    if (offset + 4 > data.size()) {
      return std::unexpected("procedure table offset out of bounds");
    }

    const auto count = read_le<uint32_t>(data, offset);
    offset += 4;

    ProcTable procs;
    procs.reserve(count);

    for (auto i = 0U; i < count; ++i) {
      if (offset + 9 > data.size()) {
        return std::unexpected("procedure descriptor out of bounds");
      }

      ProcDesc desc {};
      desc.bytecode_offset = read_le<uint32_t>(data, offset);
      offset += 4;
      desc.bytecode_length = read_le<uint32_t>(data, offset);
      offset += 4;
      desc.is_extern = data[offset++] != 0;

      procs.push_back(desc);
    }

    return procs;
  }

}  // namespace musi
