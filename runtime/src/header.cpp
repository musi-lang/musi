#include "header.hpp"

#include <format>

#include "types.hpp"

namespace musi {

  namespace {
    constexpr size_t OFFSET_VERSION = 4;
    constexpr size_t OFFSET_BC_OFFSET = 8;
    constexpr size_t OFFSET_BC_SIZE = 12;
    constexpr size_t OFFSET_EXPORT_OFFSET = 16;
    constexpr size_t OFFSET_EXPORT_COUNT = 20;
    constexpr size_t OFFSET_LINK_OFFSET = 24;
    constexpr size_t OFFSET_LINK_COUNT = 28;
    constexpr uint8_t TAG_TEXT = 0x05;
    constexpr size_t SIZE_U16 = 2;
    constexpr size_t SIZE_U32 = 4;
    constexpr size_t SIZE_PROC_DESC = 9;

    template<typename T>
    auto read_le(std::span<const uint8_t> data, const size_t offset) -> T {
      T value {};
      const auto bytes = data.subspan(offset, sizeof(T));
      for (auto i = 0U; i < sizeof(T); ++i) {
        value |= static_cast<T>(static_cast<uint8_t>(bytes[i]) << (i * 8));
      }
      return value;
    }

    auto check_bounds(
        std::span<const uint8_t> data,
        size_t offset,
        size_t size,
        std::string_view msg) -> Expected<void> {
      if (offset + size > data.size()) {
        return std::unexpected(std::string(msg));
      }
      return {};
    }

    auto read_string(std::span<const uint8_t> data, size_t offset, size_t len)
        -> std::string {
      std::string str(len, '\0');
      for (auto j = 0U; j < len; ++j) {
        str[j] = static_cast<char>(data[offset + j]);
      }
      return str;
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

    hdr.version = read_le<uint32_t>(data, OFFSET_VERSION);
    hdr.bc_offset = read_le<uint32_t>(data, OFFSET_BC_OFFSET);
    hdr.bc_size = read_le<uint32_t>(data, OFFSET_BC_SIZE);
    hdr.export_offset = read_le<uint32_t>(data, OFFSET_EXPORT_OFFSET);
    hdr.export_count = read_le<uint32_t>(data, OFFSET_EXPORT_COUNT);
    hdr.link_offset = read_le<uint32_t>(data, OFFSET_LINK_OFFSET);
    hdr.link_count = read_le<uint32_t>(data, OFFSET_LINK_COUNT);

    return hdr;
  }

  auto parse_export_table(std::span<const uint8_t> data, size_t offset)
      -> Expected<ExportEntryList> {
    if (const auto res = check_bounds(
            data,
            offset,
            SIZE_U32,
            "export table offset out of bounds");
        !res) {
      return std::unexpected(res.error());
    }
    const auto count = read_le<uint32_t>(data, offset);
    offset += SIZE_U32;

    ExportEntryList exports;
    exports.reserve(count);

    for (auto i = 0U; i < count; ++i) {
      if (const auto res = check_bounds(
              data,
              offset,
              SIZE_U32,
              "export entry name length out of bounds");
          !res) {
        return std::unexpected(res.error());
      }
      const auto name_len = read_le<uint32_t>(data, offset);
      offset += SIZE_U32;

      if (const auto res = check_bounds(
              data,
              offset,
              name_len + SIZE_U32,
              "export entry data out of bounds");
          !res) {
        return std::unexpected(res.error());
      }
      const auto name = read_string(data, offset, name_len);
      offset += name_len;

      const auto proc_id = read_le<uint32_t>(data, offset);
      offset += SIZE_U32;

      exports.push_back({name, proc_id});
    }

    return exports;
  }

  auto parse_link_table(
      std::span<const uint8_t> data,
      size_t offset,
      uint32_t count) -> Expected<LinkEntryList> {
    LinkEntryList links;
    links.reserve(count);

    for (auto i = 0U; i < count; ++i) {
      if (const auto res = check_bounds(
              data,
              offset,
              SIZE_U16 * 2,
              "link entry header out of bounds");
          !res) {
        return std::unexpected(res.error());
      }
      const auto proc_id = read_le<uint16_t>(data, offset);
      offset += SIZE_U16;

      const auto key_len = read_le<uint16_t>(data, offset);
      offset += SIZE_U16;

      if (const auto res = check_bounds(
              data,
              offset,
              key_len,
              "link entry key out of bounds");
          !res) {
        return std::unexpected(res.error());
      }
      const auto link_key = read_string(data, offset, key_len);
      offset += key_len;

      links.push_back({proc_id, link_key});
    }

    return links;
  }

  auto parse_const_pool(std::span<const uint8_t> data, size_t offset)
      -> Expected<ConstPool> {
    if (const auto res = check_bounds(
            data,
            offset,
            SIZE_U32,
            "constant pool offset out of bounds");
        !res) {
      return std::unexpected(res.error());
    }
    const auto count = read_le<uint32_t>(data, offset);
    offset += SIZE_U32;

    ConstPool pool;
    pool.reserve(count);

    for (auto i = 0U; i < count; ++i) {
      if (offset >= data.size()) {
        return std::unexpected("constant pool entry out of bounds");
      }

      const auto tag = data[offset++];
      if (tag == TAG_TEXT) {
        if (auto res = check_bounds(
                data,
                offset,
                SIZE_U32,
                "constant string length out of bounds");
            !res) {
          return std::unexpected(res.error());
        }
        const auto len = read_le<uint32_t>(data, offset);
        offset += SIZE_U32;

        if (const auto res = check_bounds(
                data,
                offset,
                len,
                "constant string data out of bounds");
            !res) {
          return std::unexpected(res.error());
        }
        const auto str = read_string(data, offset, len);
        offset += len;

        pool.push_back(str);
      } else {
        return std::unexpected(
            std::format("unsupported constant tag '0x{:02X}'", tag));
      }
    }

    return pool;
  }

  auto parse_proc_table(std::span<const uint8_t> data, size_t offset)
      -> Expected<ProcTable> {
    if (auto res = check_bounds(
            data,
            offset,
            SIZE_U32,
            "procedure table offset out of bounds");
        !res) {
      return std::unexpected(res.error());
    }
    const auto count = read_le<uint32_t>(data, offset);
    offset += SIZE_U32;

    ProcTable procs;
    procs.reserve(count);

    for (auto i = 0U; i < count; ++i) {
      if (const auto res = check_bounds(
              data,
              offset,
              SIZE_PROC_DESC,
              "procedure descriptor out of bounds");
          !res) {
        return std::unexpected(res.error());
      }
      ProcDesc desc {};
      desc.bytecode_offset = read_le<uint32_t>(data, offset);
      offset += SIZE_U32;
      desc.bytecode_length = read_le<uint32_t>(data, offset);
      offset += SIZE_U32;
      desc.is_extern = data[offset++] != 0;

      procs.push_back(desc);
    }

    return procs;
  }

}  // namespace musi
