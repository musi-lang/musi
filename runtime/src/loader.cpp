#include "loader.hpp"

#include <format>
#include <fstream>

#include "header.hpp"

namespace musi {

  auto load_bytecode(std::string_view path) -> Expected<std::vector<uint8_t>> {
    try {
      std::ifstream file(std::string(path), std::ios::binary | std::ios::ate);
      if (!file) {
        return std::unexpected(std::format("cannot open file '{}'", path));
      }

      const auto size = file.tellg();
      if (size <= 0) {
        return std::unexpected(
            std::format("file '{}' is empty or invalid", path));
      }

      file.seekg(0, std::ios::beg);

      std::vector<uint8_t> data(static_cast<size_t>(size));
      if (!file.read(std::bit_cast<char*>(data.data()), size)) {
        return std::unexpected(std::format("failed reading file '{}'", path));
      }

      const auto hdr_res = parse_header(data);
      if (!hdr_res) {
        return std::unexpected(
            std::format("invalid .msil header: {}", hdr_res.error()));
      }

      return data;
    } catch (const std::exception& ex) {
      return std::unexpected(
          std::format("exception loading '{}': {}", path, ex.what()));
    }
  }

}  // namespace musi
