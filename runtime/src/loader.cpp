#include "loader.hpp"

#include <format>
#include <fstream>

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

      std::vector<uint8_t> bc(static_cast<size_t>(size));
      if (!file.read(std::bit_cast<char*>(bc.data()), size)) {
        return std::unexpected(std::format("failed reading file '{}'", path));
      }

      return bc;
    } catch (const std::exception& e) {
      return std::unexpected(
          std::format("exception loading '{}': {}", path, e.what()));
    }
  }

}  // namespace musi
