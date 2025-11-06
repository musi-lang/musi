#include <filesystem>
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include "header.hpp"
#include "loader.hpp"

namespace {
  auto get_home_directory() -> std::string {
    std::filesystem::path home_dir;
// NOLINTBEGIN(concurrency-mt-unsafe)
#ifdef _WIN32
    home_dir = std::getenv("USERPROFILE");
#else
    home_dir = std::getenv("HOME");
// NOLINTEND(concurrency-mt-unsafe)
#endif
    if (home_dir.empty()) {
      throw std::runtime_error("Unable to determine home directory");
    }
    return home_dir.string();
  }
}  // namespace

TEST_CASE("load_bytecode reads file successfully") {
  const auto bc = musi::load_bytecode(
      get_home_directory() + "/musi/runtime/test/basic_binding.msc");
  REQUIRE(bc.has_value());
  CHECK(bc->size() > 0);
}

TEST_CASE("load_bytecode fails on nonexistent file") {
  const auto bc = musi::load_bytecode("/nonexistent/file.msc");
  CHECK_FALSE(bc.has_value());
}

TEST_CASE("load_bytecode validates MUSI magic header") {
  const auto bc = musi::load_bytecode(
      get_home_directory() + "/musi/runtime/test/basic_binding.msc");
  REQUIRE(bc.has_value());
  REQUIRE(bc->size() >= 4);
  CHECK((*bc)[0] == 'M');
  CHECK((*bc)[1] == 'U');
  CHECK((*bc)[2] == 'S');
  CHECK((*bc)[3] == 'I');
}

TEST_CASE("parse_header validates magic") {
  const auto bc = musi::load_bytecode(
      get_home_directory() + "/musi/runtime/test/basic_binding.msc");
  REQUIRE(bc.has_value());

  const auto hdr = musi::parse_header(*bc);
  REQUIRE(hdr.has_value());
  CHECK(hdr->magic[0] == 'M');
  CHECK(hdr->magic[1] == 'U');
  CHECK(hdr->magic[2] == 'S');
  CHECK(hdr->magic[3] == 'I');
}

TEST_CASE("parse_header reads version") {
  const auto bc = musi::load_bytecode(
      get_home_directory() + "/musi/runtime/test/basic_binding.msc");
  REQUIRE(bc.has_value());

  const auto hdr = musi::parse_header(*bc);
  REQUIRE(hdr.has_value());
  CHECK(hdr->major_version == 0);
  CHECK(hdr->minor_version == 1);
}

TEST_CASE("parse_header fails on invalid magic") {
  std::array<uint8_t, 32> bad_data {};
  bad_data[0] = 'B';
  bad_data[1] = 'A';
  bad_data[2] = 'D';
  bad_data[3] = '!';
  const auto hdr = musi::parse_header(bad_data);
  CHECK_FALSE(hdr.has_value());
}

TEST_CASE("parse_header fails on too small file") {
  std::array<uint8_t, 4> small_data {'M', 'U', 'S', 'I'};
  const auto hdr = musi::parse_header(small_data);
  CHECK_FALSE(hdr.has_value());
}

TEST_CASE("parse_export_table reads empty table") {
  std::array<uint8_t, 4> data {0, 0, 0, 0};
  const auto exports = musi::parse_export_table(data, 0);
  REQUIRE(exports.has_value());
  CHECK(exports->empty());
}

TEST_CASE("parse_link_table reads empty table") {
  std::array<uint8_t, 4> data {0, 0, 0, 0};
  const auto links = musi::parse_link_table(data, 0);
  REQUIRE(links.has_value());
  CHECK(links->empty());
}
