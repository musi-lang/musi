#include <cstdlib>
#include <print>
#include <span>

#include "_io.hpp"
#include "header.hpp"
#include "loader.hpp"
#include "module.hpp"
#include "spdlog/spdlog.h"
#include "vm.hpp"

using musi::Bytecode;
using musi::ConstPool;
using musi::Header;
using musi::LinkEntryList;
using musi::ProcTable;
using musi::VM;

namespace {

  constexpr uint8_t LINK_ENTRY_TAG = 0x05;
  constexpr size_t UINT32_SIZE = 4;
  constexpr uint32_t BYTE_SHIFT_8 = 8U;
  constexpr uint32_t BYTE_SHIFT_16 = 16U;
  constexpr uint32_t BYTE_SHIFT_24 = 24U;

  auto read_uint32(const std::span<const uint8_t>& bc, const size_t offset)
      -> uint32_t {
    return static_cast<uint32_t>(bc[offset])
           | (static_cast<uint32_t>(bc[offset + 1U]) << BYTE_SHIFT_8)
           | (static_cast<uint32_t>(bc[offset + 2U]) << BYTE_SHIFT_16)
           | (static_cast<uint32_t>(bc[offset + 3U]) << BYTE_SHIFT_24);
  }

  auto parse_args(int argc, char** argv) -> std::string_view {
    const std::span args(argv, static_cast<size_t>(argc));
    if (args.size() != 2) {
      std::println(stderr, "Usage: {} <filename.msil>", args[0]);
      std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
    }
    return {args[1]};
  }

  auto load_and_parse_bytecode(const std::string_view& filename) -> std::
      tuple<std::vector<uint8_t>, Header, ConstPool, ProcTable, LinkEntryList> {
    const auto bc_res = musi::load_bytecode(filename);
    if (!bc_res) {
      std::println(stderr, "{}", bc_res.error());
      std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
    }
    const auto& bc = bc_res.value();

    const auto hdr_res = musi::parse_header(bc);
    if (!hdr_res) {
      std::println(stderr, "{}", hdr_res.error());
      std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
    }
    const auto& hdr = hdr_res.value();

    const auto pool_res = musi::parse_const_pool(bc, hdr.metadata_offset);
    if (!pool_res) {
      std::println(stderr, "{}", pool_res.error());
      std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
    }
    const auto& const_pool = pool_res.value();

    LinkEntryList link_entries;

    auto const_pool_end = static_cast<uint32_t>(hdr.metadata_offset);
    if (const_pool_end + UINT32_SIZE <= bc.size()) {
      const auto count = read_uint32(bc, const_pool_end);
      const_pool_end += UINT32_SIZE;
      for (auto i = 0U; i < count; ++i) {
        if (const_pool_end >= bc.size()) {
          break;
        }
        const auto tag = bc[const_pool_end++];
        if (tag == LINK_ENTRY_TAG) {
          if (const_pool_end + UINT32_SIZE > bc.size()) {
            break;
          }
          const auto len = read_uint32(bc, const_pool_end);
          const_pool_end += UINT32_SIZE + len;
        }
      }
    }
    const auto proc_res = musi::parse_proc_table(bc, const_pool_end);
    if (!proc_res) {
      std::println(stderr, "{}", proc_res.error());
      std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
    }
    auto proc_table = proc_res.value();

    return {bc_res.value(), hdr, const_pool, proc_table, link_entries};
  }

  auto setup_vm(
      const std::vector<uint8_t>& bc,
      const Header& hdr,
      const ConstPool& const_pool,
      const ProcTable& func_table,
      const LinkEntryList& link_entries) -> VM {
    const auto bc_start = hdr.bc_offset;
    std::span<const uint8_t> bc_span {bc};
    const auto bytecode_span = bc_span.subspan(bc_start, hdr.bc_size);
    spdlog::trace(
        "bc_start={} bc_size={} first_byte={:#04x}",
        bc_start,
        hdr.bc_size,
        bytecode_span[0]);
    VM vm(Bytecode {bytecode_span}, const_pool, func_table, link_entries);
    vm.register_intrinsic("musi.io.write", musi::intrinsics::io::write);
    vm.register_intrinsic("musi.io.writeln", musi::intrinsics::io::writeln);
    return vm;
  }
}  // namespace

auto main(int argc, char** argv) -> int {
  spdlog::set_level(spdlog::level::trace);
  try {
    const auto filename = parse_args(argc, argv);

    musi::ModuleRegistry registry;
    const auto module_res = registry.load_module(filename);
    if (!module_res) {
      std::println(stderr, "{}", module_res.error());
      return EXIT_FAILURE;
    }

    const auto* module = module_res.value();
    auto vm = setup_vm(
        module->bytecode,
        module->header,
        module->const_pool,
        module->proc_table,
        {});

    const auto exec_res = vm.exec();
    if (!exec_res) {
      std::println(stderr, "{}", exec_res.error());
      return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
  } catch (const std::exception& ex) {
    std::println(stderr, "{}", ex.what());
    return EXIT_FAILURE;
  } catch (...) {
    std::println(stderr, "unknown exception caught");
    return EXIT_FAILURE;
  }
}
