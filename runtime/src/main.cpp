#include <cstdlib>
#include <print>
#include <span>

#include "header.hpp"
#include "intrinsics.hpp"
#include "loader.hpp"
#include "vm.hpp"

using musi::Bytecode;
using musi::ConstPool;
using musi::Header;
using musi::LinkEntryList;
using musi::ProcTable;
using musi::VM;

namespace {
  auto parse_args(int argc, char** argv) -> std::string_view {
    const std::span args(argv, static_cast<size_t>(argc));
    if (args.size() != 2) {
      std::println(stderr, "Usage: {} <filename.msil>", args[0]);
      std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
    }
    return {args[1]};
  }

  auto load_and_parse_bytecode(const std::string_view& filename) -> std::tuple<
      std::span<const uint8_t>,
      Header,
      ConstPool,
      ProcTable,
      LinkEntryList> {
    const auto bc_result = musi::load_bytecode(filename);
    if (!bc_result) {
      std::println(stderr, "{}", bc_result.error());
      std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
    }
    const auto& bc = bc_result.value();

    const auto hdr_result = musi::parse_header(bc);
    if (!hdr_result) {
      std::println(stderr, "{}", hdr_result.error());
      std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
    }
    const auto& hdr = hdr_result.value();

    const auto pool_result = musi::parse_const_pool(bc, 32);
    if (!pool_result) {
      std::println(stderr, "{}", pool_result.error());
      std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
    }
    const auto& const_pool = pool_result.value();

    LinkEntryList link_entries;
    if (hdr.link_count > 0) {
      const auto link_result = musi::parse_link_table(bc, hdr.link_offset);
      if (!link_result) {
        std::println(stderr, "{}", link_result.error());
        std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
      }
      link_entries = link_result.value();
    }

    auto const_pool_end = 32U;
    if (const_pool_end + 4 <= bc.size()) {
      const auto count =
          static_cast<uint32_t>(bc[const_pool_end])
          | (static_cast<uint32_t>(bc[const_pool_end + 1U]) << 8U)
          | (static_cast<uint32_t>(bc[const_pool_end + 2U]) << 16U)
          | (static_cast<uint32_t>(bc[const_pool_end + 3U]) << 24U);
      const_pool_end += 4;
      for (auto i = 0U; i < count; ++i) {
        if (const_pool_end >= bc.size()) {
          break;
        }
        const auto tag = bc[const_pool_end++];
        if (tag == 0x05) {
          if (const_pool_end + 4 > bc.size()) {
            break;
          }
          const auto len =
              static_cast<uint32_t>(bc[const_pool_end])
              | (static_cast<uint32_t>(bc[const_pool_end + 1U]) << 8U)
              | (static_cast<uint32_t>(bc[const_pool_end + 2U]) << 16U)
              | (static_cast<uint32_t>(bc[const_pool_end + 3U]) << 24U);
          const_pool_end += 4 + len;
        }
      }
    }
    const auto proc_result = musi::parse_proc_table(bc, const_pool_end);
    if (!proc_result) {
      std::println(stderr, "{}", proc_result.error());
      std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
    }
    auto proc_table = proc_result.value();
    for (auto& proc : proc_table) {
      proc.bytecode_offset -= static_cast<int>(hdr.bc_offset);
    }

    return {bc, hdr, const_pool, proc_table, link_entries};
  }

  auto setup_vm(
      const std::span<const uint8_t>& bc,
      const Header& hdr,
      const ConstPool& const_pool,
      const ProcTable& func_table,
      const LinkEntryList& link_entries) -> VM {
    const auto bc_start = hdr.bc_offset;
    std::span<const uint8_t> bc_span = bc;
    VM vm(
        Bytecode {bc_span.subspan(bc_start, hdr.bc_size)},
        const_pool,
        func_table,
        link_entries);
    vm.register_intrinsic("musi.io.write", musi::intrinsics::io_write);
    vm.register_intrinsic("musi.io.writeln", musi::intrinsics::io_writeln);
    return vm;
  }
}  // namespace

auto main(int argc, char** argv) -> int {
  try {
    const auto filename = parse_args(argc, argv);
    const auto [bc, hdr, const_pool, func_table, link_entries] =
        load_and_parse_bytecode(filename);
    auto vm = setup_vm(bc, hdr, const_pool, func_table, link_entries);

    const auto exec_result = vm.exec();
    if (!exec_result) {
      std::println(stderr, "{}", exec_result.error());
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
