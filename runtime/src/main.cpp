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
using musi::VM;

namespace {
  auto parse_args(int argc, char** argv) -> std::string_view {
    const std::span args(argv, static_cast<size_t>(argc));
    if (args.size() != 2) {
      std::println(stderr, "Usage: {} <filename.msc>", args[0]);
      std::exit(EXIT_FAILURE);  // NOLINT(concurrency-mt-unsafe)
    }
    return {args[1]};
  }

  auto load_and_parse_bytecode(const std::string_view& filename)
      -> std::tuple<std::span<const uint8_t>, Header, ConstPool> {
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

    return {bc, hdr, const_pool};
  }

  auto setup_vm(
      const std::span<const uint8_t>& bc,
      const Header& hdr,
      const ConstPool& const_pool) -> VM {
    const auto bc_start = hdr.bc_offset;
    std::span<const uint8_t> bc_span = bc;
    VM vm(Bytecode {bc_span.subspan(bc_start, hdr.bc_size)}, const_pool);
    vm.register_intrinsic("musi.io.write", musi::intrinsics::io_write);
    vm.register_intrinsic("musi.io.writeln", musi::intrinsics::io_writeln);
    return vm;
  }
}  // namespace

auto main(int argc, char** argv) -> int {
  try {
    const auto filename = parse_args(argc, argv);
    const auto [bc, hdr, const_pool] = load_and_parse_bytecode(filename);
    auto vm = setup_vm(bc, hdr, const_pool);

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
