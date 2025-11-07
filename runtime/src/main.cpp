#include <cstdlib>
#include <print>
#include <span>

#include "intrinsics.hpp"
#include "loader.hpp"
#include "vm.hpp"

auto main(int argc, char** argv) -> int {
  try {
    const std::span args(argv, static_cast<size_t>(argc));

    if (args.size() != 2) {
      std::println(stderr, "usage: {} <filename.msc>", args[0]);
      return EXIT_FAILURE;
    }

    const std::string_view filename(args[1]);
    const auto bc_result = musi::load_bytecode(filename);
    if (!bc_result) {
      std::println(stderr, "{}", bc_result.error());
      return EXIT_FAILURE;
    }

    const auto& bc = bc_result.value();
    musi::VM vm(musi::Bytecode {bc.data(), bc.size()});
    vm.register_intrinsic("musi.io.write", musi::intrinsics::io_write);
    vm.register_intrinsic("musi.io.writeln", musi::intrinsics::io_writeln);

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
