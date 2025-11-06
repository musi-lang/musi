#include <cstdlib>
#include <print>
#include <span>

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
      std::println(stderr, "error: {}", bc_result.error());
      return EXIT_FAILURE;
    }

    const auto& bc = bc_result.value();
    musi::VM vm(musi::Bytecode {bc.data(), bc.size()});

    const auto exec_result = vm.exec();
    if (!exec_result) {
      std::println(stderr, "runtime error: {}", exec_result.error());
      return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
  } catch (const std::exception& ex) {
    std::println(stderr, "fatal error: {}", ex.what());
    return EXIT_FAILURE;
  } catch (...) {
    std::println(stderr, "fatal error: unknown exception");
    return EXIT_FAILURE;
  }
}
