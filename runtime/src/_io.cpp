#include "_io.hpp"

#include <print>

#include "object.hpp"

namespace musi::intrinsics::io {
  void write(VM& vm) {
    const auto val = vm.pop();
    if (const auto* obj_ptr = std::get_if<Object*>(&val)) {
      if (const auto* str = dynamic_cast<StringObject*>(*obj_ptr)) {
        std::print("{}", str->data);
      }
    }
  }

  void writeln(VM& vm) {
    const auto val = vm.pop();
    if (const auto* obj_ptr = std::get_if<Object*>(&val)) {
      if (const auto* str = dynamic_cast<StringObject*>(*obj_ptr)) {
        std::println("{}", str->data);
      }
    }
  }
}  // namespace musi::intrinsics::io
