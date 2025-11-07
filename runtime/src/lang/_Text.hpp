#pragma once

#include <memory>
#include <string>

#include "object.hpp"

namespace musi::intrinsics::lang {

  class Text {
public:
    static auto from_literal(const std::string& str)
        -> std::unique_ptr<StringObject>;
    static auto length(StringObject* text) -> int64_t;
  };

}  // namespace musi::intrinsics::lang
