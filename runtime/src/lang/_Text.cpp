#include "_Text.hpp"

namespace musi {

  auto Text::from_literal(const std::string& str)
      -> std::unique_ptr<StringObject> {
    return std::make_unique<StringObject>(str);
  }

  auto Text::length(StringObject* text) -> int64_t {
    return static_cast<int64_t>(text->data.size());
  }

}  // namespace musi
