#include "token.hpp"

namespace musi {
    auto TokenStream::advance() -> const Token& {
        const auto& current_token = peek();
        if (!is_at_end()) {
            m_current_position++;
        }
        return current_token;
    }
    auto TokenStream::advance_by(uint32_t count) -> void {
        for (uint32_t i = 0; i < count; i++) {
            advance();
        }
    }
    auto TokenStream::skip_newlines() -> void {
        while (matches(Token::Kind::Newline)) {
            advance();
        }
    }
}  // namespace musi
