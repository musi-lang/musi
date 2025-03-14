#pragma once

#include <expected>
#include <queue>
#include <span>
#include <string_view>

#include "common.hpp"
#include "diagnostics.hpp"
#include "source.hpp"
#include "token.hpp"

namespace musi {
    template<typename T>
    using LexResult = Result<T, Diagnostic>;

    class Lexer {
    public:
        using TokenQueue = std::queue<Token>;

        static constexpr uint8_t TAB_WIDTH = 8;
        static constexpr uint32_t MAX_STRING_LENGTH = 65535;

        Lexer(const Source& source, DiagnosticEngine& diagnostics)
            : m_current_location(source.name(), { .line = 1, .column = 1 }, 0)
            , m_diagnostics(diagnostics)
            , m_source(source) {}

        [[nodiscard]] auto lex() -> LexResult<Vec<Token>>;

    private:
        static constexpr auto is_identifier_start(char ch) -> bool {
            return is_alpha(ch) || ch == '_';
        }
        static constexpr auto is_identifier_continue(char ch) -> bool {
            return is_alnum(ch) || ch == '_';
        }

        [[nodiscard]] auto validate_indentation(uint32_t spaces, SourceLocation location) -> bool;

        [[nodiscard]] auto next_token() -> LexResult<Token>;
        [[nodiscard]] auto handle_line_start() -> LexResult<Token>;
        [[nodiscard]] auto handle_comments() -> LexResult<Token>;
        [[nodiscard]] auto handle_character(char ch) -> LexResult<Token>;
        [[nodiscard]] auto match_operator_sequence(
            std::span<const std::pair<Token::Kind, std::string_view>> patterns
        ) -> LexResult<Token>;

        [[nodiscard]] auto lex_identifier_or_keyword() -> LexResult<Token>;
        [[nodiscard]] auto lex_delimiter_or_operator() -> LexResult<Token>;
        [[nodiscard]] auto lex_numeric() -> LexResult<Token>;
        [[nodiscard]] auto lex_decimal_number(std::string& number, SourceLocation start_location)
            -> LexResult<Token>;
        [[nodiscard]] auto lex_radix_number(std::string& number) -> LexResult<Token>;
        [[nodiscard]] auto lex_exponent(std::string& number) -> LexResult<Token>;
        [[nodiscard]] auto lex_textual_literal() -> LexResult<Token>;
        [[nodiscard]] auto lex_1quote_string(char quote_char, SourceLocation start_location)
            -> LexResult<Token>;
        [[nodiscard]] auto lex_3quote_string(SourceLocation start_location) -> LexResult<Token>;
        [[nodiscard]] auto lex_newline() -> LexResult<Token>;
        [[nodiscard]] auto lex_indent(uint32_t spaces) -> LexResult<Token>;
        [[nodiscard]] auto lex_dedent(uint32_t spaces) -> LexResult<Token>;

        [[nodiscard]] auto convert_escape_sequence() -> LexResult<std::string>;
        [[nodiscard]] auto process_escape_sequence(char quote) -> LexResult<std::string>;
        auto append_escape_sequence(std::string& content, char quote) -> LexResult<void>;

        [[nodiscard]] auto is_valid_dedent(uint32_t spaces) -> bool;
        [[nodiscard]] auto is_at_end() const -> bool {
            return m_current_location.offset() == m_source.get().content().length();
        }
        [[nodiscard]] auto peek_at(uint32_t offset) const -> char;
        [[nodiscard]] auto peek_back(uint32_t offset) const -> char;
        [[nodiscard]] auto peek() const -> char {
            return peek_at(0);
        }
        [[nodiscard]] auto peek_next() const -> char {
            return peek_at(1);
        }
        [[nodiscard]] auto peek_previous() const -> char {
            return peek_back(1);
        }
        auto advance() -> char;
        auto advance_until(const std::function<bool()>& predicate) -> void;
        auto advance_by(uint32_t count) -> void;
        auto matches(char expected) -> bool {
            if (is_at_end() || peek() != expected) {
                return false;
            }
            advance();
            return true;
        }
        auto skip_comments() -> void;
        auto count_leading_whitespace() -> void;
        auto generate_dedent_tokens(uint32_t spaces, SourceLocation location) -> void;

        auto sync() -> void;
        auto sync_at_statement_boundary() -> bool;
        auto sync_at_keyword() -> bool;
        auto try_keyword_sync(std::string_view keyword) -> bool;

        [[nodiscard]] auto make_none() -> std::unexpected<Diagnostic>;
        [[nodiscard]] auto make_error(
            std::string_view message,
            Option<SourceLocation> location = std::nullopt
        ) -> std::unexpected<Diagnostic>;
        [[nodiscard]] auto make_token(
            Token::Kind kind,
            std::string_view lexeme,
            Option<SourceLocation> location = std::nullopt
        ) -> Token;

        bool m_at_line_start { true };
        uint32_t m_current_indent { 0 };
        Vec<uint32_t> m_indent_stack { 0 };
        Option<uint32_t> m_indent_width;
        SourceLocation m_current_location;
        TokenQueue m_pending_tokens;
        Ref<DiagnosticEngine> m_diagnostics;
        Ref<const Source> m_source;
    };
}  // namespace musi
