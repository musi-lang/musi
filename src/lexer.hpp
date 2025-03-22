#pragma once

#include <expected>
#include <queue>
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

        static constexpr auto is_identifier_start(char ch) -> bool {
            return is_alpha(ch) || ch == '_';
        }
        static constexpr auto is_identifier_continue(char ch) -> bool {
            return is_alnum(ch) || ch == '_';
        }

        Lexer(const Source& source, DiagnosticEngine& diagnostics)
            : m_diagnostics(diagnostics)
            , m_reader(source) {}

        auto lex() -> LexResult<Vec<Token>>;

    private:
        [[nodiscard]] static auto is_arithmetic_operator(char ch) -> bool {
            return ch == '+' || ch == '-' || ch == '*' || ch == '/' || ch == '%';
        }

        [[nodiscard]] auto is_consecutive_operator(char op) const -> bool {
            return m_reader.peek() == op;
        }

        auto lex_token() -> LexResult<Token>;
        auto lex_identifier() -> LexResult<Token>;
        auto lex_symbol() -> LexResult<Token>;
        auto lex_number(std::string& number, SourceLocation start_location) -> LexResult<Token>;
        auto lex_exponent(std::string& number) -> LexResult<Token>;
        auto lex_string(char quote, SourceLocation start_location) -> LexResult<Token>;
        auto lex_newline() -> LexResult<Token>;

        auto process_line_start() -> LexResult<Token>;
        auto process_comments() -> LexResult<Token>;
        auto process_character(char symbol) -> LexResult<Token>;
        auto process_number(bool negated = false) -> LexResult<Token>;
        auto process_string() -> LexResult<Token>;
        auto process_escape() -> LexResult<std::string>;
        auto process_escape_char(char quote) -> LexResult<std::string>;
        auto process_escape_append(std::string& content, char quote) -> LexResult<void>;

        auto handle_radix_number(std::string& number, char radix) -> LexResult<Token>;
        auto handle_indentation(uint32_t spaces) -> bool;
        auto handle_indent(uint32_t spaces) -> LexResult<Token>;
        auto handle_dedent(uint32_t spaces) -> LexResult<Token>;
        auto handle_dedent_level(uint32_t spaces) -> bool;

        auto match_operator_sequence(
            std::span<const std::pair<Token::Kind, std::string_view>> patterns
        ) -> LexResult<Token>;

        auto skip_comments() -> void;
        auto count_leading_whitespace() -> void;
        auto emit_dedent_token(uint32_t spaces, SourceLocation location) -> void;

        auto sync() -> void;
        auto sync_statement() -> bool;
        auto sync_keyword() -> bool;
        auto sync_keyword_match(std::string_view keyword) -> bool;

        auto make_none() -> std::unexpected<Diagnostic>;
        auto make_error(std::string_view message, Option<SourceLocation> location = std::nullopt)
            -> std::unexpected<Diagnostic>;
        auto make_token(
            Token::Kind kind,
            std::string_view lexeme,
            Option<SourceLocation> location = std::nullopt
        ) -> Token;

        bool m_at_line_start { true };
        uint32_t m_current_indent { 0 };
        Option<uint32_t> m_indent_width;
        Vec<uint32_t> m_indent_stack { 0 };
        TokenQueue m_pending_tokens;
        Ref<DiagnosticEngine> m_diagnostics;
        SourceReader m_reader;
    };
}  // namespace musi
