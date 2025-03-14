#include "lexer.hpp"

#include <array>

#include "errors.hpp"
#include "token.hpp"

namespace musi {
    auto Lexer::lex() -> LexResult<Vec<Token>> {
        Vec<Token> tokens;
        Vec<Diagnostic> errors;

        uint32_t consecutive_errors = 0;

        while (!is_at_end()) {
            auto start_offset = m_current_location.offset();

            auto token = next_token();
            if (!token) {
                if (is_at_end()) {
                    break;
                }

                errors.push_back(std::move(token).error());

                if (m_current_location.offset() == start_offset) {
                    sync();
                    consecutive_errors++;
                    if (consecutive_errors > errors::MAX_CONSECUTIVE_ERRORS) {
                        return make_error("too many lexical errors, aborting...");
                    }
                } else {
                    consecutive_errors = 0;
                }
                continue;
            }

            consecutive_errors = 0;
            tokens.push_back(*token);
        }

        if (!errors.empty()) {
            return std::unexpected(std::move(errors.front()));
        }

        while (m_indent_stack.size() > 1) {
            m_indent_stack.pop_back();
            tokens.push_back(make_token(Token::Kind::Dedent, "", m_current_location));
        }
        tokens.push_back(make_token(Token::Kind::Eof, "", m_current_location));
        return tokens;
    }

    auto Lexer::validate_indentation(uint32_t spaces, [[maybe_unused]] SourceLocation location)
        -> bool {
        if (!m_indent_width && spaces > 0) {
            m_indent_width = spaces;
            return true;
        }
        if (m_indent_width && spaces > 0) {
            return spaces % *m_indent_width == 0;
        }
        return true;
    }

    auto Lexer::next_token() -> LexResult<Token> {
        if (!m_pending_tokens.empty()) {
            auto pending_token = m_pending_tokens.front();
            m_pending_tokens.pop();
            return pending_token;
        }

        if (m_at_line_start) {
            if (auto result = handle_line_start(); result) {
                return result;
            }
        }

        if (peek() == '#') {
            return handle_comments();
        }

        return handle_character(peek());
    }
    auto Lexer::handle_line_start() -> LexResult<Token> {
        count_leading_whitespace();

        if (peek() != '#' && peek() != '\n' && peek() != '\r') {
            if (m_current_indent > m_indent_stack.back()) {
                return lex_indent(m_current_indent);
            }
            if (m_current_indent < m_indent_stack.back()) {
                return lex_dedent(m_current_indent);
            }

            m_at_line_start = false;
        }

        return std::unexpected(make_none().error());
    }
    auto Lexer::handle_comments() -> LexResult<Token> {
        while (peek() == '#') {
            skip_comments();
            if (peek() == '\n' || peek() == '\r') {
                return lex_newline();
            }
            count_leading_whitespace();
        }

        return next_token();
    }
    auto Lexer::handle_character(char ch) -> LexResult<Token> {
        switch (ch) {
            case '\n':
            case '\r':
                return lex_newline();
            case ' ':
            case '\t':
                count_leading_whitespace();
                return next_token();
            case '"':
            case '\'':
            case '\\':
                return lex_textual_literal();
            case '-':
                if (is_digit(peek_next())) {
                    return lex_numeric();
                }
                return lex_delimiter_or_operator();
            default:
                if (is_at_end()) {
                    return make_none();
                }
                if (is_digit(ch)) {
                    return lex_numeric();
                }
                if (is_identifier_start(ch)) {
                    return lex_identifier_or_keyword();
                }
                return lex_delimiter_or_operator();
        }
    }
    auto Lexer::match_operator_sequence(
        std::span<const std::pair<Token::Kind, std::string_view>> patterns
    ) -> LexResult<Token> {
        auto current_position = m_current_location.offset() - 1;
        auto content = m_source.get().content();

        for (const auto& [kind, pattern] : patterns) {
            auto pattern_length = pattern.length();
            if (current_position + pattern_length > content.length()) {
                continue;
            }

            auto current_slice = content.substr(current_position, pattern_length);
            if (current_slice == pattern) {
                advance_by(static_cast<uint32_t>(pattern_length - 1));
                return make_token(kind, pattern);
            }
        }

        return make_error(errors::unknown(std::format("pattern '{}'", patterns.front().second)));
    }

    auto Lexer::lex_identifier_or_keyword() -> LexResult<Token> {
        std::string identifier;
        while (!is_at_end() && (is_identifier_start(peek()) || is_identifier_continue(peek()))) {
            identifier += advance();
        }

        if (identifier == "true" || identifier == "false") {
            return make_token(Token::Kind::BoolLiteral, identifier);
        }

        auto keyword_map = Token::map_keywords();
        if (auto it = keyword_map.find(identifier); it != keyword_map.end()) {
            return make_token(it->second, identifier);
        }
        return make_token(Token::Kind::Identifier, identifier);
    }
    auto Lexer::lex_delimiter_or_operator() -> LexResult<Token> {
        auto current_char = advance();
        switch (current_char) {
            case '(':
                return make_token(Token::Kind::LeftParen, "(");
            case ')':
                return make_token(Token::Kind::RightParen, ")");
            case '[':
                return make_token(Token::Kind::LeftBracket, "[");
            case ']':
                return make_token(Token::Kind::RightBracket, "]");
            case '{':
                return make_token(Token::Kind::LeftBrace, "{");
            case '}':
                return make_token(Token::Kind::RightBrace, "}");
            case '+':
                return make_token(Token::Kind::Plus, "+");
            case '-':
                if (is_digit(peek())) {
                    return lex_numeric();
                }
                return match_operator_sequence({ {
                    { Token::Kind::MinusGreater, "->" },
                    { Token::Kind::Minus, "-" },
                } });
            case '*':
                return make_token(Token::Kind::Star, "*");
            case '/':
                return match_operator_sequence({ {
                    { Token::Kind::SlashEquals, "/=" },
                    { Token::Kind::Slash, "/" },
                } });
            case '!':
                return make_token(Token::Kind::Bang, "!");
            case '=':
                return make_token(Token::Kind::Equals, "=");
            case '>':
                return match_operator_sequence({ {
                    { Token::Kind::GreaterEquals, ">=" },
                    { Token::Kind::Greater, ">" },
                } });
            case '<':
                return match_operator_sequence({ {
                    { Token::Kind::LessEqualsGreater, "<=>" },
                    { Token::Kind::LessEquals, "<=" },
                    { Token::Kind::Less, "<" },
                } });
            case '^':
                return make_token(Token::Kind::Caret, "^");
            case '|':
                return match_operator_sequence({ {
                    { Token::Kind::PipeGreater, "|>" },
                    { Token::Kind::Pipe, "|" },
                } });
            case ':':
                return match_operator_sequence({ {
                    { Token::Kind::ColonEquals, ":=" },
                    { Token::Kind::Colon, ":" },
                } });
            case ';':
                return make_token(Token::Kind::Semicolon, ";");
            case ',':
                return make_token(Token::Kind::Comma, ",");
            case '.':
                return match_operator_sequence({ {
                    { Token::Kind::DotDotLess, "..<" },
                    { Token::Kind::DotDot, ".." },
                    { Token::Kind::Dot, "." },
                } });
            case '?':
                return make_token(Token::Kind::Question, "?");
            case '_':
                return make_token(Token::Kind::Underscore, "_");
            default:
                return make_error(errors::unknown(std::format("character '{}'", current_char)));
        }
    }
    auto Lexer::lex_numeric() -> LexResult<Token> {
        std::string number;
        auto start_location = m_current_location;

        if (peek() == '-') {
            number += advance();
        }

        if (peek() == '0' && !is_at_end()) {
            auto next = static_cast<char>(std::tolower(static_cast<unsigned char>(peek_next())));
            if (next == 'x' || next == 'b' || next == 'o') {
                number += advance();
                return lex_radix_number(number);
            }
        }

        return lex_decimal_number(number, start_location);
    }
    auto Lexer::lex_decimal_number(std::string& number, SourceLocation start_location)
        -> LexResult<Token> {
        bool has_decimal = false;
        bool has_suffix = false;

        while (!is_at_end()) {
            if (peek() == '.') {
                if (has_decimal) {
                    return make_error(errors::expected_of("named member", "numeric literal"));
                }

                if (peek_next() == '.' || peek_next() == '<') {
                    break;
                }

                has_decimal = true;
                number += advance();
            } else if (is_digit(peek())) {
                number += advance();
            } else if (peek() == 'e' || peek() == 'E') {
                return lex_exponent(number);
            } else if (!has_suffix && peek() == 'N') {
                has_suffix = true;
                number += advance();
            } else {
                break;
            }
        }

        if (number == "-" || number == ".") {
            return make_error(
                errors::invalid(std::format("numeric literal '{}'", number)),
                start_location
            );
        }

        return make_token(Token::Kind::NumericLiteral, number, start_location);
    }
    auto Lexer::lex_radix_number(std::string& number) -> LexResult<Token> {
        number += advance();
        auto make_number = [&](auto predicate) {
            while (!is_at_end() && predicate(peek())) {
                number += advance();
            }
            return make_token(Token::Kind::NumericLiteral, number);
        };

        switch (peek()) {
            case 'x':
            case 'X':
                number += advance();
                return make_number(is_xdigit);
            case 'b':
            case 'B':
                number += advance();
                return make_number([](auto ch) { return ch == '0' || ch == '1'; });
            case 'o':
            case 'O':
                number += advance();
                return make_number([](auto ch) { return ch >= '0' && ch <= '7'; });
            default:
                return make_error(errors::unknown(std::format("numeric base '{}'", number)));
        }
    }
    auto Lexer::lex_exponent(std::string& number) -> LexResult<Token> {
        number += advance();
        if (peek() == '+' || peek() == '-') {
            number += advance();
        }

        if (!is_digit(peek())) {
            return make_error(errors::expected_in("digit", "exponent notation"));
        }
        while (!is_at_end() && is_digit(peek())) {
            number += advance();
        }

        if (peek() == 'e' || peek() == 'E') {
            return make_error(
                errors::invalid_in(std::format("digit '{}'", peek()), "exponent notation")
            );
        }

        return make_token(Token::Kind::NumericLiteral, number);
    }
    auto Lexer::lex_textual_literal() -> LexResult<Token> {
        auto start_location = m_current_location;

        auto quote_char = advance();
        if (quote_char == '"' && peek() == '"' && peek_next() == '"') {
            advance_by(2);
            return lex_3quote_string(start_location);
        }
        return lex_1quote_string(quote_char, start_location);
    }
    auto Lexer::lex_1quote_string(char quote_char, SourceLocation start_location)
        -> LexResult<Token> {
        std::string content;

        while (!is_at_end()) {
            if (peek() == '\n' || is_at_end()) {
                if (peek() == '\n') {
                    advance();
                }

                return make_error(
                    errors::unterminated(
                        std::string(quote_char == '"' ? "string" : "character") + " literal"
                    ),
                    start_location
                );
            }

            if (peek() == '\\') {
                if (auto result = append_escape_sequence(content, quote_char); !result) {
                    return std::unexpected(std::move(result).error());
                }
                continue;
            }
            if (peek() == quote_char) {
                advance();
                return make_token(
                    quote_char == '"' ? Token::Kind::StrLiteral : Token::Kind::CharLiteral,
                    content,
                    start_location
                );
            }

            content += advance();
        }

        return make_error(
            errors::unterminated(
                std::string(quote_char == '"' ? "string" : "character") + " literal"
            ),
            start_location
        );
    }
    auto Lexer::lex_3quote_string(SourceLocation start_location) -> LexResult<Token> {
        std::string content;

        uint32_t char_count = 0;

        while (!is_at_end()) {
            if (++char_count > MAX_STRING_LENGTH) {
                return make_error(errors::exceeded("maximum string literal length"));
            }

            if (peek() == '\\') {
                if (auto result = append_escape_sequence(content, '"'); !result) {
                    return std::unexpected(std::move(result).error());
                }
                continue;
            }
            if (peek() == '"' && peek_next() == '"' && peek_at(2) == '"') {
                advance_by(3);
                return make_token(Token::Kind::StrLiteral, content, start_location);
            }

            content += advance();
        }

        return make_error(errors::unterminated("string literal"), start_location);
    }
    auto Lexer::lex_newline() -> LexResult<Token> {
        auto start_location = m_current_location;
        advance();

        m_at_line_start = true;
        if (peek_back(2) == '\r' && peek() == '\n') {
            return make_token(Token::Kind::Newline, "\\r\\n", start_location);
        }
        return make_token(Token::Kind::Newline, "\\n", start_location);
    }
    auto Lexer::lex_indent(uint32_t spaces) -> LexResult<Token> {
        auto start_location = SourceLocation(
            m_current_location.filename(),
            { .line = m_current_location.line(), .column = 1 },
            m_current_location.offset() - spaces
        );

        if (!validate_indentation(spaces, start_location)) {
            return make_error(
                std::format("indentation of {} must be multiple of {}", spaces, *m_indent_width),
                start_location
            );
        }

        m_indent_stack.push_back(spaces);
        m_at_line_start = false;
        return make_token(Token::Kind::Indent, "", start_location);
    }
    auto Lexer::lex_dedent(uint32_t spaces) -> LexResult<Token> {
        auto start_location = m_current_location;

        if (spaces >= m_indent_stack.back()) {
            m_at_line_start = false;
            return next_token();
        }

        if (!is_valid_dedent(spaces)) {
            return make_error(errors::mismatched("indentation"), start_location);
        }
        generate_dedent_tokens(spaces, start_location);

        if (!m_pending_tokens.empty()) {
            auto pending_token = m_pending_tokens.front();
            m_pending_tokens.pop();
            m_at_line_start = false;
            return pending_token;
        }

        return make_error(errors::unknown("dedent processing error"), start_location);
    }

    auto Lexer::convert_escape_sequence() -> LexResult<std::string> {
        advance();

        auto start_location = m_current_location;

        if (is_at_end()) {
            return make_error(std::format("escape sequence '\\{}' at end of file", peek()));
        }

        auto escape_char = advance();
        switch (std::tolower(escape_char)) {
            case 'n':
                return "\n";
            case 'r':
                return "\r";
            case 't':
                return "\t";
            case '\\':
                return "\\";
            case '\'':
                return "'";
            case '"':
                return "\"";
            case '0':
                return "\0";
            case 'a':
                return "\a";
            case 'b':
                return "\b";
            case 'f':
                return "\f";
            case 'v':
                return "\v";
            case 'u':
                return make_error("unicode not implemented");
            default:
                return make_error(
                    errors::unknown(std::format("escape sequence '{}'", escape_char)),
                    start_location
                );
        }
    }
    auto Lexer::process_escape_sequence(char quote) -> LexResult<std::string> {
        auto escaped_string = convert_escape_sequence();
        if (!escaped_string) {
            while (!is_at_end() && peek() != quote && peek() != '\n') {
                advance();
            }
            if (peek() == quote) {
                advance();
            }
            return std::unexpected(std::move(escaped_string).error());
        }
        return *escaped_string;
    }
    auto Lexer::append_escape_sequence(std::string& content, char quote) -> LexResult<void> {
        auto escaped_string = process_escape_sequence(quote);
        if (!escaped_string) {
            return std::unexpected(std::move(escaped_string).error());
        }
        content += *escaped_string;
        return {};
    }

    auto Lexer::is_valid_dedent(uint32_t spaces) -> bool {
        return std::ranges::any_of(m_indent_stack, [spaces](auto indent) {
            return indent == spaces;
        });
    }
    auto Lexer::peek_at(uint32_t offset) const -> char {
        auto current_offset = m_current_location.offset();
        auto content = m_source.get().content();
        if (current_offset + offset >= content.length()) {
            return '\0';
        }
        return content.at(current_offset + offset);
    }
    auto Lexer::peek_back(uint32_t offset) const -> char {
        auto current_offset = m_current_location.offset();
        auto content = m_source.get().content();
        if (offset > current_offset) {
            return '\0';
        }
        return content.at(current_offset - offset);
    }
    auto Lexer::advance() -> char {
        auto current_char = peek();
        auto current_offset = m_current_location.offset();
        if (current_char == '\n') {
            m_current_location = { m_current_location.filename(),
                                   { .line = m_current_location.line() + 1, .column = 1 },
                                   current_offset + 1 };
        } else {
            m_current_location = { m_current_location.filename(),
                                   m_source.get().line_column(current_offset + 1),
                                   current_offset + 1 };
        }
        return current_char;
    }
    auto Lexer::advance_until(const std::function<bool()>& predicate) -> void {
        while (!is_at_end() && !predicate()) {
            advance();
        }
    }
    auto Lexer::advance_by(uint32_t count) -> void {
        for (uint32_t i = 0; i < count; i++) {
            advance();
        }
    }
    auto Lexer::skip_comments() -> void {
        while (!is_at_end() && peek() != '\n') {
            advance();
        }
    }
    auto Lexer::count_leading_whitespace() -> void {
        if (peek() == '\f') {
            advance();
        }

        uint32_t spaces = 0;
        auto has_tabs = false;
        auto has_spaces = false;
        auto start_location = m_current_location;

        while (peek() == ' ' || peek() == '\t') {
            if (peek() == '\t') {
                spaces += TAB_WIDTH - (spaces % TAB_WIDTH);
                has_tabs = true;
            } else {
                spaces++;
                has_spaces = true;
            }
            advance();
        }

        if (has_tabs && has_spaces) {
            auto diagnostic = Diagnostic(
                DiagnosticSeverity::Error,
                errors::mixed("tabs and spaces"),
                SourceSpan { start_location, m_current_location.offset() - start_location.offset() }
            );
            m_diagnostics.get().emit_error(diagnostic.span(), std::string(diagnostic.message()));
        }

        m_current_indent = spaces;
    }
    auto Lexer::generate_dedent_tokens(uint32_t spaces, SourceLocation location) -> void {
        while (!m_indent_stack.empty() && m_indent_stack.back() > spaces) {
            m_indent_stack.pop_back();
            m_pending_tokens.push(make_token(Token::Kind::Dedent, "", location));
        }
    }

    auto Lexer::sync() -> void {
        if (sync_at_statement_boundary() || sync_at_keyword()) {
        } else {
            advance_until([this]() { return is_at_end() || peek() == '\n' || peek() == '\r'; });

            if (!is_at_end()) {
                advance();
                m_at_line_start = true;
                m_current_indent = 0;
            }
        }

        while (!m_pending_tokens.empty()) {
            m_pending_tokens.pop();
        }
    }
    auto Lexer::sync_at_statement_boundary() -> bool {
        auto current_char = peek();
        if (current_char == '\n' || current_char == '\r' || current_char == ';') {
            advance();

            if (current_char == '\n' || current_char == '\r') {
                m_at_line_start = true;
                m_current_indent = 0;
            }
            return true;
        }
        if (current_char == '}' || current_char == ')' || current_char == ']') {
            advance();
            return true;
        }
        if (current_char == ':' && peek_next() == '=') {
            advance_by(2);
            return true;
        }
        return false;
    }
    auto Lexer::sync_at_keyword() -> bool {
        static const std::array<std::string_view, 13> keywords = {
            "then",  "else",  "do",  "unless", "return", "if",  "for",
            "while", "until", "let", "var",    "func",   "proc"
        };

        return std::ranges::any_of(keywords, [this](const auto& keyword) {
            return try_keyword_sync(keyword);
        });
    }
    auto Lexer::try_keyword_sync(std::string_view keyword) -> bool {
        if (m_current_location.offset() + keyword.length() > m_source.get().content().length()) {
            return false;
        }

        auto content_slice =
            m_source.get().content().substr(m_current_location.offset(), keyword.length());
        if (content_slice == keyword) {
            if (m_current_location.offset() + keyword.length() >= m_source.get().content().length()
                || !is_identifier_continue(m_source.get().content(
                )[m_current_location.offset() + keyword.length()])) {
                advance_by(static_cast<uint32_t>(keyword.length()));
                return true;
            }
        }
        return false;
    }

    auto Lexer::make_none() -> std::unexpected<Diagnostic> {
        return std::unexpected<Diagnostic>(
            Diagnostic(DiagnosticSeverity::None, "", SourceSpan { m_current_location, 0 })
        );
    }
    auto Lexer::make_error(std::string_view message, Option<SourceLocation> location)
        -> std::unexpected<Diagnostic> {
        auto diagnostic = Diagnostic(
            DiagnosticSeverity::Error,
            std::string(message),
            SourceSpan { location.value_or(m_current_location), 0 }
        );
        m_diagnostics.get().emit_error(diagnostic.span(), std::string(diagnostic.message()));
        return std::unexpected(std::move(diagnostic));
    }
    auto Lexer::make_token(
        Token::Kind kind,
        std::string_view lexeme,
        Option<SourceLocation> location
    ) -> Token {
        if (kind == Token::Kind::Eof) {
            return { kind, lexeme, m_current_location };
        }

        auto start_location = location.value_or([&] {
            return SourceLocation(
                m_current_location.filename(),
                m_source.get().line_column(
                    static_cast<uint32_t>(m_current_location.offset() - lexeme.length())
                ),
                static_cast<uint32_t>(lexeme.length())
            );
        }());

        return { kind, lexeme, start_location };
    }
}  // namespace musi
