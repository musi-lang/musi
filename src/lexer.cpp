#include "lexer.hpp"

#include <array>

#include "errors.hpp"
#include "token.hpp"

namespace musi {
    auto Lexer::lex() -> LexResult<Vec<Token>> {
        Vec<Token> tokens;
        Vec<Diagnostic> errors;

        uint32_t consecutive_errors = 0;

        while (!m_reader.is_at_end()) {
            auto start_offset = m_reader.location().offset();

            auto token = lex_token();
            if (!token) {
                if (m_reader.is_at_end()) {
                    break;
                }

                errors.push_back(std::move(token).error());

                if (m_reader.location().offset() == start_offset) {
                    sync();
                    consecutive_errors++;
                    if (consecutive_errors > errors::MAX_CONSECUTIVE_ERRORS) {
                        return std::unexpected(Diagnostic(
                            DiagnosticSeverity::Error,
                            "too many lexical errors, aborting...",
                            SourceSpan {}
                        ));
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
            tokens.push_back(make_token(Token::Kind::Dedent, "<DEDENT>", m_reader.location()));
        }
        tokens.push_back(make_token(Token::Kind::Eof, "<EOF>", m_reader.location()));
        return tokens;
    }

    auto Lexer::lex_token() -> LexResult<Token> {
        if (!m_pending_tokens.empty()) {
            auto pending_token = m_pending_tokens.front();
            m_pending_tokens.pop();
            return pending_token;
        }

        if (m_at_line_start) {
            if (auto result = process_line_start(); result) {
                return result;
            }
        }

        if (m_reader.peek() == '#') {
            return process_comments();
        }
        return process_character(m_reader.peek());
    }
    auto Lexer::lex_identifier() -> LexResult<Token> {
        std::string identifier;
        while (!m_reader.is_at_end()
               && (is_identifier_start(m_reader.peek()) || is_identifier_continue(m_reader.peek()))
        ) {
            identifier += m_reader.advance();
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
    auto Lexer::lex_symbol() -> LexResult<Token> {
        auto start_location = m_reader.location();

        auto current_char = m_reader.advance();
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
                if (is_digit(m_reader.peek())) {
                    return process_number();
                }
                if (m_reader.peek() == '+') {
                    auto error_location = m_reader.location();
                    m_reader.advance();
                    return make_error("too many consecutive plus operators", error_location);
                }
                return make_token(Token::Kind::Plus, "+");
            case '-':
                if (is_digit(m_reader.peek())) {
                    return process_number(true);
                }
                if (m_reader.peek() == '-') {
                    auto error_location = m_reader.location();
                    m_reader.advance();
                    return make_error("too many consecutive minus operators", error_location);
                }
                return make_token(Token::Kind::Minus, "-");
            case '*':
                if (is_consecutive_operator('*')) {
                    auto error_location = m_reader.location();
                    m_reader.advance();
                    return make_error(
                        "too many consecutive multiplication operators",
                        error_location
                    );
                }
                return make_token(Token::Kind::Star, "*");
            case '/':
                if (is_consecutive_operator('/')) {
                    auto error_location = m_reader.location();
                    m_reader.advance();
                    return make_error("too many consecutive division operators", error_location);
                }
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
            case '#':
                return make_token(Token::Kind::Hash, "#");
            case '@':
                return make_token(Token::Kind::At, "@");
            case '`':
                return make_token(Token::Kind::Backtick, "`");
            default:
                return make_error(
                    errors::unknown(std::format("character '{}'", current_char)),
                    start_location
                );
        }
    }
    auto Lexer::lex_number(std::string& number, SourceLocation start_location) -> LexResult<Token> {
        bool has_decimal_point = false;
        bool has_suffix = false;

        while (!m_reader.is_at_end()) {
            if (m_reader.peek() == '.') {
                if (m_reader.peek_next() == '.' || m_reader.peek_next() == '<') {
                    break;
                }

                if (has_decimal_point) {
                    return make_error(errors::multiple_in("decimal points", "real literal"));
                }
                if (!is_digit(m_reader.peek_next())) {
                    return make_error(errors::missing_after("digit", "decimal point"));
                }

                has_decimal_point = true;
                number += m_reader.advance();
            } else if (is_digit(m_reader.peek())) {
                number += m_reader.advance();
            } else if (m_reader.peek() == 'e' || m_reader.peek() == 'E') {
                return lex_exponent(number);
            } else if (!has_suffix && m_reader.peek() == 'N') {
                has_suffix = true;
                number += m_reader.advance();
            } else if (is_alpha(m_reader.peek())) {
                return make_error(errors::invalid_in("digit", "natural literal"));
            } else {
                break;
            }
        }

        if (number == "-" || number == ".") {
            return make_error(errors::invalid("numeric literal"), start_location);
        }

        if (has_decimal_point) {
            return make_token(Token::Kind::RealLiteral, number, start_location);
        }
        if (number.starts_with('-')) {
            return make_token(Token::Kind::IntLiteral, number, start_location);
        }
        if (has_suffix) {
            return make_token(Token::Kind::NatLiteral, number, start_location);
        }
        return make_token(Token::Kind::IntLiteral, number, start_location);
    }
    auto Lexer::lex_exponent(std::string& number) -> LexResult<Token> {
        number += m_reader.advance();

        if (m_reader.peek() == '+' || m_reader.peek() == '-') {
            number += m_reader.advance();
        }
        if (!is_digit(m_reader.peek())) {
            return make_error(errors::expected_in("digit", "real exponent"));
        }

        while (!m_reader.is_at_end() && is_digit(m_reader.peek())) {
            number += m_reader.advance();
        }

        if (m_reader.peek() == 'e' || m_reader.peek() == 'E') {
            return make_error(errors::invalid_in("digit", "real exponent"));
        }
        return make_token(Token::Kind::RealLiteral, number);
    }
    auto Lexer::lex_string(char quote, SourceLocation start_location) -> LexResult<Token> {
        std::string content;

        while (!m_reader.is_at_end()) {
            if (m_reader.peek() == '\n' || m_reader.is_at_end()) {
                if (m_reader.peek() == '\n') {
                    m_reader.advance();
                }

                return make_error(
                    errors::unterminated(
                        std::string(quote == '"' ? "string" : "character") + " literal"
                    ),
                    start_location
                );
            }
            if (m_reader.peek() == '\\') {
                if (auto result = process_escape_append(content, quote); !result) {
                    return std::unexpected(std::move(result).error());
                }
                continue;
            }
            if (m_reader.peek() == quote) {
                m_reader.advance();
                return make_token(
                    quote == '"' ? Token::Kind::StrLiteral : Token::Kind::CharLiteral,
                    content,
                    start_location
                );
            }
            content += m_reader.advance();
        }

        return make_error(
            errors::unterminated(std::string(quote == '"' ? "string" : "character") + " literal"),
            start_location
        );
    }
    auto Lexer::lex_multiline_string(SourceLocation start_location) -> LexResult<Token> {
        std::string content;

        uint32_t char_count = 0;

        while (!m_reader.is_at_end()) {
            if (++char_count > MAX_STRING_LENGTH) {
                return make_error(errors::exceeded("maximum string literal length"));
            }

            if (m_reader.peek() == '\\') {
                if (auto result = process_escape_append(content, '"'); !result) {
                    return std::unexpected(std::move(result).error());
                }
                continue;
            }
            if (m_reader.peek() == '"' && m_reader.peek_next() == '"'
                && m_reader.peek_at(2) == '"') {
                m_reader.advance_by(3);
                return make_token(Token::Kind::StrLiteral, content, start_location);
            }

            content += m_reader.advance();
        }

        return make_error(errors::unterminated("string literal"), start_location);
    }
    auto Lexer::lex_newline() -> LexResult<Token> {
        auto start_location = m_reader.location();
        m_reader.advance();

        m_at_line_start = true;
        if (m_reader.peek_back(2) == '\r' && m_reader.peek() == '\n') {
            return make_token(Token::Kind::Newline, "\\r\\n", start_location);
        }
        return make_token(Token::Kind::Newline, "\\n", start_location);
    }

    auto Lexer::process_line_start() -> LexResult<Token> {
        count_leading_whitespace();

        if (m_reader.peek() != '#' && m_reader.peek() != '\n' && m_reader.peek() != '\r') {
            if (m_current_indent > m_indent_stack.back()) {
                return handle_indent(m_current_indent);
            }
            if (m_current_indent < m_indent_stack.back()) {
                return handle_dedent(m_current_indent);
            }
            m_at_line_start = false;
        }
        return std::unexpected(make_none().error());
    }
    auto Lexer::process_comments() -> LexResult<Token> {
        while (m_reader.peek() == '#') {
            skip_comments();

            if (m_reader.peek() == '\n' || m_reader.peek() == '\r') {
                return lex_newline();
            }

            count_leading_whitespace();
        }

        return lex_token();
    }
    auto Lexer::process_character(char symbol) -> LexResult<Token> {
        switch (symbol) {
            case '\n':
            case '\r':
                return lex_newline();
            case ' ':
            case '\t':
                count_leading_whitespace();
                return lex_token();
            case '"':
            case '\'':
            case '\\':
                return process_string();
            default:
                if (m_reader.is_at_end()) {
                    return make_none();
                }
                if (is_digit(symbol)) {
                    return process_number();
                }
                if (is_identifier_start(symbol)) {
                    return lex_identifier();
                }
                return lex_symbol();
        }
    }
    auto Lexer::process_number(bool negated) -> LexResult<Token> {
        auto start_location = m_reader.location();
        std::string number;

        if (negated) {
            number = "-";
        }

        while (!m_reader.is_at_end() && is_digit(m_reader.peek())) {
            number += m_reader.advance();
        }
        if (number.empty() || number == "-") {
            m_reader.advance();
            return make_error(errors::invalid("numeric literal"), start_location);
        }
        if (number == "-0" || number == "0") {
            auto next_char = static_cast<char>(std::tolower(m_reader.peek()));
            if (next_char == 'x' || next_char == 'b' || next_char == 'o') {
                auto radix_char = next_char;
                number += m_reader.advance();
                if (!is_digit(m_reader.peek())
                    && (radix_char != 'x' || !is_xdigit(m_reader.peek()))) {
                    return make_error(errors::invalid("numeric literal"), start_location);
                }
                number += m_reader.advance();
                return handle_radix_number(number, radix_char);
            }
        }

        return lex_number(number, start_location);
    }
    auto Lexer::process_string() -> LexResult<Token> {
        auto start_location = m_reader.location();

        auto quote = m_reader.advance();
        if (quote == '"' && m_reader.peek() == '"' && m_reader.peek_next() == '"') {
            m_reader.advance_by(2);
            return lex_multiline_string(start_location);
        }
        return lex_string(quote, start_location);
    }
    auto Lexer::process_escape() -> LexResult<std::string> {
        m_reader.advance();

        auto start_location = m_reader.location();

        if (m_reader.is_at_end()) {
            return make_error(std::format("escape sequence '\\{}' at end of file", m_reader.peek())
            );
        }

        switch (std::tolower(m_reader.advance())) {
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
                return make_error(errors::invalid_in("escape sequence", "literal"), start_location);
        }
    }
    auto Lexer::process_escape_char(char quote) -> LexResult<std::string> {
        auto escaped_string = process_escape();
        if (!escaped_string) {
            while (!m_reader.is_at_end() && m_reader.peek() != quote && m_reader.peek() != '\n') {
                m_reader.advance();
            }
            if (m_reader.peek() == quote) {
                m_reader.advance();
            }
            return std::unexpected(std::move(escaped_string).error());
        }
        return *escaped_string;
    }
    auto Lexer::process_escape_append(std::string& content, char quote) -> LexResult<void> {
        auto escaped_string = process_escape_char(quote);
        if (!escaped_string) {
            return std::unexpected(std::move(escaped_string).error());
        }
        content += *escaped_string;
        return {};
    }

    auto Lexer::handle_radix_number(std::string& number, char radix) -> LexResult<Token> {
        while (!m_reader.is_at_end() && (is_alnum(m_reader.peek()) || m_reader.peek() == '_')) {
            auto error_location = m_reader.location();

            if (m_reader.peek() == '_') {
                number += m_reader.advance();
                continue;
            }

            auto is_valid_digit = [radix, this]() -> bool {
                switch (radix) {
                    case 'x':
                        return is_xdigit(m_reader.peek());
                    case 'b':
                        return m_reader.peek() == '0' || m_reader.peek() == '1';
                    case 'o':
                        return m_reader.peek() >= '0' && m_reader.peek() <= '7';
                    default:
                        return false;
                }
            };
            if (!is_valid_digit()) {
                auto radix_kind = [radix]() -> std::string_view {
                    switch (radix) {
                        case 'x':
                            return "hexadecimal";
                        case 'b':
                            return "binary";
                        case 'o':
                            return "octal";
                        default:
                            return "unknown";
                    }
                }();

                return make_error(
                    errors::invalid_in(std::format("{} digit", radix_kind), "integer literal"),
                    error_location
                );
            }

            number += m_reader.advance();
        }

        return make_token(Token::Kind::IntLiteral, number);
    }
    auto Lexer::handle_indentation(uint32_t spaces) -> bool {
        if (!m_indent_width && spaces > 0) {
            m_indent_width = spaces;
            return true;
        }
        if (m_indent_width && spaces > 0) {
            return spaces % *m_indent_width == 0;
        }
        return true;
    }
    auto Lexer::handle_indent(uint32_t spaces) -> LexResult<Token> {
        auto start_location = SourceLocation(
            m_reader.location().filename(),
            { .line = m_reader.location().line(), .column = 1 },
            m_reader.location().offset() - spaces
        );

        if (!handle_indentation(spaces)) {
            return make_error(
                std::format("indentation of {} must be multiple of {}", spaces, *m_indent_width),
                start_location
            );
        }

        m_indent_stack.push_back(spaces);
        m_at_line_start = false;
        return make_token(Token::Kind::Indent, "<INDENT>", start_location);
    }
    auto Lexer::handle_dedent(uint32_t spaces) -> LexResult<Token> {
        auto start_location = m_reader.location();

        if (spaces >= m_indent_stack.back()) {
            m_at_line_start = false;
            return lex_token();
        }

        if (!handle_dedent_level(spaces)) {
            return make_error(errors::mismatched("indentation"), start_location);
        }
        emit_dedent_token(spaces, start_location);

        if (!m_pending_tokens.empty()) {
            auto pending_token = m_pending_tokens.front();
            m_pending_tokens.pop();
            m_at_line_start = false;
            return pending_token;
        }

        return make_error(errors::unknown("dedent processing error"), start_location);
    }
    auto Lexer::handle_dedent_level(uint32_t spaces) -> bool {
        return std::ranges::any_of(m_indent_stack, [spaces](auto indent) {
            return indent == spaces;
        });
    }

    auto Lexer::match_operator_sequence(
        std::span<const std::pair<Token::Kind, std::string_view>> patterns
    ) -> LexResult<Token> {
        auto current_position = m_reader.location().offset() - 1;

        auto content = m_reader.source().content();

        for (const auto& [kind, pattern] : patterns) {
            auto pattern_length = pattern.length();
            if (current_position + pattern_length > content.length()) {
                continue;
            }

            auto current_slice = content.substr(current_position, pattern_length);
            if (current_slice == pattern) {
                m_reader.advance_by(static_cast<uint32_t>(pattern_length - 1));
                return make_token(kind, pattern);
            }
        }

        return make_error(errors::invalid("operator sequence"));
    }

    auto Lexer::skip_comments() -> void {
        while (!m_reader.is_at_end() && m_reader.peek() != '\n') {
            m_reader.advance();
        }
    }
    auto Lexer::count_leading_whitespace() -> void {
        if (m_reader.peek() == '\f') {
            m_reader.advance();
        }

        uint32_t spaces = 0;
        auto has_tabs = false;
        auto has_spaces = false;
        auto start_location = m_reader.location();

        while (m_reader.peek() == ' ' || m_reader.peek() == '\t') {
            if (m_reader.peek() == '\t') {
                spaces += TAB_WIDTH - (spaces % TAB_WIDTH);
                has_tabs = true;
            } else {
                spaces++;
                has_spaces = true;
            }
            m_reader.advance();
        }

        if (has_tabs && has_spaces) {
            auto diagnostic = Diagnostic(
                DiagnosticSeverity::Error,
                errors::mixed("tabs and spaces"),
                SourceSpan { start_location,
                             m_reader.location().offset() - start_location.offset() }
            );
            m_diagnostics.get().emit_error(diagnostic.span(), std::string(diagnostic.message()));
        }

        m_current_indent = spaces;
    }
    auto Lexer::emit_dedent_token(uint32_t spaces, SourceLocation location) -> void {
        while (!m_indent_stack.empty() && m_indent_stack.back() > spaces) {
            m_indent_stack.pop_back();
            m_pending_tokens.push(make_token(Token::Kind::Dedent, "<DEDENT>", location));
        }
    }

    auto Lexer::sync() -> void {
        if (m_reader.is_at_end()) {
            return;
        }

        if (sync_statement() || sync_keyword()) {
            return;
        }
        m_reader.advance();

        while (!m_reader.is_at_end()) {
            if (m_reader.peek() == '\n' || m_reader.peek() == '\r' || m_reader.peek() == ';') {
                m_reader.advance();
                m_at_line_start = true;
                m_current_indent = 0;
                break;
            }
            m_reader.advance();
        }

        while (!m_pending_tokens.empty()) {
            m_pending_tokens.pop();
        }
    }
    auto Lexer::sync_statement() -> bool {
        if (m_reader.peek() == '\n' || m_reader.peek() == '\r' || m_reader.peek() == ';') {
            m_reader.advance();

            if (m_reader.peek() == '\n' || m_reader.peek() == '\r') {
                m_at_line_start = true;
                m_current_indent = 0;
            }
            return true;
        }
        if (m_reader.peek() == '}' || m_reader.peek() == ')' || m_reader.peek() == ']') {
            m_reader.advance();
            return true;
        }
        if (m_reader.peek() == ':' && m_reader.peek_next() == '=') {
            m_reader.advance_by(2);
            return true;
        }
        return false;
    }
    auto Lexer::sync_keyword() -> bool {
        static const auto keywords = {
            magic_enum::enum_name(Token::Kind::Do),     magic_enum::enum_name(Token::Kind::Else),
            magic_enum::enum_name(Token::Kind::For),    magic_enum::enum_name(Token::Kind::Func),
            magic_enum::enum_name(Token::Kind::If),     magic_enum::enum_name(Token::Kind::Let),
            magic_enum::enum_name(Token::Kind::Loop),   magic_enum::enum_name(Token::Kind::Proc),
            magic_enum::enum_name(Token::Kind::Return), magic_enum::enum_name(Token::Kind::Then),
            magic_enum::enum_name(Token::Kind::Unless), magic_enum::enum_name(Token::Kind::Until),
            magic_enum::enum_name(Token::Kind::Var),    magic_enum::enum_name(Token::Kind::While),
        };
        return std::ranges::any_of(keywords, [this](const auto& keyword) {
            return sync_keyword_match(to_lower_str(keyword));
        });
    }
    auto Lexer::sync_keyword_match(std::string_view keyword) -> bool {
        if (m_reader.location().offset() + keyword.length()
            > m_reader.source().content().length()) {
            return false;
        }

        auto content_slice =
            m_reader.source().content().substr(m_reader.location().offset(), keyword.length());
        if (content_slice == keyword) {
            if (m_reader.location().offset() + keyword.length()
                    >= m_reader.source().content().length()
                || !is_identifier_continue(m_reader.source().content(
                )[m_reader.location().offset() + keyword.length()])) {
                m_reader.advance_by(static_cast<uint32_t>(keyword.length()));
                return true;
            }
        }
        return false;
    }

    auto Lexer::make_none() -> std::unexpected<Diagnostic> {
        return std::unexpected<Diagnostic>(
            Diagnostic(DiagnosticSeverity::None, "", SourceSpan { m_reader.location(), 0 })
        );
    }
    auto Lexer::make_error(std::string_view message, Option<SourceLocation> location)
        -> std::unexpected<Diagnostic> {
        auto diagnostic = Diagnostic(
            DiagnosticSeverity::Error,
            std::string(message),
            SourceSpan { location.value_or(m_reader.location()), 0 }
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
            return { kind, lexeme, m_reader.location() };
        }

        auto start_location = location.value_or([&] {
            return SourceLocation(
                m_reader.location().filename(),
                m_reader.source().line_column(
                    static_cast<uint32_t>(m_reader.location().offset() - lexeme.length())
                ),
                static_cast<uint32_t>(lexeme.length())
            );
        }());

        return { kind, lexeme, start_location };
    }
}  // namespace musi
