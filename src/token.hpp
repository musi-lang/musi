#pragma once

#include <cstdint>
#include <format>
#include <magic_enum.hpp>
#include <string>

#include "common.hpp"
#include "location.hpp"

namespace musi {
    class Token {
    public:
        enum class Kind : uint8_t {
            Eof,

            /* layout */
            Newline,
            Indent,
            Dedent,

            /* identifiers */
            Identifier,

            /* keywords (alphabetically) */
            And,
            As,
            Async,
            Await,
            Break,
            Case,
            Continue,
            Do,
            Else,
            Enum,
            Export,
            For,
            From,
            Func,
            If,
            Import,
            In,
            Let,
            Loop,
            Mod,
            Nand,
            Nor,
            Not,
            Or,
            Override,
            Proc,
            Rem,
            Return,
            Rol,
            Ror,
            Sealed,
            Shared,
            Shl,
            Shr,
            Step,
            Struct,
            Then,
            Type,
            Unless,
            Unsafe,
            Until,
            Var,
            Weak,
            When,
            Where,
            While,
            Xnor,
            Xor,
            Yield,

            /* literals */
            IntLiteral,     // 123
            RealLiteral,    // 1.23e-4
            NatLiteral,     // 0N
            StrLiteral,     // "a", """a"""
            RawStrLiteral,  // r"a", r"""a"""
            CharLiteral,    // 'a'
            BoolLiteral,    // true, false

            /* symbols */
            LeftParen,          // (
            RightParen,         // )
            LeftBracket,        // [
            RightBracket,       // ]
            LeftBrace,          // {
            RightBrace,         // }
            Plus,               // +
            Minus,              // -
            MinusGreater,       // ->
            Star,               // *
            Slash,              // /
            SlashEquals,        // /=
            Caret,              // ^
            Pipe,               // |
            PipeGreater,        // |>
            Equals,             // =
            Less,               // <
            LessEquals,         // <=
            LessEqualsGreater,  // <=>
            Greater,            // >
            GreaterEquals,      // >=
            Bang,               // !
            Question,           // ?
            Colon,              // :
            ColonEquals,        // :=
            Semicolon,          // ;
            Comma,              // ,
            Dot,                // .
            DotDot,             // .. (inclusive range)
            DotDotLess,         // ..< (exclusive range)
            Hash,               // #
            Underscore,         // _
            At,                 // @
            Backtick,           // `
        };

        static constexpr auto map_keywords() -> Map<std::string, Kind> {
            Map<std::string, Kind> keyword_map;

            for (auto kind : magic_enum::enum_values<Kind>()) {
                if (kind >= Kind::And && kind <= Kind::Yield) {
                    auto name = std::string_view(magic_enum::enum_name(kind));
                    std::string lower_name;
                    for (auto name_ch : name) {
                        lower_name += static_cast<char>(std::tolower(name_ch));
                    }

                    keyword_map.emplace(lower_name, kind);
                }
            }

            return keyword_map;
        }

        Token(Kind kind, std::string_view lexeme, SourceLocation location)
            : m_kind(kind)
            , m_lexeme(lexeme)
            , m_location(location) {}

        auto operator==(Kind kind) const -> bool {
            return m_kind == kind;
        }
        auto operator!=(Kind kind) const -> bool {
            return m_kind != kind;
        }

        [[nodiscard]] auto kind() const -> Kind {
            return m_kind;
        }
        [[nodiscard]] auto lexeme() const -> std::string_view {
            return m_lexeme;
        }
        [[nodiscard]] auto location() const -> const SourceLocation& {
            return m_location;
        }
        [[nodiscard]] auto span() const -> SourceSpan {
            return { m_location, static_cast<uint32_t>(m_lexeme.length()) };
        }

        [[nodiscard]] auto to_json() const -> Json {
            return { { "kind", magic_enum::enum_name(m_kind) },
                     { "lexeme", m_lexeme },
                     { "location", m_location.to_json() } };
        }

    private:
        Kind m_kind;
        std::string m_lexeme;
        SourceLocation m_location;
    };
    class TokenStream {
    public:
        explicit TokenStream(const Vec<Token>& tokens) : m_tokens(tokens) {}

        [[nodiscard]] auto tokens() const -> const Vec<Token>& {
            return m_tokens;
        }
        [[nodiscard]] auto position() const -> uint32_t {
            return m_current_position;
        }
        [[nodiscard]] auto size() const -> uint32_t {
            return static_cast<uint32_t>(m_tokens.size());
        }

        [[nodiscard]] auto at(uint32_t index) const -> const Token& {
            return m_tokens.at(index);
        }
        [[nodiscard]] auto peek_at(uint32_t offset) const -> const Token& {
            return at(
                m_current_position + offset >= m_tokens.size() ? size() - 1
                                                               : m_current_position + offset
            );
        }
        [[nodiscard]] auto peek_back(uint32_t offset) const -> const Token& {
            return at(m_current_position < offset ? 0 : m_current_position - offset);
        }
        [[nodiscard]] auto peek() const -> const Token& {
            return peek_at(0);
        }
        [[nodiscard]] auto peek_next() const -> const Token& {
            return peek_at(1);
        }
        [[nodiscard]] auto is_at_end() const -> bool {
            return peek().kind() == Token::Kind::Eof || m_current_position >= m_tokens.size();
        }
        [[nodiscard]] auto matches(Token::Kind kind) const -> bool {
            if (is_at_end()) {
                return false;
            }
            return peek().kind() == kind;
        }

        auto advance() -> const Token&;
        auto advance_by(uint32_t count) -> void;
        auto skip_newlines() -> void;

    private:
        Vec<Token> m_tokens;
        uint32_t m_current_position { 0 };
    };
}  // namespace musi

template<>
struct std::formatter<musi::Token::Kind> {
    constexpr static auto parse(std::format_parse_context& ctx)
        -> std::format_parse_context::iterator {
        return ctx.begin();
    }
    static auto format(musi::Token::Kind kind, format_context& ctx)
        -> std::format_context::iterator {
        return std::format_to(ctx.out(), "{}", magic_enum::enum_name(kind));
    }
};

template<>
struct std::formatter<musi::Token> {
    constexpr static auto parse(std::format_parse_context& ctx)
        -> std::format_parse_context::iterator {
        return ctx.begin();
    }
    static auto format(const musi::Token& token, format_context& ctx)
        -> std::format_context::iterator {
        return std::format_to(ctx.out(), "{}", token.to_json().dump());
    }
};
