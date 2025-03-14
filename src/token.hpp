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
            Class,
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
            Mod,
            Not,
            Or,
            Override,
            Proc,
            Public,
            Return,
            Sealed,
            Shared,
            Shl,
            Shr,
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
            Star,               // *
            Slash,              // /
            Bang,               // !
            Equals,             // =
            Greater,            // >
            Less,               // <
            Caret,              // ^
            Pipe,               // |
            MinusGreater,       // ->
            PipeGreater,        // |>
            GreaterEquals,      // >=
            LessEquals,         // <=
            LessEqualsGreater,  // <=>
            SlashEquals,        // /=
            ColonEquals,        // :=
            Colon,              // :
            Semicolon,          // ;
            Comma,              // ,
            Dot,                // .
            DotDot,             // .. (inclusive range)
            DotDotLess,         // ..< (exclusive range)
            Question,           // ?
            Underscore,         // _
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
