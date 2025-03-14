#pragma once

#include <expected>

#include "diagnostics.hpp"
#include "node.hpp"
#include "token.hpp"

namespace musi {
    template<typename T>
    using ParseResult = Result<T, Diagnostic>;

    class Parser {
    public:
        enum class Precedence : uint8_t {
            None,
            Assignment,  // :=
            LogicalOr,   // or, xor
            LogicalAnd,  // and
            Equality,    // = /=
            Comparison,  // < > <= <=> >=
            Range,       // .. ..<
            Term,        // + -
            Factor,      // * / mod shl shr
            Power,       // ^
            Unary,       // not -
            Call,        // () [] .
            Primary
        };

        using PrefixParseFn = ParseResult<ExpressionPtr> (Parser::*)();
        using InfixParseFn = ParseResult<ExpressionPtr> (Parser::*)(ExpressionPtr);

        static constexpr uint8_t MAX_BLOCK_DEPTH = 255;

        explicit Parser(const Vec<Token>& tokens, DiagnosticEngine& diagnostics)
            : m_tokens(tokens)
            , m_diagnostics(diagnostics) {}

        [[nodiscard]] auto parse() -> ParseResult<Vec<Box<Node>>>;

    private:
        [[nodiscard]] static auto is_sync_token(Token::Kind kind) -> bool {
            switch (kind) {
                case Token::Kind::Let:
                case Token::Kind::Var:
                case Token::Kind::Func:
                case Token::Kind::Proc:
                case Token::Kind::If:
                case Token::Kind::Unless:
                case Token::Kind::While:
                case Token::Kind::For:
                case Token::Kind::Return:
                case Token::Kind::Indent:
                case Token::Kind::Dedent:
                case Token::Kind::LeftBrace:
                case Token::Kind::RightBrace:
                    return true;
                default:
                    return false;
            }
        }
        [[nodiscard]] static auto has_locations_on_same_line(
            const SourceLocation& first,
            const SourceLocation& second
        ) -> bool {
            return first.line() == second.line();
        }
        [[nodiscard]] static auto
            is_violating_unary_spacing_rule(const Token& unary_op, const Token& operand) -> bool {
            if (unary_op.kind() == Token::Kind::Not) {
                return false;
            }
            return has_whitespace_after_unary(unary_op, operand);
        }
        [[nodiscard]] static auto
            has_whitespace_after_unary(const Token& unary_op, const Token& operand) -> bool {
            if (unary_op.location().line() != operand.location().line()) {
                return true;
            }

            auto op_end = unary_op.location().column() + unary_op.lexeme().length();
            auto operand_start = operand.location().column();
            return op_end != operand_start;
        }
        [[nodiscard]] static auto fetch_prefix_rule(Token::Kind kind) -> PrefixParseFn {
            switch (kind) {
                case Token::Kind::LeftParen:
                    return &Parser::parse_grouping_expression;
                case Token::Kind::IntLiteral:
                case Token::Kind::RealLiteral:
                case Token::Kind::NatLiteral:
                case Token::Kind::StrLiteral:
                case Token::Kind::CharLiteral:
                case Token::Kind::BoolLiteral:
                    return &Parser::parse_literal_expression;
                case Token::Kind::Identifier:
                    return &Parser::parse_identifier_expression;
                case Token::Kind::Plus:
                case Token::Kind::Minus:
                case Token::Kind::Not:
                    return &Parser::parse_unary_expression;
                case Token::Kind::If:
                    return &Parser::parse_if_expression;
                case Token::Kind::Unless:
                    return &Parser::parse_unless_expression;
                case Token::Kind::While:
                    return &Parser::parse_while_expression;
                case Token::Kind::LeftBrace:
                case Token::Kind::Indent:
                    return &Parser::parse_block_expression;
                case Token::Kind::Return:
                    return &Parser::parse_return_expression;
                default:
                    return nullptr;
            }
        }
        [[nodiscard]] static auto fetch_infix_rule(Token::Kind kind) -> InfixParseFn {
            switch (kind) {
                case Token::Kind::Plus:
                case Token::Kind::Minus:
                case Token::Kind::Star:
                case Token::Kind::Slash:
                case Token::Kind::Less:
                case Token::Kind::LessEquals:
                case Token::Kind::Greater:
                case Token::Kind::GreaterEquals:
                case Token::Kind::LessEqualsGreater:
                case Token::Kind::Equals:
                case Token::Kind::SlashEquals:
                case Token::Kind::And:
                case Token::Kind::Or:
                    return &Parser::parse_binary_expression;
                case Token::Kind::LeftParen:
                    return &Parser::parse_call_expression;
                default:
                    return nullptr;
            }
        }

        static auto could_start_expression(Token::Kind kind) -> bool {
            return kind == Token::Kind::If || kind == Token::Kind::Unless
                   || kind == Token::Kind::While || kind == Token::Kind::For
                   || kind == Token::Kind::Return;
        }
        static auto could_end_expression(Token::Kind kind) -> bool {
            return kind == Token::Kind::RightParen || kind == Token::Kind::RightBracket
                   || kind == Token::Kind::RightBrace || kind == Token::Kind::Comma
                   || kind == Token::Kind::Colon || kind == Token::Kind::ColonEquals;
        }

        [[nodiscard]] auto peek_at(size_t offset) const -> const Token&;
        [[nodiscard]] auto peek() const -> const Token& {
            return peek_at(0);
        }
        [[nodiscard]] auto peek_next() const -> const Token&;
        [[nodiscard]] auto is_at_end() const -> bool {
            return peek().kind() == Token::Kind::Eof || m_current_position >= m_tokens.size();
        }
        [[nodiscard]] auto is_at_block_start() const -> bool;
        [[nodiscard]] auto is_statement_terminated() const -> bool;
        [[nodiscard]] auto has_more_statements() const -> bool;
        [[nodiscard]] auto matches(Token::Kind kind) const -> bool {
            if (is_at_end()) {
                return false;
            }
            return peek().kind() == kind;
        }
        [[nodiscard]] auto fetch_precedence() const -> Precedence;
        auto advance() -> const Token&;
        auto advance_by(uint32_t count) -> void;
        auto skip_newlines() -> void;

        auto sync() -> void;
        auto sync_to_statement_boundary() -> bool;
        auto sync_to_declaration_boundary() -> bool;
        auto sync_to_block_boundary() -> bool;
        auto sync_to_expression_boundary() -> bool;

        auto make_error(std::string_view message, Option<SourceSpan> span = std::nullopt)
            -> std::unexpected<Diagnostic>;
        auto expect(Token::Kind kind, std::string_view message) -> ParseResult<Token>;
        auto expect_identifier(std::string_view message = "identifier") -> ParseResult<Token>;

        auto validate_unary_whitespace() -> ParseResult<void>;
        auto validate_consecutive_statements(const SourceLocation& expression_start)
            -> ParseResult<void>;
        auto validate_statement_termination() -> ParseResult<void>;
        auto validate_postfix_condition(ExpressionPtr& expression, const Token& postfix_token)
            -> ParseResult<void>;
        auto validate_then_branch() -> ParseResult<void>;
        auto validate_arguments(Vec<ExpressionPtr>& arguments) -> ParseResult<void>;
        auto validate_parameters(Vec<Token>& parameters) -> ParseResult<void>;

        [[nodiscard]] auto match_and_advance(Token::Kind kind, std::string_view error_message)
            -> ParseResult<Token> {
            if (!matches(kind)) {
                return make_error(error_message);
            }
            return { advance() };
        }

        auto parse_expression(Precedence precedence = Precedence::None)
            -> ParseResult<ExpressionPtr>;
        auto parse_expression_or_block_expression() -> ParseResult<ExpressionPtr>;
        auto parse_statement() -> ParseResult<StatementPtr>;

        auto parse_prefix() -> ParseResult<ExpressionPtr>;
        auto parse_infix(ExpressionPtr left) -> ParseResult<ExpressionPtr>;
        auto parse_postfix(ExpressionPtr left) -> ParseResult<ExpressionPtr>;

        auto parse_grouping_expression() -> ParseResult<ExpressionPtr>;
        auto parse_literal_expression() -> ParseResult<ExpressionPtr>;
        auto parse_identifier_expression() -> ParseResult<ExpressionPtr>;
        auto parse_unary_expression() -> ParseResult<ExpressionPtr>;
        auto parse_if_expression() -> ParseResult<ExpressionPtr>;
        auto parse_while_expression() -> ParseResult<ExpressionPtr>;
        auto parse_unless_expression() -> ParseResult<ExpressionPtr>;
        auto parse_block_expression() -> ParseResult<ExpressionPtr>;
        auto parse_block_expression_implicit() -> ParseResult<ExpressionPtr>;
        auto parse_binary_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr>;
        auto parse_call_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr>;
        auto parse_return_expression() -> ParseResult<ExpressionPtr>;

        auto parse_expression_statement() -> ParseResult<StatementPtr>;

        auto parse_variable_declaration() -> ParseResult<DeclarationPtr>;
        auto parse_subprogram_declaration() -> ParseResult<DeclarationPtr>;

        Vec<Token> m_tokens;
        uint32_t m_current_position { 0 };
        uint32_t m_block_depth { 0 };
        Ref<DiagnosticEngine> m_diagnostics;
    };
}  // namespace musi
