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
            BitwiseOr,   // (bitwise) or, nor, xor, xnor
            BitwiseAnd,  // (bitwise) and, nand
            Equality,    // = /=
            Comparison,  // < > <= <=> >=
            Shift,       // shl, shr, rol, ror
            Range,       // .. ..<
            Term,        // + -
            Factor,      // * / mod rem
            Power,       // ^
            Unary,       // - + not
            Call,        // . ()
            Primary,
        };

        using PrefixParseFn = ParseResult<ExpressionPtr> (Parser::*)();
        using InfixParseFn = ParseResult<ExpressionPtr> (Parser::*)(ExpressionPtr);

        static constexpr uint8_t MAX_BLOCK_DEPTH = 255;

        static auto is_sync_token(Token::Kind kind) -> bool {
            switch (kind) {
                case Token::Kind::Dedent:
                case Token::Kind::For:
                case Token::Kind::Func:
                case Token::Kind::If:
                case Token::Kind::Indent:
                case Token::Kind::LeftBrace:
                case Token::Kind::Let:
                case Token::Kind::Proc:
                case Token::Kind::Return:
                case Token::Kind::RightBrace:
                case Token::Kind::Unless:
                case Token::Kind::Var:
                case Token::Kind::While:
                    return true;
                default:
                    return false;
            }
        }
        static auto has_locations_on_same_line(
            const SourceLocation& first,
            const SourceLocation& second
        ) -> bool {
            return first.line() == second.line();
        }
        static auto is_violating_unary_spacing_rule(const Token& unary_op, const Token& operand)
            -> bool {
            if (unary_op.kind() == Token::Kind::Not) {
                return false;
            }
            return has_whitespace_after_unary(unary_op, operand);
        }
        static auto has_whitespace_after_unary(const Token& unary_op, const Token& operand)
            -> bool {
            if (unary_op.location().line() != operand.location().line()) {
                return true;
            }

            auto op_end = unary_op.location().column() + unary_op.lexeme().length();
            auto operand_start = operand.location().column();
            return op_end != operand_start;
        }
        static auto fetch_prefix_rule(Token::Kind kind) -> PrefixParseFn {
            switch (kind) {
                case Token::Kind::IntLiteral:
                case Token::Kind::RealLiteral:
                case Token::Kind::NatLiteral:
                case Token::Kind::StrLiteral:
                case Token::Kind::CharLiteral:
                case Token::Kind::BoolLiteral:
                    return &Parser::parse_literal_expression;
                case Token::Kind::Identifier:
                    return &Parser::parse_identifier_expression;
                case Token::Kind::LeftParen:
                    return &Parser::parse_grouping_expression;
                case Token::Kind::LeftBracket:
                    return &Parser::parse_array_expression;
                case Token::Kind::Plus:
                case Token::Kind::Minus:
                case Token::Kind::Not:
                    return &Parser::parse_unary_expression;
                case Token::Kind::Indent:
                    return &Parser::parse_block_expression;
                case Token::Kind::Break:
                    return &Parser::parse_break_expression;
                case Token::Kind::Continue:
                    return &Parser::parse_continue_expression;
                case Token::Kind::For:
                    return &Parser::parse_for_in_expression;
                case Token::Kind::If:
                    return &Parser::parse_if_expression;
                case Token::Kind::Loop:
                    return &Parser::parse_loop_expression;
                case Token::Kind::Return:
                    return &Parser::parse_return_expression;
                case Token::Kind::Unless:
                    return &Parser::parse_unless_expression;
                case Token::Kind::Until:
                    return &Parser::parse_until_expression;
                case Token::Kind::While:
                    return &Parser::parse_while_expression;
                case Token::Kind::Yield:
                    return &Parser::parse_yield_expression;
                default:
                    return nullptr;
            }
        }
        static auto fetch_infix_rule(Token::Kind kind) -> InfixParseFn {
            switch (kind) {
                case Token::Kind::LeftParen:
                    return &Parser::parse_call_expression;
                case Token::Kind::Dot:
                    return &Parser::parse_member_expression;
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
                case Token::Kind::Mod:
                case Token::Kind::Or:
                case Token::Kind::Rem:
                case Token::Kind::Rol:
                case Token::Kind::Ror:
                case Token::Kind::Shl:
                case Token::Kind::Shr:
                case Token::Kind::Xor:
                case Token::Kind::Xnor:
                case Token::Kind::Nor:
                case Token::Kind::Nand:
                case Token::Kind::DotDot:
                case Token::Kind::DotDotLess:
                    return &Parser::parse_binary_expression;
                case Token::Kind::ColonEquals:
                    return &Parser::parse_assignment_expression;
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

        explicit Parser(const Vec<Token>& tokens, DiagnosticEngine& diagnostics)
            : m_stream(tokens)
            , m_diagnostics(diagnostics) {}

        auto parse() -> ParseResult<Vec<Box<Node>>>;

    private:
        static auto make_quoted_literal_span(const Token& token) -> SourceSpan {
            auto quote_column = token.location().column();
            auto quote_offset = token.location().offset() - 1;

            auto quote_start = SourceLocation(
                token.location().filename(),
                LineColumn { .line = token.location().line(), .column = quote_column },
                quote_offset
            );
            auto total_length = token.lexeme().length() + 2;

            auto full_span = SourceSpan { quote_start, total_length };
            return full_span;
        }

        [[nodiscard]] auto fetch_precedence() const -> Precedence;
        [[nodiscard]] auto is_at_block_start() const -> bool {
            return m_stream.matches(Token::Kind::Indent)
                   || m_stream.matches(Token::Kind::LeftBrace);
        }
        [[nodiscard]] auto is_statement_terminated() const -> bool {
            return m_stream.matches(Token::Kind::Semicolon)
                   || m_stream.matches(Token::Kind::Newline)
                   || m_stream.matches(Token::Kind::Dedent) || m_stream.is_at_end();
        }
        [[nodiscard]] auto has_more_statements() const -> bool;
        [[nodiscard]] auto is_end_of_iterators() const -> bool {
            return m_stream.is_at_end() || m_stream.matches(Token::Kind::Loop)
                   || m_stream.matches(Token::Kind::Yield) || m_stream.matches(Token::Kind::Where)
                   || m_stream.matches(Token::Kind::Step) || m_stream.matches(Token::Kind::Dedent);
        }
        [[nodiscard]] auto is_binary_context() const -> bool {
            if (m_stream.position() == 0) {
                return false;
            }

            const auto& previous_token = m_stream.peek_back(1);
            return previous_token.kind() == Token::Kind::Identifier
                   || previous_token.kind() == Token::Kind::RightParen
                   || previous_token.kind() == Token::Kind::RightBracket
                   || previous_token.kind() == Token::Kind::RightBrace
                   || previous_token.kind() == Token::Kind::IntLiteral
                   || previous_token.kind() == Token::Kind::RealLiteral
                   || previous_token.kind() == Token::Kind::NatLiteral;
        }
        [[nodiscard]] auto is_conditional_context() const -> bool {
            if (m_stream.position() < 2) {
                return false;
            }

            const auto& previous_token = m_stream.peek_back(2);
            return previous_token.kind() == Token::Kind::If
                   || previous_token.kind() == Token::Kind::While
                   || previous_token.kind() == Token::Kind::Until
                   || previous_token.kind() == Token::Kind::Unless;
        }
        [[nodiscard]] auto is_logical_context() const -> bool {
            if (m_stream.position() < 2) {
                return false;
            }

            const auto& previous_token = m_stream.peek_back(2);
            return previous_token.kind() == Token::Kind::Equals
                   || previous_token.kind() == Token::Kind::SlashEquals
                   || previous_token.kind() == Token::Kind::Less
                   || previous_token.kind() == Token::Kind::LessEquals
                   || previous_token.kind() == Token::Kind::Greater
                   || previous_token.kind() == Token::Kind::GreaterEquals
                   || previous_token.kind() == Token::Kind::LessEqualsGreater
                   || previous_token.kind() == Token::Kind::Not;
        }
        [[nodiscard]] auto is_bitwise_context() const -> bool {
            return !is_logical_context();
        }

        auto parse_prefix() -> ParseResult<ExpressionPtr>;
        auto parse_infix(ExpressionPtr left) -> ParseResult<ExpressionPtr>;
        auto parse_postfix(ExpressionPtr left) -> ParseResult<ExpressionPtr>;

        auto parse_type_annotation(Token::Kind separator_kind) -> ParseResult<TypeAnnotationPtr>;

        auto parse_expression_or_block() -> ParseResult<ExpressionPtr>;
        auto parse_expression(Precedence precedence = Precedence::None)
            -> ParseResult<ExpressionPtr>;
        auto parse_literal_expression() -> ParseResult<ExpressionPtr>;
        auto parse_identifier_expression() -> ParseResult<ExpressionPtr>;
        auto parse_grouping_expression() -> ParseResult<ExpressionPtr>;
        auto parse_array_expression() -> ParseResult<ExpressionPtr>;
        auto parse_tuple_expression() -> ParseResult<ExpressionPtr>;
        auto parse_call_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr>;
        auto parse_member_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr>;
        auto parse_unary_expression() -> ParseResult<ExpressionPtr>;
        auto parse_binary_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr>;
        auto parse_range_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr>;
        auto parse_logical_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr>;
        auto parse_assignment_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr>;
        auto parse_block_expression() -> ParseResult<ExpressionPtr>;
        auto parse_block_expression_implicit() -> ParseResult<ExpressionPtr>;
        auto parse_break_expression() -> ParseResult<ExpressionPtr>;
        auto parse_continue_expression() -> ParseResult<ExpressionPtr>;
        auto parse_for_in_expression() -> ParseResult<ExpressionPtr>;
        auto parse_if_expression() -> ParseResult<ExpressionPtr>;
        auto parse_loop_expression() -> ParseResult<ExpressionPtr>;
        auto parse_return_expression() -> ParseResult<ExpressionPtr>;
        auto parse_unless_expression() -> ParseResult<ExpressionPtr>;
        auto parse_until_expression() -> ParseResult<ExpressionPtr>;
        auto parse_while_expression() -> ParseResult<ExpressionPtr>;
        auto parse_yield_expression() -> ParseResult<ExpressionPtr>;

        auto parse_statement() -> ParseResult<StatementPtr>;
        auto parse_expression_statement() -> ParseResult<StatementPtr>;
        auto parse_subprogram_declaration() -> ParseResult<StatementPtr>;
        auto parse_variable_declaration() -> ParseResult<StatementPtr>;

        auto parse_pattern() -> ParseResult<PatternPtr>;
        auto parse_tuple_pattern() -> ParseResult<PatternPtr>;
        auto parse_array_pattern() -> ParseResult<PatternPtr>;

        // parsing helpers
        auto parse_for_iterator() -> ParseResult<ExpressionPtr>;
        auto parse_for_in_clause() -> ParseResult<ExpressionPtr>;
        auto parse_for_body_or_yield(Vec<EnumeratorPtr>&& enumerators, Option<GuardPtr>&& guard)
            -> ParseResult<ExpressionPtr>;

        auto handle_unary_whitespace() -> ParseResult<void>;
        auto handle_consecutive_statements(const SourceLocation& expression_start)
            -> ParseResult<void>;
        auto handle_statement_termination() -> ParseResult<void>;
        auto handle_postfix_condition(ExpressionPtr& expression, const Token& postfix_token)
            -> ParseResult<void>;
        auto handle_then_branch() -> ParseResult<void>;
        auto handle_iterators(Vec<EnumeratorPtr>& enumerators, bool indented) -> ParseResult<void>;
        auto handle_optional_clauses(Option<GuardPtr>& guard) -> ParseResult<void>;
        auto handle_arguments(Vec<ExpressionPtr>& arguments) -> ParseResult<void>;
        auto handle_argument_separator() -> ParseResult<void>;
        auto handle_parameters(Vec<ParameterPtr>& parameters) -> ParseResult<void>;
        auto handle_array_elements(Vec<ExpressionPtr>& elements) -> ParseResult<void>;
        auto handle_tuple_elements(Vec<ExpressionPtr>& elements) -> ParseResult<void>;

        auto sync() -> void;
        auto sync_to_statement_boundary() -> bool;
        auto sync_to_declaration_boundary() -> bool;
        auto sync_to_block_boundary() -> bool;
        auto sync_to_expression_boundary() -> bool;

        auto make_error(std::string_view message, Option<SourceSpan> span = std::nullopt)
            -> std::unexpected<Diagnostic>;
        auto expect(Token::Kind kind, std::string_view message) -> ParseResult<Token>;
        auto expect_identifier(std::string_view message = "identifier") -> ParseResult<Token>;
        auto match_and_advance(Token::Kind kind, std::string_view error_message)
            -> ParseResult<Token> {
            if (!m_stream.matches(kind)) {
                return make_error(error_message);
            }
            return { m_stream.advance() };
        }

        TokenStream m_stream;
        uint32_t m_block_depth { 0 };
        Ref<DiagnosticEngine> m_diagnostics;
    };
}  // namespace musi
