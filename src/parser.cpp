#include "parser.hpp"

#include "errors.hpp"
#include "node.hpp"

namespace musi {
    auto Parser::fetch_precedence() const -> Precedence {
        switch (m_stream.peek().kind()) {
            case Token::Kind::ColonEquals:
                return Precedence::Assignment;
            case Token::Kind::Nand:
            case Token::Kind::Nor:
            case Token::Kind::Or:
            case Token::Kind::Xor:
            case Token::Kind::Xnor:
                return Precedence::BitwiseOr;
            case Token::Kind::And:
                return Precedence::BitwiseAnd;
            case Token::Kind::Equals:
            case Token::Kind::SlashEquals:
                return Precedence::Equality;
            case Token::Kind::Less:
            case Token::Kind::LessEquals:
            case Token::Kind::Greater:
            case Token::Kind::GreaterEquals:
            case Token::Kind::LessEqualsGreater:
                return Precedence::Comparison;
            case Token::Kind::Shl:
            case Token::Kind::Shr:
            case Token::Kind::Rol:
            case Token::Kind::Ror:
                return Precedence::Shift;
            case Token::Kind::DotDot:
            case Token::Kind::DotDotLess:
                return Precedence::Range;
            case Token::Kind::Plus:
            case Token::Kind::Minus:
                return Precedence::Term;
            case Token::Kind::Star:
            case Token::Kind::Slash:
            case Token::Kind::Mod:
                return Precedence::Factor;
            case Token::Kind::Caret:
                return Precedence::Power;
            case Token::Kind::LeftParen:
            case Token::Kind::LeftBracket:
            case Token::Kind::Dot:
                return Precedence::Call;
            default:
                return Precedence::None;
        }
    }
    auto Parser::has_more_statements() const -> bool {
        if (m_stream.matches(Token::Kind::Dedent) || m_stream.matches(Token::Kind::Eof)) {
            return false;
        }
        if (m_stream.matches(Token::Kind::Newline)) {
            auto position = m_stream.position();
            uint32_t newline_count = 0;

            while (position < m_stream.size()
                   && m_stream.at(position).kind() == Token::Kind::Newline) {
                newline_count++;
                position++;
            }

            if (newline_count > 1) {
                return false;
            }
        }

        const auto next_kind = m_stream.peek_next().kind();
        return next_kind != Token::Kind::Dedent && next_kind != Token::Kind::Eof;
    }
    auto Parser::parse() -> ParseResult<Vec<Box<Node>>> {
        Vec<Box<Node>> nodes;
        Vec<Diagnostic> errors;
        uint32_t consecutive_errors = 0;

        m_stream.skip_newlines();
        while (!m_stream.is_at_end()) {
            auto start_position = m_stream.position();

            auto statement = parse_statement();
            if (!statement) {
                errors.push_back(std::move(statement).error());

                if (m_stream.position() == start_position) {
                    consecutive_errors++;
                    if (consecutive_errors > errors::MAX_CONSECUTIVE_ERRORS) {
                        return std::unexpected(Diagnostic(
                            DiagnosticSeverity::Error,
                            "too many syntactic errors, aborting...",
                            SourceSpan {}
                        ));
                    }
                } else {
                    consecutive_errors = 0;
                }
                continue;
            }

            consecutive_errors = 0;
            nodes.push_back(std::move(*statement));
            m_stream.skip_newlines();
        }

        if (!errors.empty() && nodes.empty()) {
            return std::unexpected(std::move(errors.front()));
        }

        return nodes;
    }

    auto Parser::parse_type_annotation(Token::Kind separator_kind)
        -> ParseResult<TypeAnnotationPtr> {
        auto op = m_stream.advance();

        auto type = parse_expression(Precedence::Primary);
        if (!type) {
            return make_error(errors::expected_after(
                "type",
                separator_kind == Token::Kind::Colon ? "':'" : "'->'"
            ));
        }
        return std::make_unique<TypeAnnotation>(std::move(*type));
    }

    auto Parser::parse_prefix() -> ParseResult<ExpressionPtr> {
        auto prefix_token = m_stream.peek();
        auto prefix_kind = prefix_token.kind();

        auto prefix_rule = fetch_prefix_rule(prefix_kind);
        if (prefix_rule == nullptr) {
            auto error = make_error(
                errors::unexpected(std::format("token '{}'", prefix_token.lexeme())),
                prefix_token.span()
            );
            sync();
            return error;
        }
        return (this->*prefix_rule)();
    }
    auto Parser::parse_infix(ExpressionPtr left) -> ParseResult<ExpressionPtr> {
        auto infix_kind = m_stream.peek().kind();

        auto infix_rule = fetch_infix_rule(infix_kind);
        if (infix_rule == nullptr) {
            return std::move(left);
        }
        return (this->*infix_rule)(std::move(left));
    }
    auto Parser::parse_postfix(ExpressionPtr left) -> ParseResult<ExpressionPtr> {
        const auto postfix_start = left->span().start();

        if (m_stream.matches(Token::Kind::If) || m_stream.matches(Token::Kind::Unless)
            || m_stream.matches(Token::Kind::While)) {
            auto postfix_token = m_stream.peek();
            if (has_locations_on_same_line(postfix_start, postfix_token.location())) {
                auto result = handle_postfix_condition(left, postfix_token);
                if (!result) {
                    return std::unexpected(std::move(result).error());
                }
            }
        }

        return std::move(left);
    }
    auto Parser::parse_for_iterator() -> ParseResult<ExpressionPtr> {
        auto iterator = expect_identifier("iterator name");
        if (!iterator) {
            return std::unexpected(std::move(iterator).error());
        }
        return std::make_unique<IdentifierExpression>(*iterator);
    }
    auto Parser::parse_for_in_clause() -> ParseResult<ExpressionPtr> {
        auto in_token =
            match_and_advance(Token::Kind::In, errors::expected_after("'in'", "iterator"));
        if (!in_token) {
            return std::unexpected(std::move(in_token).error());
        }

        auto in_clause = parse_expression();
        if (!in_clause) {
            return std::unexpected(std::move(in_clause).error());
        }
        return in_clause;
    }
    auto Parser::parse_for_body_or_yield(Vec<EnumeratorPtr>&& enumerators, Option<GuardPtr>&& guard)
        -> ParseResult<ExpressionPtr> {
        if (m_stream.matches(Token::Kind::Yield)) {
            m_stream.advance();
            m_stream.skip_newlines();

            auto expression = parse_expression();
            if (!expression) {
                return std::unexpected(std::move(expression).error());
            }
            auto yield_expression = std::make_unique<YieldExpression>(std::move(*expression));
            return std::make_unique<ForInExpression>(
                std::move(enumerators),
                std::move(guard),
                std::move(yield_expression),
                std::make_unique<BlockExpression>(Vec<StatementPtr> {})
            );
        }

        auto loop_token = match_and_advance(
            Token::Kind::Loop,
            errors::expected_after("'loop'", "'for' expression")
        );
        if (!loop_token) {
            return std::unexpected(std::move(loop_token).error());
        }
        m_stream.skip_newlines();

        auto body = parse_expression_or_block();
        if (!body) {
            return std::unexpected(std::move(body).error());
        }
        return std::make_unique<ForInExpression>(
            std::move(enumerators),
            std::move(guard),
            nullptr,
            std::move(*body)
        );
    }

    auto Parser::parse_expression_or_block() -> ParseResult<ExpressionPtr> {
        if (is_at_block_start()) {
            return parse_block_expression();
        }
        return parse_expression();
    }
    auto Parser::parse_expression(Precedence precedence) -> ParseResult<ExpressionPtr> {
        auto unary_result = handle_unary_whitespace();
        if (!unary_result) {
            sync();
            return std::unexpected(std::move(unary_result).error());
        }

        auto left = parse_prefix();
        if (!left) {
            sync();
            return std::unexpected(std::move(left).error());
        }

        while (precedence < fetch_precedence()) {
            auto infix_result = parse_infix(std::move(*left));
            if (!infix_result) {
                sync();
                return std::unexpected(std::move(infix_result).error());
            }
            left = std::move(infix_result);
        }

        return left;
    }
    auto Parser::parse_literal_expression() -> ParseResult<ExpressionPtr> {
        auto literal_token = m_stream.advance();
        if (literal_token.kind() == Token::Kind::CharLiteral
            && literal_token.lexeme().length() > 1) {
            return make_error(
                errors::multiple_in("characters", "character literal"),
                make_quoted_literal_span(literal_token)
            );
        }
        return std::make_unique<LiteralExpression>(literal_token);
    }
    auto Parser::parse_identifier_expression() -> ParseResult<ExpressionPtr> {
        return std::make_unique<IdentifierExpression>(m_stream.advance());
    }
    auto Parser::parse_grouping_expression() -> ParseResult<ExpressionPtr> {
        m_stream.advance();
        m_stream.skip_newlines();

        if (m_stream.matches(Token::Kind::RightParen)) {
            m_stream.advance();
            return std::make_unique<TupleExpression>(Vec<ExpressionPtr> {});
        }

        auto first = parse_expression();
        if (!first) {
            return std::unexpected(std::move(first).error());
        }

        m_stream.skip_newlines();
        if (m_stream.matches(Token::Kind::Comma)) {
            Vec<ExpressionPtr> elements;
            elements.push_back(std::move(*first));

            m_stream.advance();
            m_stream.skip_newlines();

            auto tuple_result = handle_tuple_elements(elements);
            if (!tuple_result) {
                return std::unexpected(std::move(tuple_result).error());
            }

            auto right_paren = match_and_advance(Token::Kind::RightParen, errors::expected("')'"));
            if (!right_paren) {
                return std::unexpected(std::move(right_paren).error());
            }

            return std::make_unique<TupleExpression>(std::move(elements));
        }

        auto right_paren = match_and_advance(Token::Kind::RightParen, errors::expected("')'"));
        if (!right_paren) {
            return std::unexpected(std::move(right_paren).error());
        }

        return first;
    }
    auto Parser::parse_array_expression() -> ParseResult<ExpressionPtr> {
        m_stream.advance();
        m_stream.skip_newlines();

        Vec<ExpressionPtr> elements;
        auto element_result = handle_array_elements(elements);
        if (!element_result) {
            return std::unexpected(std::move(element_result).error());
        }

        auto right_bracket = match_and_advance(Token::Kind::RightBracket, errors::expected("']'"));
        if (!right_bracket) {
            return std::unexpected(std::move(right_bracket).error());
        }

        return std::make_unique<ArrayExpression>(std::move(elements));
    }
    auto Parser::parse_tuple_expression() -> ParseResult<ExpressionPtr> {
        m_stream.advance();
        m_stream.skip_newlines();

        Vec<ExpressionPtr> elements;

        if (!m_stream.matches(Token::Kind::RightParen)) {
            while (!m_stream.is_at_end()) {
                auto element = parse_expression();
                if (!element) {
                    return std::unexpected(std::move(element).error());
                }
                elements.push_back(std::move(*element));

                if (m_stream.matches(Token::Kind::RightParen)) {
                    break;
                }

                if (!m_stream.matches(Token::Kind::Comma)) {
                    return make_error(errors::expected("','"));
                }
                m_stream.advance();
                m_stream.skip_newlines();
            }
        }

        auto right_paren = match_and_advance(Token::Kind::RightParen, errors::expected("')'"));
        if (!right_paren) {
            return std::unexpected(std::move(right_paren).error());
        }

        return std::make_unique<TupleExpression>(std::move(elements));
    }
    auto Parser::parse_call_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr> {
        auto left_paren = match_and_advance(
            Token::Kind::LeftParen,
            errors::expected_before("'('", "expression list")
        );
        if (!left_paren) {
            return std::unexpected(std::move(left_paren).error());
        }
        m_stream.skip_newlines();

        Vec<ExpressionPtr> arguments;
        auto argument_result = handle_arguments(arguments);
        if (!argument_result) {
            return std::unexpected(std::move(argument_result).error());
        }

        auto right_paren = match_and_advance(
            Token::Kind::RightParen,
            errors::expected_after("')'", "expression list")
        );
        if (!right_paren) {
            return std::unexpected(std::move(right_paren).error());
        }

        return std::make_unique<CallExpression>(std::move(left), std::move(arguments));
    }
    auto Parser::parse_member_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr> {
        auto member_op = m_stream.peek();
        m_stream.advance();
        m_stream.skip_newlines();

        if (m_stream.matches(Token::Kind::IntLiteral)) {
            auto property_token = m_stream.advance();

            auto lexeme = property_token.lexeme();
            if (lexeme.empty() || lexeme.front() == '-') {
                return make_error("index cannot be negative", property_token.span());
            }

            auto property = std::make_unique<LiteralExpression>(property_token);
            return std::make_unique<MemberExpression>(
                member_op,
                std::move(left),
                std::move(property)
            );
        }

        auto member = expect_identifier("member name");
        if (!member) {
            return std::unexpected(std::move(member).error());
        }

        auto expression = std::make_unique<IdentifierExpression>(*member);
        return std::make_unique<MemberExpression>(
            member_op,
            std::move(left),
            std::move(expression)
        );
    }
    auto Parser::parse_unary_expression() -> ParseResult<ExpressionPtr> {
        auto unary_op = m_stream.peek();
        if (unary_op.kind() == Token::Kind::Not) {
            m_stream.advance();

            auto operand = parse_expression(Precedence::Unary);
            if (!operand) {
                return std::unexpected(std::move(operand).error());
            }
            return std::make_unique<UnaryExpression>(unary_op, std::move(*operand));
        }

        auto next_token = m_stream.peek_next();
        if (next_token.kind() != Token::Kind::Eof
            && is_violating_unary_spacing_rule(unary_op, next_token)) {
            auto span = SourceSpan::merge(unary_op.span().start(), next_token.span().start());
            return make_error("unary operator cannot be separated from its operand", span);
        }
        m_stream.advance();

        auto operand = parse_expression(Precedence::Unary);
        if (!operand) {
            return std::unexpected(std::move(operand).error());
        }
        return std::make_unique<UnaryExpression>(unary_op, std::move(*operand));
    }
    auto Parser::parse_binary_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr> {
        auto binary_op = m_stream.advance();
        if (binary_op.kind() == Token::Kind::DotDot
            || binary_op.kind() == Token::Kind::DotDotLess) {
            return parse_range_expression(std::move(left));
        }

        auto precedence = fetch_precedence();
        auto right = parse_expression(precedence);

        if (!right) {
            return std::unexpected(std::move(right).error());
        }

        return std::make_unique<BinaryExpression>(binary_op, std::move(left), std::move(*right));
    }
    auto Parser::parse_range_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr> {
        auto range_op = m_stream.peek_back(1);

        auto right = parse_expression(Precedence::Range);
        if (!right) {
            return std::unexpected(std::move(right).error());
        }
        return std::make_unique<RangeExpression>(range_op, std::move(left), std::move(*right));
    }
    auto Parser::parse_assignment_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr> {
        auto colon_equals = m_stream.advance();

        auto value = parse_expression(Precedence::Assignment);
        if (!value) {
            return make_error(errors::expected_after("expression", "':='"));
        }
        return std::make_unique<AssignmentExpression>(
            std::move(colon_equals),
            std::move(left),
            std::move(*value)
        );
    }
    auto Parser::parse_block_expression() -> ParseResult<ExpressionPtr> {
        if (m_block_depth >= MAX_BLOCK_DEPTH) {
            return make_error(errors::exceeded("maximum block nesting depth"));
        }
        m_block_depth++;

        m_stream.skip_newlines();
        auto indent = match_and_advance(
            Token::Kind::Indent,
            errors::expected_at("indentation", "start of block")
        );
        if (!indent) {
            m_block_depth--;
            return std::unexpected(std::move(indent).error());
        }
        m_stream.skip_newlines();

        Vec<StatementPtr> statements;
        while (!m_stream.is_at_end()) {
            if (m_stream.matches(Token::Kind::Dedent)) {
                break;
            }

            auto statement = parse_statement();
            if (!statement) {
                m_block_depth--;
                return std::unexpected(std::move(statement).error());
            }
            statements.push_back(std::move(*statement));
        }

        auto dedent = match_and_advance(
            Token::Kind::Dedent,
            errors::expected_at("dedentation", "end of block")
        );
        if (!dedent) {
            m_block_depth--;
            return std::unexpected(std::move(dedent).error());
        }

        m_block_depth--;
        return std::make_unique<BlockExpression>(std::move(statements));
    }
    auto Parser::parse_block_expression_implicit() -> ParseResult<ExpressionPtr> {
        if (m_block_depth >= MAX_BLOCK_DEPTH) {
            return make_error(errors::exceeded("maximum block nesting depth"));
        }
        m_block_depth++;

        Vec<StatementPtr> statements;
        while (!m_stream.is_at_end() && !m_stream.matches(Token::Kind::Newline)) {
            auto statement = parse_statement();
            if (!statement) {
                m_block_depth--;
                return std::unexpected(std::move(statement).error());
            }
            statements.push_back(std::move(*statement));

            if (m_stream.matches(Token::Kind::Semicolon)) {
                m_stream.advance();
                m_stream.skip_newlines();
            }
        }

        m_block_depth--;
        return std::make_unique<BlockExpression>(std::move(statements));
    }
    auto Parser::parse_break_expression() -> ParseResult<ExpressionPtr> {
        auto break_token = m_stream.advance();
        return std::make_unique<BreakExpression>(break_token.span());
    }
    auto Parser::parse_continue_expression() -> ParseResult<ExpressionPtr> {
        auto continue_token = m_stream.advance();
        return std::make_unique<ContinueExpression>(continue_token.span());
    }
    auto Parser::parse_for_in_expression() -> ParseResult<ExpressionPtr> {
        m_stream.advance();
        m_stream.skip_newlines();

        Vec<EnumeratorPtr> enumerators;
        Option<GuardPtr> guard;

        bool indented = m_stream.matches(Token::Kind::Indent);
        if (indented) {
            m_stream.advance();
            m_stream.skip_newlines();
        }

        auto iterators_result = handle_iterators(enumerators, indented);
        if (!iterators_result) {
            return std::unexpected(std::move(iterators_result).error());
        }

        if (m_stream.matches(Token::Kind::Where)) {
            auto clauses_result = handle_optional_clauses(guard);
            if (!clauses_result) {
                return std::unexpected(std::move(clauses_result).error());
            }
        }

        if (indented) {
            if (!m_stream.matches(Token::Kind::Dedent)) {
                return make_error(errors::expected_after("dedentation", "'for' expression"));
            }
            m_stream.advance();
            m_stream.skip_newlines();
        }

        return parse_for_body_or_yield(std::move(enumerators), std::move(guard));
    }
    auto Parser::parse_if_expression() -> ParseResult<ExpressionPtr> {
        m_stream.advance();

        auto condition = parse_expression();
        if (!condition) {
            return std::unexpected(std::move(condition).error());
        }

        m_stream.skip_newlines();
        auto then_result = handle_then_branch();
        if (!then_result) {
            return std::unexpected(std::move(then_result).error());
        }

        auto then_branch = parse_expression_or_block();
        if (!then_branch) {
            return std::unexpected(std::move(then_branch).error());
        }

        m_stream.skip_newlines();
        ExpressionPtr else_branch = nullptr;
        if (m_stream.matches(Token::Kind::Else)) {
            m_stream.advance();
            m_stream.skip_newlines();

            if (m_stream.matches(Token::Kind::If)) {
                auto nested_if = parse_if_expression();
                if (!nested_if) {
                    return std::unexpected(std::move(nested_if).error());
                }
                else_branch = std::move(*nested_if);
            } else {
                auto expression = parse_expression_or_block();
                if (!expression) {
                    return std::unexpected(std::move(expression).error());
                }
                else_branch = std::move(*expression);
            }
        }

        return std::make_unique<IfExpression>(
            std::move(*condition),
            std::move(*then_branch),
            std::move(else_branch)
        );
    }
    auto Parser::parse_loop_expression() -> ParseResult<ExpressionPtr> {
        m_stream.advance();
        m_stream.skip_newlines();

        auto body = parse_expression_or_block();
        if (!body) {
            return std::unexpected(std::move(body).error());
        }
        return std::make_unique<LoopExpression>(std::move(*body));
    }
    auto Parser::parse_return_expression() -> ParseResult<ExpressionPtr> {
        m_stream.advance();

        if (is_statement_terminated() || m_stream.matches(Token::Kind::RightBrace)
            || m_stream.matches(Token::Kind::Dedent) || m_stream.matches(Token::Kind::Eof)) {
            return std::make_unique<ReturnExpression>(nullptr);
        }

        auto value = parse_expression();
        if (!value) {
            return std::unexpected(std::move(value).error());
        }
        return std::make_unique<ReturnExpression>(std::move(*value));
    }
    auto Parser::parse_unless_expression() -> ParseResult<ExpressionPtr> {
        m_stream.advance();

        auto condition = parse_expression();
        if (!condition) {
            return std::unexpected(std::move(condition).error());
        }

        m_stream.skip_newlines();
        if (m_stream.matches(Token::Kind::Then)) {
            auto then_token = match_and_advance(
                Token::Kind::Then,
                errors::expected_after("'then'", "'unless' condition")
            );
            if (!then_token) {
                return std::unexpected(std::move(then_token).error());
            }
            m_stream.skip_newlines();

            auto body = parse_expression_or_block();
            if (!body) {
                return std::unexpected(std::move(body).error());
            }
            return std::make_unique<UnlessExpression>(std::move(*condition), std::move(*body));
        }

        auto true_literal = std::make_unique<LiteralExpression>(
            Token(Token::Kind::BoolLiteral, "true", (*condition)->span().start())
        );
        return std::make_unique<UnlessExpression>(std::move(*condition), std::move(true_literal));
    }
    auto Parser::parse_until_expression() -> ParseResult<ExpressionPtr> {
        m_stream.advance();

        auto condition = parse_expression();
        if (!condition) {
            return std::unexpected(std::move(condition).error());
        }

        auto loop_token = match_and_advance(
            Token::Kind::Loop,
            errors::expected_after("'loop'", "'until' condition")
        );
        if (!loop_token) {
            return std::unexpected(std::move(loop_token).error());
        }
        m_stream.skip_newlines();

        auto body = parse_expression_or_block();
        if (!body) {
            return std::unexpected(std::move(body).error());
        }
        return std::make_unique<UntilExpression>(std::move(*condition), std::move(*body));
    }
    auto Parser::parse_while_expression() -> ParseResult<ExpressionPtr> {
        m_stream.advance();

        auto condition = parse_expression();
        if (!condition) {
            return std::unexpected(std::move(condition).error());
        }

        auto loop_token = match_and_advance(
            Token::Kind::Loop,
            errors::expected_after("'loop'", "'while' condition")
        );
        if (!loop_token) {
            return std::unexpected(std::move(loop_token).error());
        }
        m_stream.skip_newlines();

        auto body = parse_expression_or_block();
        if (!body) {
            return std::unexpected(std::move(body).error());
        }
        return std::make_unique<WhileExpression>(std::move(*condition), std::move(*body));
    }
    auto Parser::parse_yield_expression() -> ParseResult<ExpressionPtr> {
        m_stream.advance();
        m_stream.skip_newlines();

        auto value = parse_expression();
        if (!value) {
            return std::unexpected(std::move(value).error());
        }
        return std::make_unique<YieldExpression>(std::move(*value));
    }

    auto Parser::parse_statement() -> ParseResult<StatementPtr> {
        if (m_stream.matches(Token::Kind::Dedent) || m_stream.matches(Token::Kind::Eof)) {
            return make_error(errors::expected("expression"));
        }
        m_stream.skip_newlines();

        auto statement = [&]() -> ParseResult<StatementPtr> {
            switch (m_stream.peek().kind()) {
                case Token::Kind::Func:
                case Token::Kind::Proc:
                    return parse_subprogram_declaration();
                case Token::Kind::Let:
                case Token::Kind::Var:
                    return parse_variable_declaration();
                default:
                    return parse_expression_statement();
            }
        }();
        if (!statement) {
            auto error_position = m_stream.position();

            sync();
            if (m_stream.position() == error_position && !m_stream.is_at_end()) {
                m_stream.advance();
            }
            return std::unexpected(std::move(statement).error());
        }
        return statement;
    }
    auto Parser::parse_expression_statement() -> ParseResult<StatementPtr> {
        auto expression = parse_expression();
        if (!expression) {
            return std::unexpected(std::move(expression).error());
        }

        auto postfix = parse_postfix(std::move(*expression));
        if (!postfix) {
            return std::unexpected(std::move(postfix).error());
        }

        const auto postfix_start = (*postfix)->span().start();

        if (!is_statement_terminated() && !m_stream.is_at_end() && m_block_depth == 0) {
            auto consec_result = handle_consecutive_statements(postfix_start);
            if (!consec_result) {
                return std::unexpected(std::move(consec_result).error());
            }
        }

        if (m_block_depth > 0) {
            if (m_stream.matches(Token::Kind::Newline)
                || m_stream.matches(Token::Kind::Semicolon)) {
                m_stream.advance();
                m_stream.skip_newlines();
            } else if (!m_stream.matches(Token::Kind::Dedent) && !m_stream.matches(Token::Kind::Eof)
                       && has_more_statements()) {
                const auto& next_token = m_stream.peek();
                if (has_locations_on_same_line(postfix_start, next_token.location())) {
                    return make_error(
                        "consecutive statements on same line must be separated by ';'",
                        next_token.span()
                    );
                }
            }
        } else if (has_more_statements()) {
            bool handled = false;
            if (m_stream.matches(Token::Kind::Newline)
                || m_stream.matches(Token::Kind::Semicolon)) {
                m_stream.advance();
                m_stream.skip_newlines();
                handled = true;
            }
            if (!handled) {
                const auto& next_token = m_stream.peek();
                if (has_locations_on_same_line(postfix_start, next_token.location())) {
                    return make_error("consecutive statements at top level must be separated by ';'"
                    );
                }
            }
        }

        return std::make_unique<ExpressionStatement>(std::move(*postfix));
    }
    auto Parser::parse_subprogram_declaration() -> ParseResult<StatementPtr> {
        bool has_return_type = m_stream.matches(Token::Kind::Func);
        Token kind_token(
            has_return_type ? Token::Kind::Func : Token::Kind::Proc,
            has_return_type ? "func" : "proc",
            m_stream.peek().location()
        );
        m_stream.advance();

        auto name_token =
            expect_identifier(std::format("{} name", has_return_type ? "function" : "procedure"));
        if (!name_token) {
            return std::unexpected(std::move(name_token).error());
        }
        auto name = std::make_unique<IdentifierExpression>(*name_token);

        auto left_paren = match_and_advance(
            Token::Kind::LeftParen,
            errors::expected_after(
                "'('",
                std::format("{} name", has_return_type ? "function" : "procedure")
            )
        );
        if (!left_paren) {
            return std::unexpected(std::move(left_paren).error());
        }

        Vec<ParameterPtr> parameters;
        if (!m_stream.matches(Token::Kind::RightParen)) {
            auto parameter_result = handle_parameters(parameters);
            if (!parameter_result) {
                return std::unexpected(std::move(parameter_result).error());
            }
        }

        auto right_paren = match_and_advance(
            Token::Kind::RightParen,
            errors::expected_after("')'", "parameter list")
        );
        if (!right_paren) {
            return std::unexpected(std::move(right_paren).error());
        }

        Option<TypeAnnotationPtr> return_type;
        if (has_return_type) {
            if (!m_stream.matches(Token::Kind::MinusGreater)) {
                return make_error(errors::expected_after("'->'", "parameter list"));
            }

            auto type = parse_type_annotation(Token::Kind::MinusGreater);
            if (!type) {
                return std::unexpected(std::move(type).error());
            }
            return_type = std::move(*type);
        }

        auto colon_equals = match_and_advance(
            Token::Kind::ColonEquals,
            errors::expected_after("':='", has_return_type ? "return type" : "parameter list")
        );
        if (!colon_equals) {
            return std::unexpected(std::move(colon_equals).error());
        }
        m_stream.skip_newlines();

        auto body =
            is_at_block_start() ? parse_block_expression() : parse_block_expression_implicit();
        if (!body) {
            return std::unexpected(std::move(body).error());
        }
        return std::make_unique<SubprogramDeclaration>(
            kind_token,
            std::move(name),
            std::move(parameters),
            std::move(return_type),
            std::move(*body)
        );
    }
    auto Parser::parse_variable_declaration() -> ParseResult<StatementPtr> {
        bool reassignable = m_stream.matches(Token::Kind::Var);
        Token kind_token(
            reassignable ? Token::Kind::Var : Token::Kind::Let,
            reassignable ? "var" : "let",
            m_stream.peek().location()
        );
        m_stream.advance();

        Vec<Box<VariableDeclarator>> declarations;

        while (!m_stream.is_at_end()) {
            auto name = parse_identifier_expression();
            if (!name) {
                return std::unexpected(std::move(name).error());
            }

            Option<TypeAnnotationPtr> type_annotation;
            if (m_stream.matches(Token::Kind::Colon)) {
                auto type = parse_type_annotation(Token::Kind::Colon);
                if (!type) {
                    return std::unexpected(std::move(type).error());
                }
                type_annotation = std::move(*type);
            }

            auto colon_equals = match_and_advance(
                Token::Kind::ColonEquals,
                errors::expected_after(
                    "':='",
                    type_annotation ? "type annotation" : "variable name"
                )
            );
            if (!colon_equals) {
                return std::unexpected(std::move(colon_equals).error());
            }

            auto initial_value = parse_expression();
            if (!initial_value) {
                return make_error(errors::expected_after("initial value", "':='"));
            }

            declarations.push_back(std::make_unique<VariableDeclarator>(
                std::move(*name),
                std::move(type_annotation),
                std::move(*initial_value)
            ));

            if (!m_stream.matches(Token::Kind::Comma)) {
                break;
            }
            m_stream.advance();
            m_stream.skip_newlines();
        }

        return std::make_unique<VariableDeclaration>(kind_token, std::move(declarations));
    }

    auto Parser::parse_pattern() -> ParseResult<PatternPtr> {
        switch (m_stream.peek().kind()) {
            case Token::Kind::LeftParen:
                return parse_tuple_pattern();
            case Token::Kind::LeftBracket:
                return parse_array_pattern();
            default:
                return make_error(errors::expected("pattern"));
        }
    }
    auto Parser::parse_tuple_pattern() -> ParseResult<PatternPtr> {
        auto left_paren = match_and_advance(
            Token::Kind::LeftParen,
            errors::expected_before("'('", "tuple pattern")
        );
        if (!left_paren) {
            return std::unexpected(std::move(left_paren).error());
        }
        m_stream.skip_newlines();

        Vec<PatternPtr> elements;

        if (!m_stream.matches(Token::Kind::RightParen)) {
            while (!m_stream.is_at_end()) {
                auto element = parse_pattern();
                if (!element) {
                    return std::unexpected(std::move(element).error());
                }
                elements.push_back(std::move(*element));

                m_stream.skip_newlines();
                if (m_stream.matches(Token::Kind::RightParen)) {
                    break;
                }

                if (!m_stream.matches(Token::Kind::Comma)) {
                    return make_error(errors::expected_in("','", "tuple pattern"));
                }
                m_stream.advance();
                m_stream.skip_newlines();

                if (m_stream.matches(Token::Kind::RightParen)) {
                    break;
                }
            }
        }

        auto right_paren = match_and_advance(
            Token::Kind::RightParen,
            errors::expected_after("')'", "tuple pattern")
        );
        if (!right_paren) {
            return std::unexpected(std::move(right_paren).error());
        }

        return std::make_unique<TuplePattern>(std::move(elements));
    }
    auto Parser::parse_array_pattern() -> ParseResult<PatternPtr> {
        auto left_bracket = match_and_advance(
            Token::Kind::LeftBracket,
            errors::expected_before("'['", "array pattern")
        );
        if (!left_bracket) {
            return std::unexpected(std::move(left_bracket).error());
        }
        m_stream.skip_newlines();

        Vec<PatternPtr> elements;

        if (!m_stream.matches(Token::Kind::RightBracket)) {
            while (!m_stream.is_at_end()) {
                auto element = parse_pattern();
                if (!element) {
                    return std::unexpected(std::move(element).error());
                }
                elements.push_back(std::move(*element));

                m_stream.skip_newlines();
                if (m_stream.matches(Token::Kind::RightBracket)) {
                    break;
                }

                if (!m_stream.matches(Token::Kind::Comma)) {
                    return make_error(errors::expected_in("','", "array pattern"));
                }
                m_stream.advance();
                m_stream.skip_newlines();

                if (m_stream.matches(Token::Kind::RightBracket)) {
                    break;
                }
            }
        }

        auto right_bracket = match_and_advance(
            Token::Kind::RightBracket,
            errors::expected_after("']'", "array pattern")
        );
        if (!right_bracket) {
            return std::unexpected(std::move(right_bracket).error());
        }

        return std::make_unique<ArrayPattern>(std::move(elements));
    }

    auto Parser::handle_unary_whitespace() -> ParseResult<void> {
        if (m_stream.matches(Token::Kind::Plus) || m_stream.matches(Token::Kind::Minus)
            || m_stream.matches(Token::Kind::Not)) {
            auto unary_op = m_stream.peek();
            auto next_token = m_stream.peek_next();
            if (!is_binary_context() && next_token.kind() != Token::Kind::Eof
                && is_violating_unary_spacing_rule(unary_op, next_token)) {
                return make_error(
                    "unary operator cannot be separated from its operand",
                    unary_op.span()
                );
            }
        }
        return {};
    }
    auto Parser::handle_consecutive_statements(const SourceLocation& expression_start)
        -> ParseResult<void> {
        const auto& current_token = m_stream.peek();
        if (current_token.kind() != Token::Kind::Eof && current_token.kind() != Token::Kind::Unless
            && current_token.kind() != Token::Kind::Dedent) {
            if (has_locations_on_same_line(expression_start, current_token.location())) {
                return make_error(
                    "consecutive statements on line must be separated by ';'",
                    current_token.span()
                );
            }
        }
        return {};
    }
    auto Parser::handle_statement_termination() -> ParseResult<void> {
        if (m_block_depth > 0) {
            if (m_stream.matches(Token::Kind::Newline)
                || m_stream.matches(Token::Kind::Semicolon)) {
                m_stream.advance();
                m_stream.skip_newlines();
            }
        } else if (has_more_statements()) {
            auto semicolon = match_and_advance(Token::Kind::Semicolon, errors::expected("';'"));
            if (!semicolon) {
                return std::unexpected(std::move(semicolon).error());
            }
            m_stream.skip_newlines();
        }
        return {};
    }
    auto Parser::handle_postfix_condition(ExpressionPtr& expression, const Token& postfix_token)
        -> ParseResult<void> {
        m_stream.advance();

        auto condition = parse_expression();
        if (!condition) {
            return std::unexpected(std::move(condition).error());
        }

        switch (postfix_token.kind()) {
            case Token::Kind::If:
                expression = std::make_unique<IfExpression>(
                    std::move(*condition),
                    std::move(expression),
                    nullptr
                );
                break;
            case Token::Kind::Unless:
                expression = std::make_unique<UnlessExpression>(
                    std::move(*condition),
                    std::move(expression)
                );
                break;
            case Token::Kind::While:
                expression =
                    std::make_unique<WhileExpression>(std::move(*condition), std::move(expression));
                break;
            default:
                break;
        }
        return {};
    }
    auto Parser::handle_then_branch() -> ParseResult<void> {
        auto then_token = match_and_advance(
            Token::Kind::Then,
            errors::expected_after("'then'", "'if' condition")
        );
        if (!then_token) {
            return std::unexpected(std::move(then_token).error());
        }
        m_stream.skip_newlines();
        return {};
    }
    auto Parser::handle_iterators(Vec<EnumeratorPtr>& enumerators, bool indented)
        -> ParseResult<void> {
        auto iterator = parse_for_iterator();
        if (!iterator) {
            return std::unexpected(std::move(iterator).error());
        }

        auto in_clause = parse_for_in_clause();
        if (!in_clause) {
            return std::unexpected(std::move(in_clause).error());
        }

        enumerators.push_back(
            std::make_unique<Enumerator>(std::move(*iterator), std::move(*in_clause))
        );

        while (!m_stream.is_at_end()) {
            m_stream.skip_newlines();
            if (m_stream.matches(Token::Kind::Loop) || m_stream.matches(Token::Kind::Where)
                || m_stream.matches(Token::Kind::Step) || m_stream.matches(Token::Kind::Yield)
                || m_stream.matches(Token::Kind::Dedent)) {
                break;
            }

            if (!indented && !m_stream.matches(Token::Kind::Semicolon)) {
                break;
            }

            if (m_stream.matches(Token::Kind::Semicolon)) {
                m_stream.advance();
            }
            m_stream.skip_newlines();

            iterator = parse_for_iterator();
            if (!iterator) {
                return std::unexpected(std::move(iterator).error());
            }

            in_clause = parse_for_in_clause();
            if (!in_clause) {
                return std::unexpected(std::move(in_clause).error());
            }

            enumerators.push_back(
                std::make_unique<Enumerator>(std::move(*iterator), std::move(*in_clause))
            );
        }
        return {};
    }
    auto Parser::handle_optional_clauses(Option<GuardPtr>& guard) -> ParseResult<void> {
        m_stream.skip_newlines();

        if (m_stream.matches(Token::Kind::Where)) {
            m_stream.advance();
            m_stream.skip_newlines();

            auto expression = parse_expression();
            if (!expression) {
                return std::unexpected(std::move(expression).error());
            }
            guard = std::make_unique<Guard>(std::move(*expression));
            m_stream.skip_newlines();
        }

        return {};
    }
    auto Parser::handle_arguments(Vec<ExpressionPtr>& arguments) -> ParseResult<void> {
        if (!m_stream.matches(Token::Kind::RightParen)) {
            while (!m_stream.is_at_end()) {
                m_stream.skip_newlines();

                if (m_stream.matches(Token::Kind::Comma)) {
                    return make_error(errors::unexpected_in("','", "argument list"));
                }

                auto unary_result = handle_unary_whitespace();
                if (!unary_result) {
                    return std::unexpected(std::move(unary_result).error());
                }

                auto argument = parse_expression();
                if (!argument) {
                    return std::unexpected(std::move(argument).error());
                }
                arguments.push_back(std::move(*argument));

                m_stream.skip_newlines();
                if (m_stream.matches(Token::Kind::RightParen)) {
                    break;
                }

                if (!m_stream.matches(Token::Kind::Comma)) {
                    return make_error(errors::expected_in("','", "argument list"));
                }
                m_stream.advance();
                m_stream.skip_newlines();

                if (m_stream.matches(Token::Kind::Comma)) {
                    return make_error(errors::unexpected_in("','", "argument list"));
                }

                if (m_stream.matches(Token::Kind::RightParen)) {
                    return make_error(errors::unexpected_after("')'", "argument list"));
                }
            }
        }
        return {};
    }
    auto Parser::handle_parameters(Vec<ParameterPtr>& parameters) -> ParseResult<void> {
        while (!m_stream.is_at_end()) {
            auto name_token = expect_identifier("parameter name");
            if (!name_token) {
                return std::unexpected(std::move(name_token).error());
            }
            auto name = std::make_unique<IdentifierExpression>(*name_token);

            if (!m_stream.matches(Token::Kind::Colon)) {
                return make_error(errors::expected_after("':'", "parameter name"));
            }

            auto type = parse_type_annotation(Token::Kind::Colon);
            if (!type) {
                return std::unexpected(std::move(type).error());
            }

            parameters.push_back(std::make_unique<Parameter>(std::move(name), std::move(*type)));

            if (!m_stream.matches(Token::Kind::Comma)) {
                break;
            }
            m_stream.advance();
            m_stream.skip_newlines();

            if (m_stream.matches(Token::Kind::RightParen)) {
                break;
            }
        }

        return {};
    }
    auto Parser::handle_tuple_elements(Vec<ExpressionPtr>& elements) -> ParseResult<void> {
        while (!m_stream.matches(Token::Kind::RightParen)) {
            auto element = parse_expression();
            if (!element) {
                return std::unexpected(std::move(element).error());
            }
            elements.push_back(std::move(*element));

            m_stream.skip_newlines();
            if (m_stream.matches(Token::Kind::RightParen)) {
                break;
            }

            if (!m_stream.matches(Token::Kind::Comma)) {
                return make_error(errors::expected_in("','", "tuple elements"));
            }
            m_stream.advance();
            m_stream.skip_newlines();

            if (m_stream.matches(Token::Kind::RightParen)) {
                return make_error(errors::unexpected_after("')'", "tuple elements"));
            }
        }
        return {};
    }
    auto Parser::handle_array_elements(Vec<ExpressionPtr>& elements) -> ParseResult<void> {
        if (!m_stream.matches(Token::Kind::RightBracket)) {
            while (!m_stream.is_at_end()) {
                m_stream.skip_newlines();

                auto element = parse_expression();
                if (!element) {
                    return std::unexpected(std::move(element).error());
                }
                elements.push_back(std::move(*element));

                m_stream.skip_newlines();
                if (m_stream.matches(Token::Kind::RightBracket)) {
                    break;
                }

                if (!m_stream.matches(Token::Kind::Comma)) {
                    return make_error(errors::expected_in("','", "array elements"));
                }
                m_stream.advance();
                m_stream.skip_newlines();

                if (m_stream.matches(Token::Kind::RightBracket)) {
                    break;
                }
            }
        }
        return {};
    }

    auto Parser::sync() -> void {
        if (m_stream.is_at_end()) {
            return;
        }

        if (sync_to_statement_boundary() || sync_to_declaration_boundary()
            || sync_to_block_boundary() || sync_to_expression_boundary()) {
            return;
        }
        m_stream.advance();
    }
    auto Parser::sync_to_statement_boundary() -> bool {
        while (!m_stream.is_at_end()) {
            if (m_stream.matches(Token::Kind::Semicolon)
                || m_stream.matches(Token::Kind::Newline)) {
                m_stream.advance();
                m_stream.skip_newlines();
                return true;
            }

            if (is_sync_token(m_stream.peek().kind())) {
                return true;
            }
            m_stream.advance();
        }
        return false;
    }
    auto Parser::sync_to_declaration_boundary() -> bool {
        while (!m_stream.is_at_end()) {
            auto declaration_kind = m_stream.peek().kind();
            if (declaration_kind == Token::Kind::Let || declaration_kind == Token::Kind::Var
                || declaration_kind == Token::Kind::Func || declaration_kind == Token::Kind::Proc) {
                return true;
            }

            if (m_stream.matches(Token::Kind::Semicolon)
                || m_stream.matches(Token::Kind::Newline)) {
                m_stream.advance();
                m_stream.skip_newlines();
                return true;
            }
            m_stream.advance();
        }
        return false;
    }
    auto Parser::sync_to_block_boundary() -> bool {
        while (!m_stream.is_at_end()) {
            auto block_kind = m_stream.peek().kind();
            if (block_kind == Token::Kind::Indent || block_kind == Token::Kind::Dedent
                || block_kind == Token::Kind::LeftBrace || block_kind == Token::Kind::RightBrace) {
                return true;
            }

            if (m_stream.matches(Token::Kind::Semicolon)
                || m_stream.matches(Token::Kind::Newline)) {
                m_stream.advance();
                m_stream.skip_newlines();
                return true;
            }
            m_stream.advance();
        }
        return false;
    }
    auto Parser::sync_to_expression_boundary() -> bool {
        while (!m_stream.is_at_end()) {
            auto expression_kind = m_stream.peek().kind();
            if (could_end_expression(expression_kind)) {
                return true;
            }
            if (could_start_expression(expression_kind)) {
                return true;
            }

            if (m_stream.matches(Token::Kind::Semicolon)
                || m_stream.matches(Token::Kind::Newline)) {
                m_stream.advance();
                m_stream.skip_newlines();
                return true;
            }
            m_stream.advance();
        }
        return false;
    }

    auto Parser::make_error(std::string_view message, Option<SourceSpan> span)
        -> std::unexpected<Diagnostic> {
        auto diagnostic = Diagnostic(
            DiagnosticSeverity::Error,
            std::string(message),
            span.value_or(SourceSpan { m_stream.peek().span() })
        );
        m_diagnostics.get().emit_error(diagnostic.span(), std::string(diagnostic.message()));
        return std::unexpected(std::move(diagnostic));
    }
    auto Parser::expect(Token::Kind kind, std::string_view message) -> ParseResult<Token> {
        if (!m_stream.matches(kind)) {
            return make_error(errors::expected(message));
        }
        return { m_stream.advance() };
    }
    auto Parser::expect_identifier(std::string_view message) -> ParseResult<Token> {
        return expect(Token::Kind::Identifier, message);
    }
};  // namespace musi
