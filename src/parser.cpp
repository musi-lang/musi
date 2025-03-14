#include "parser.hpp"

#include "errors.hpp"

namespace musi {
    auto Parser::parse() -> ParseResult<Vec<Box<Node>>> {
        Vec<Box<Node>> nodes;
        Vec<Diagnostic> errors;
        uint32_t consecutive_errors = 0;

        skip_newlines();
        while (!is_at_end()) {
            auto start_position = m_current_position;

            auto statement = parse_statement();
            if (!statement) {
                errors.push_back(std::move(statement).error());

                if (m_current_position == start_position) {
                    consecutive_errors++;
                    if (consecutive_errors > errors::MAX_CONSECUTIVE_ERRORS) {
                        return make_error("too many consecutive parsing errors, aborting...");
                    }
                } else {
                    consecutive_errors = 0;
                }
                continue;
            }

            consecutive_errors = 0;
            nodes.push_back(std::move(*statement));
            skip_newlines();
        }

        if (!errors.empty() && nodes.empty()) {
            return std::unexpected(std::move(errors.front()));
        }

        return nodes;
    }

    auto Parser::peek_at(size_t offset) const -> const Token& {
        return m_tokens.at(std::min(m_current_position + offset, m_tokens.size() - 1));
    }
    auto Parser::peek_next() const -> const Token& {
        if (m_current_position + 1 >= m_tokens.size()) {
            return m_tokens.back();
        }
        return m_tokens.at(m_current_position + 1);
    }
    auto Parser::is_at_block_start() const -> bool {
        return matches(Token::Kind::Indent) || matches(Token::Kind::LeftBrace);
    }
    auto Parser::is_statement_terminated() const -> bool {
        return matches(Token::Kind::Semicolon) || matches(Token::Kind::Newline)
               || matches(Token::Kind::Dedent) || is_at_end();
    }
    auto Parser::has_more_statements() const -> bool {
        if (matches(Token::Kind::Dedent) || matches(Token::Kind::Eof)) {
            return false;
        }

        if (matches(Token::Kind::Newline)) {
            size_t position = m_current_position;
            size_t newline_count = 0;

            while (position < m_tokens.size() && m_tokens[position].kind() == Token::Kind::Newline
            ) {
                newline_count++;
                position++;
            }

            if (newline_count > 1) {
                return false;
            }
        }

        const auto next_kind = peek_next().kind();
        return next_kind != Token::Kind::Dedent && next_kind != Token::Kind::Eof;
    }
    auto Parser::fetch_precedence() const -> Precedence {
        switch (peek().kind()) {
            case Token::Kind::ColonEquals:
                return Precedence::Assignment;
            case Token::Kind::Or:
            case Token::Kind::Xor:
                return Precedence::LogicalOr;
            case Token::Kind::And:
                return Precedence::LogicalAnd;
            case Token::Kind::Equals:
            case Token::Kind::SlashEquals:
                return Precedence::Equality;
            case Token::Kind::Less:
            case Token::Kind::LessEquals:
            case Token::Kind::Greater:
            case Token::Kind::GreaterEquals:
            case Token::Kind::LessEqualsGreater:
                return Precedence::Comparison;
            case Token::Kind::DotDot:
            case Token::Kind::DotDotLess:
                return Precedence::Range;
            case Token::Kind::Plus:
            case Token::Kind::Minus:
                return Precedence::Term;
            case Token::Kind::Star:
            case Token::Kind::Slash:
            case Token::Kind::Mod:
            case Token::Kind::Shl:
            case Token::Kind::Shr:
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
    auto Parser::advance() -> const Token& {
        const auto& current_token = peek();
        if (!is_at_end()) {
            m_current_position++;
        }
        return current_token;
    }
    auto Parser::advance_by(uint32_t count) -> void {
        for (uint32_t i = 0; i < count; i++) {
            advance();
        }
    }
    auto Parser::skip_newlines() -> void {
        while (matches(Token::Kind::Newline)) {
            advance();
        }
    }

    auto Parser::sync() -> void {
        if (sync_to_statement_boundary() || sync_to_declaration_boundary()
            || sync_to_block_boundary() || sync_to_expression_boundary()) {
            return;
        }

        if (!is_at_end()) {
            advance();
        }
    }
    auto Parser::sync_to_statement_boundary() -> bool {
        while (!is_at_end()) {
            if (matches(Token::Kind::Semicolon) || matches(Token::Kind::Newline)) {
                advance();
                skip_newlines();
                return true;
            }

            if (is_sync_token(peek().kind())) {
                return true;
            }
            advance();
        }
        return false;
    }
    auto Parser::sync_to_declaration_boundary() -> bool {
        while (!is_at_end()) {
            auto current_kind = peek().kind();
            if (current_kind == Token::Kind::Let || current_kind == Token::Kind::Var
                || current_kind == Token::Kind::Func || current_kind == Token::Kind::Proc) {
                return true;
            }

            if (matches(Token::Kind::Semicolon) || matches(Token::Kind::Newline)) {
                advance();
                skip_newlines();
                return true;
            }
            advance();
        }
        return false;
    }
    auto Parser::sync_to_block_boundary() -> bool {
        while (!is_at_end()) {
            auto current_kind = peek().kind();
            if (current_kind == Token::Kind::Indent || current_kind == Token::Kind::Dedent
                || current_kind == Token::Kind::LeftBrace
                || current_kind == Token::Kind::RightBrace) {
                return true;
            }

            if (matches(Token::Kind::Semicolon) || matches(Token::Kind::Newline)) {
                advance();
                skip_newlines();
                return true;
            }
            advance();
        }
        return false;
    }
    auto Parser::sync_to_expression_boundary() -> bool {
        while (!is_at_end()) {
            auto current_kind = peek().kind();
            if (could_end_expression(current_kind)) {
                return true;
            }
            if (could_start_expression(current_kind)) {
                return true;
            }

            if (matches(Token::Kind::Semicolon) || matches(Token::Kind::Newline)) {
                advance();
                skip_newlines();
                return true;
            }
            advance();
        }
        return false;
    }

    auto Parser::make_error(std::string_view message, Option<SourceSpan> span)
        -> std::unexpected<Diagnostic> {
        auto diagnostic = Diagnostic(
            DiagnosticSeverity::Error,
            std::string(message),
            span.value_or(SourceSpan { peek().span() })
        );
        m_diagnostics.get().emit_error(diagnostic.span(), std::string(diagnostic.message()));
        return std::unexpected(std::move(diagnostic));
    }
    auto Parser::expect(Token::Kind kind, std::string_view message) -> ParseResult<Token> {
        if (!matches(kind)) {
            return make_error(errors::expected(message));
        }
        return { advance() };
    }
    auto Parser::expect_identifier(std::string_view message) -> ParseResult<Token> {
        return expect(Token::Kind::Identifier, message);
    }

    auto Parser::validate_unary_whitespace() -> ParseResult<void> {
        if (matches(Token::Kind::Plus) || matches(Token::Kind::Minus)
            || matches(Token::Kind::Not)) {
            auto unary_op = peek();
            auto next_token = peek_next();
            if (next_token.kind() != Token::Kind::Eof
                && is_violating_unary_spacing_rule(unary_op, next_token)) {
                return make_error(
                    "unary operator cannot be separated from its operand",
                    unary_op.span()
                );
            }
        }
        return {};
    }
    auto Parser::validate_consecutive_statements(const SourceLocation& expression_start)
        -> ParseResult<void> {
        const auto& current_token = peek();
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
    auto Parser::validate_statement_termination() -> ParseResult<void> {
        if (m_block_depth > 0) {
            if (matches(Token::Kind::Newline) || matches(Token::Kind::Semicolon)) {
                advance();
                skip_newlines();
            }
        } else if (has_more_statements()) {
            auto semicolon = match_and_advance(Token::Kind::Semicolon, errors::expected("';'"));
            if (!semicolon) {
                return std::unexpected(std::move(semicolon).error());
            }
            skip_newlines();
        }
        return {};
    }
    auto Parser::validate_postfix_condition(ExpressionPtr& expression, const Token& postfix_token)
        -> ParseResult<void> {
        advance();

        auto condition = parse_expression(Precedence::None);
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
    auto Parser::validate_then_branch() -> ParseResult<void> {
        auto then_token = match_and_advance(
            Token::Kind::Then,
            errors::expected_after("'then'", "'if' condition")
        );
        if (!then_token) {
            return std::unexpected(std::move(then_token).error());
        }
        skip_newlines();
        return {};
    }
    auto Parser::validate_arguments(Vec<ExpressionPtr>& arguments) -> ParseResult<void> {
        if (!matches(Token::Kind::RightParen)) {
            while (!is_at_end()) {
                skip_newlines();
                if (matches(Token::Kind::Comma)) {
                    return make_error(errors::unexpected_in("','", "argument list"));
                }

                auto unary_result = validate_unary_whitespace();
                if (!unary_result) {
                    return std::unexpected(std::move(unary_result).error());
                }

                auto argument = parse_expression();
                if (!argument) {
                    return std::unexpected(std::move(argument).error());
                }
                arguments.push_back(std::move(*argument));
                skip_newlines();

                if (matches(Token::Kind::RightParen)) {
                    break;
                }
                if (!matches(Token::Kind::Comma)) {
                    return make_error(errors::expected_in("','", "argument list"));
                }
                advance();
                skip_newlines();
                if (matches(Token::Kind::Comma)) {
                    return make_error(errors::unexpected_in("','", "argument list"));
                }
                if (matches(Token::Kind::RightParen)) {
                    return make_error(errors::unexpected_after("')'", "argument list"));
                }
            }
        }
        return {};
    }
    auto Parser::validate_parameters(Vec<Token>& parameters) -> ParseResult<void> {
        while (!is_at_end()) {
            skip_newlines();
            if (matches(Token::Kind::Comma)) {
                return make_error(errors::unexpected_in("','", "parameter list"));
            }

            auto parameter = expect_identifier("parameter name");
            if (!parameter) {
                return std::unexpected(std::move(parameter).error());
            }
            parameters.push_back(*parameter);

            skip_newlines();
            if (matches(Token::Kind::RightParen)) {
                break;
            }
            if (!matches(Token::Kind::Comma)) {
                return make_error(errors::expected_in("','", "parameter list"));
            }
            advance();
            skip_newlines();
            if (matches(Token::Kind::Comma)) {
                return make_error(errors::unexpected_in("','", "parameter list"));
            }
            if (matches(Token::Kind::RightParen)) {
                return make_error(errors::unexpected_after("')'", "parameter list"));
            }
        }
        return {};
    }

    auto Parser::parse_expression(Precedence precedence) -> ParseResult<ExpressionPtr> {
        auto unary_result = validate_unary_whitespace();
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
            auto result = parse_infix(std::move(*left));
            if (!result) {
                sync();
                return std::unexpected(std::move(result).error());
            }
            left = std::move(result);
        }

        return left;
    }
    auto Parser::parse_expression_or_block_expression() -> ParseResult<ExpressionPtr> {
        if (is_at_block_start()) {
            return parse_block_expression();
        }
        return parse_expression(Precedence::None);
    }
    auto Parser::parse_statement() -> ParseResult<StatementPtr> {
        if (matches(Token::Kind::Dedent) || matches(Token::Kind::Eof)) {
            return make_error(errors::expected("expression"));
        }
        skip_newlines();

        auto statement = [&]() -> ParseResult<StatementPtr> {
            switch (peek().kind()) {
                case Token::Kind::Let:
                case Token::Kind::Var:
                    return parse_variable_declaration();
                case Token::Kind::Func:
                case Token::Kind::Proc:
                    return parse_subprogram_declaration();
                default:
                    return parse_expression_statement();
            }
        }();
        if (!statement) {
            auto error_position = m_current_position;

            sync();
            if (m_current_position == error_position && !is_at_end()) {
                advance();
            }
            return std::unexpected(std::move(statement).error());
        }
        return statement;
    }

    auto Parser::parse_prefix() -> ParseResult<ExpressionPtr> {
        auto prefix_rule = fetch_prefix_rule(peek().kind());
        if (prefix_rule == nullptr) {
            const auto& current_token = peek();
            auto error = make_error(
                errors::unexpected(std::format("{}", magic_enum::enum_name(current_token.kind()))),
                current_token.span()
            );

            sync();
            return error;
        }
        return (this->*prefix_rule)();
    }
    auto Parser::parse_infix(ExpressionPtr left) -> ParseResult<ExpressionPtr> {
        auto infix_rule = fetch_infix_rule(peek().kind());
        if (infix_rule == nullptr) {
            return std::move(left);
        }
        return (this->*infix_rule)(std::move(left));
    }
    auto Parser::parse_postfix(ExpressionPtr left) -> ParseResult<ExpressionPtr> {
        const auto postfix_start = left->span().start();

        if (matches(Token::Kind::If) || matches(Token::Kind::Unless)
            || matches(Token::Kind::While)) {
            auto postfix_token = peek();
            if (has_locations_on_same_line(postfix_start, postfix_token.location())) {
                auto result = validate_postfix_condition(left, postfix_token);
                if (!result) {
                    return std::unexpected(std::move(result).error());
                }
            }
        }

        return std::move(left);
    }

    auto Parser::parse_grouping_expression() -> ParseResult<ExpressionPtr> {
        advance();
        skip_newlines();

        auto expression = parse_expression(Precedence::None);
        if (!expression) {
            return std::unexpected(std::move(expression).error());
        }

        skip_newlines();
        auto right_paren = match_and_advance(Token::Kind::RightParen, errors::expected("')'"));
        if (!right_paren) {
            return std::unexpected(std::move(right_paren).error());
        }

        return expression;
    }
    auto Parser::parse_literal_expression() -> ParseResult<ExpressionPtr> {
        auto token = advance();
        return std::make_unique<LiteralExpression>(token);
    }
    auto Parser::parse_identifier_expression() -> ParseResult<ExpressionPtr> {
        auto token = advance();
        return std::make_unique<IdentifierExpression>(token);
    }
    auto Parser::parse_unary_expression() -> ParseResult<ExpressionPtr> {
        auto op = peek();

        auto next_token = peek_next();
        if (next_token.kind() != Token::Kind::Eof
            && is_violating_unary_spacing_rule(op, next_token)) {
            auto span = SourceSpan::merge(op.span().start(), next_token.span().start());
            return make_error("unary operator cannot be separated from its operand", span);
        }
        advance();

        auto operand = parse_expression(Precedence::Unary);
        if (!operand) {
            return std::unexpected(std::move(operand).error());
        }

        return std::make_unique<UnaryExpression>(op, std::move(*operand));
    }
    auto Parser::parse_if_expression() -> ParseResult<ExpressionPtr> {
        advance();

        auto condition = parse_expression(Precedence::None);
        if (!condition) {
            return std::unexpected(std::move(condition).error());
        }

        skip_newlines();
        auto then_result = validate_then_branch();
        if (!then_result) {
            return std::unexpected(std::move(then_result).error());
        }

        auto then_branch = parse_expression_or_block_expression();
        if (!then_branch) {
            return std::unexpected(std::move(then_branch).error());
        }
        skip_newlines();

        ExpressionPtr else_branch = nullptr;
        if (matches(Token::Kind::Else)) {
            advance();
            skip_newlines();
            if (matches(Token::Kind::If)) {
                auto nested_if = parse_if_expression();
                if (!nested_if) {
                    return std::unexpected(std::move(nested_if).error());
                }
                else_branch = std::move(*nested_if);
            } else {
                auto expression = parse_expression_or_block_expression();
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
    auto Parser::parse_while_expression() -> ParseResult<ExpressionPtr> {
        advance();

        auto condition = parse_expression(Precedence::None);
        if (!condition) {
            return std::unexpected(std::move(condition).error());
        }

        skip_newlines();
        auto do_token =
            match_and_advance(Token::Kind::Do, errors::expected_after("'do'", "'while' condition"));
        if (!do_token) {
            return std::unexpected(std::move(do_token).error());
        }
        skip_newlines();

        auto body = parse_expression_or_block_expression();
        if (!body) {
            return std::unexpected(std::move(body).error());
        }

        return std::make_unique<WhileExpression>(std::move(*condition), std::move(*body));
    }
    auto Parser::parse_unless_expression() -> ParseResult<ExpressionPtr> {
        advance();

        auto condition = parse_expression(Precedence::None);
        if (!condition) {
            return std::unexpected(std::move(condition).error());
        }

        skip_newlines();
        if (matches(Token::Kind::Then)) {
            auto then_token = match_and_advance(Token::Kind::Then, "");
            if (!then_token) {
                return std::unexpected(std::move(then_token).error());
            }
            skip_newlines();

            auto body = parse_expression_or_block_expression();
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
    auto Parser::parse_block_expression() -> ParseResult<ExpressionPtr> {
        if (m_block_depth >= MAX_BLOCK_DEPTH) {
            return make_error(errors::exceeded("maximum block nesting depth"));
        }
        m_block_depth++;

        skip_newlines();
        auto indent = match_and_advance(
            Token::Kind::Indent,
            errors::expected_at("indentation", "start of block")
        );
        if (!indent) {
            m_block_depth--;
            return std::unexpected(std::move(indent).error());
        }
        skip_newlines();

        Vec<StatementPtr> statements;
        while (!is_at_end()) {
            if (matches(Token::Kind::Dedent)) {
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
        while (!is_at_end() && !matches(Token::Kind::Newline)) {
            auto statement = parse_statement();
            if (!statement) {
                m_block_depth--;
                return std::unexpected(std::move(statement).error());
            }
            statements.push_back(std::move(*statement));

            if (matches(Token::Kind::Semicolon)) {
                advance();
                skip_newlines();
            }
        }

        m_block_depth--;
        return std::make_unique<BlockExpression>(std::move(statements));
    }
    auto Parser::parse_binary_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr> {
        auto op = advance();
        auto precedence = fetch_precedence();

        auto right = parse_expression(precedence);
        if (!right) {
            return std::unexpected(std::move(right).error());
        }

        return std::make_unique<BinaryExpression>(op, std::move(left), std::move(*right));
    }
    auto Parser::parse_call_expression(ExpressionPtr left) -> ParseResult<ExpressionPtr> {
        auto left_paren = match_and_advance(
            Token::Kind::LeftParen,
            errors::expected_before("'('", "expression list")
        );
        if (!left_paren) {
            return std::unexpected(std::move(left_paren).error());
        }
        skip_newlines();

        Vec<ExpressionPtr> arguments;
        auto argument_result = validate_arguments(arguments);
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
    auto Parser::parse_return_expression() -> ParseResult<ExpressionPtr> {
        advance();

        if (is_statement_terminated() || matches(Token::Kind::RightBrace)
            || matches(Token::Kind::Dedent)) {
            return std::make_unique<ReturnExpression>(nullptr);
        }

        auto value = parse_expression(Precedence::None);
        if (!value) {
            return std::unexpected(std::move(value).error());
        }

        return std::make_unique<ReturnExpression>(std::move(*value));
    }

    auto Parser::parse_expression_statement() -> ParseResult<StatementPtr> {
        auto expression = parse_expression(Precedence::None);
        if (!expression) {
            return std::unexpected(std::move(expression).error());
        }

        auto postfix = parse_postfix(std::move(*expression));
        if (!postfix) {
            return std::unexpected(std::move(postfix).error());
        }

        const auto postfix_start = (*postfix)->span().start();

        if (!is_statement_terminated() && !is_at_end() && m_block_depth == 0) {
            auto consecutive_result = validate_consecutive_statements(postfix_start);
            if (!consecutive_result) {
                return std::unexpected(std::move(consecutive_result).error());
            }
        }

        if (m_block_depth > 0) {
            if (matches(Token::Kind::Newline) || matches(Token::Kind::Semicolon)) {
                advance();
                skip_newlines();
            } else if (!matches(Token::Kind::Dedent) && !matches(Token::Kind::Eof)
                       && has_more_statements()) {
                const auto& next_token = peek();
                if (has_locations_on_same_line(postfix_start, next_token.location())) {
                    return make_error(
                        "consecutive statements on same line must be separated by ';'",
                        next_token.span()
                    );
                }
            }
        } else if (has_more_statements()) {
            bool handled = false;
            if (matches(Token::Kind::Newline) || matches(Token::Kind::Semicolon)) {
                advance();
                skip_newlines();
                handled = true;
            }
            if (!handled) {
                const auto& next_token = peek();
                if (has_locations_on_same_line(postfix_start, next_token.location())) {
                    return make_error("consecutive statements at top level must be separated by ';'"
                    );
                }
            }
        }

        return std::make_unique<ExpressionStatement>(std::move(*postfix));
    }
    auto Parser::parse_variable_declaration() -> ParseResult<DeclarationPtr> {
        bool reassignable = matches(Token::Kind::Var);
        Token kind_token(
            reassignable ? Token::Kind::Var : Token::Kind::Let,
            reassignable ? "var" : "let",
            peek().location()
        );
        advance();

        auto name_token = expect_identifier("variable name");
        if (!name_token) {
            return std::unexpected(std::move(name_token).error());
        }
        auto name = std::make_unique<IdentifierExpression>(*name_token);

        auto colon_equals = match_and_advance(
            Token::Kind::ColonEquals,
            errors::expected_after("':='", "variable declaration")
        );
        if (!colon_equals) {
            return std::unexpected(std::move(colon_equals).error());
        }

        auto value = parse_expression(Precedence::None);
        if (!value) {
            return std::unexpected(std::move(value).error());
        }

        return std::make_unique<VariableDeclaration>(
            std::move(name),
            kind_token,
            std::move(*value)
        );
    }
    auto Parser::parse_subprogram_declaration() -> ParseResult<DeclarationPtr> {
        bool has_return_type = matches(Token::Kind::Func);
        Token kind_token(
            has_return_type ? Token::Kind::Func : Token::Kind::Proc,
            has_return_type ? "func" : "proc",
            peek().location()
        );
        advance();

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

        Vec<Token> parameter_tokens;
        if (!matches(Token::Kind::RightParen)) {
            auto parameter_result = validate_parameters(parameter_tokens);
            if (!parameter_result) {
                return std::unexpected(std::move(parameter_result).error());
            }
        }

        Vec<ExpressionPtr> parameters;
        for (auto& parameter_token : parameter_tokens) {
            parameters.push_back(std::make_unique<IdentifierExpression>(parameter_token));
        }

        auto right_paren = match_and_advance(
            Token::Kind::RightParen,
            errors::expected_after("')'", "parameter list")
        );
        if (!right_paren) {
            return std::unexpected(std::move(right_paren).error());
        }

        auto colon_equals = match_and_advance(
            Token::Kind::ColonEquals,
            errors::expected_after("':='", "parameter list")
        );
        if (!colon_equals) {
            return std::unexpected(std::move(colon_equals).error());
        }
        skip_newlines();

        auto body =
            is_at_block_start() ? parse_block_expression() : parse_block_expression_implicit();
        if (!body) {
            return std::unexpected(std::move(body).error());
        }

        return std::make_unique<SubprogramDeclaration>(
            kind_token,
            std::move(name),
            std::move(parameters),
            std::move(*body)
        );
    }
};  // namespace musi
