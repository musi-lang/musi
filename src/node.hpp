#pragma once

#include <any>
#include <format>
#include <utility>

#include "common.hpp"
#include "token.hpp"

namespace musi {
    class NodeVisitor {
    public:
        virtual ~NodeVisitor() = default;
        NodeVisitor() = default;
        NodeVisitor(const NodeVisitor&) = default;
        NodeVisitor(NodeVisitor&&) = default;
        auto operator=(const NodeVisitor&) -> NodeVisitor& = default;
        auto operator=(NodeVisitor&&) -> NodeVisitor& = default;

        virtual auto visit_grouping_expression(class GroupingExpression& node) -> std::any = 0;
        virtual auto visit_literal_expression(class LiteralExpression& node) -> std::any = 0;
        virtual auto visit_identifier_expression(class IdentifierExpression& node) -> std::any = 0;
        virtual auto visit_unary_expression(class UnaryExpression& node) -> std::any = 0;
        virtual auto visit_binary_expression(class BinaryExpression& node) -> std::any = 0;
        virtual auto visit_if_expression(class IfExpression& node) -> std::any = 0;
        virtual auto visit_while_expression(class WhileExpression& node) -> std::any = 0;
        virtual auto visit_unless_expression(class UnlessExpression& node) -> std::any = 0;
        virtual auto visit_block_expression(class BlockExpression& node) -> std::any = 0;
        virtual auto visit_call_expression(class CallExpression& node) -> std::any = 0;
        virtual auto visit_return_expression(class ReturnExpression& node) -> std::any = 0;

        virtual auto visit_expression_statement(class ExpressionStatement& node) -> std::any = 0;

        virtual auto visit_variable_declaration(class VariableDeclaration& node) -> std::any = 0;
        virtual auto visit_subprogram_declaration(class SubprogramDeclaration& node)
            -> std::any = 0;
    };

    class Node {
    public:
        virtual ~Node() = default;
        explicit Node(SourceSpan span) : m_span(span) {}
        Node(const Node&) = default;
        Node(Node&&) = default;
        auto operator=(const Node&) -> Node& = default;
        auto operator=(Node&&) -> Node& = default;

        [[nodiscard]] auto span() const -> const SourceSpan& {
            return m_span;
        }

        [[nodiscard]] virtual auto to_json() const -> Json = 0;
        virtual auto accept(NodeVisitor& visitor) -> std::any = 0;

        template<typename R>
        auto accept_as(NodeVisitor& visitor) -> R {
            return std::any_cast<R>(accept(visitor));
        }

    private:
        SourceSpan m_span;
    };

    class Expression : public Node {
    public:
        using Node::Node;

        [[nodiscard]] auto span() const -> SourceSpan {
            return Node::span();
        }
    };
    class Statement : public Node {
    public:
        using Node::Node;
    };
    class Declaration : public Statement {
    public:
        using Statement::Statement;
    };

    using ExpressionPtr = Box<Expression>;
    using StatementPtr = Box<Statement>;
    using DeclarationPtr = Box<Declaration>;

    /* expression pointers */
    using IdentifierExpressionPtr = Box<IdentifierExpression>;

    class GroupingExpression final : public Expression {
    public:
        explicit GroupingExpression(ExpressionPtr expression)
            : Expression(expression->span())
            , m_expression(std::move(expression)) {}

        [[nodiscard]] auto expression() const -> const Expression& {
            return *m_expression;
        }

        auto accept(NodeVisitor& visitor) -> std::any override;
        [[nodiscard]] auto to_json() const -> Json override;

    private:
        ExpressionPtr m_expression;
    };
    class LiteralExpression final : public Expression {
    public:
        explicit LiteralExpression(const Token& value) : Expression(value.span()), m_value(value) {}

        [[nodiscard]] auto value() const -> const Token& {
            return m_value;
        }

        auto accept(NodeVisitor& visitor) -> std::any override;
        [[nodiscard]] auto to_json() const -> Json override;

    private:
        Token m_value;
    };
    class IdentifierExpression final : public Expression {
    public:
        explicit IdentifierExpression(const Token& identifier)
            : Expression(identifier.span())
            , m_identifier(identifier) {}

        [[nodiscard]] auto identifier() const -> const Token& {
            return m_identifier;
        }

        auto accept(NodeVisitor& visitor) -> std::any override;
        [[nodiscard]] auto to_json() const -> Json override;

    private:
        Token m_identifier;
    };
    class UnaryExpression final : public Expression {
    public:
        UnaryExpression(const Token& op, ExpressionPtr right)
            : Expression(SourceSpan::merge(op.span().start(), right->span().start()))
            , m_operator(op)
            , m_right(std::move(right)) {}

        [[nodiscard]] auto op() const -> const Token& {
            return m_operator;
        }
        [[nodiscard]] auto right() const -> const Expression& {
            return *m_right;
        }

        auto accept(NodeVisitor& visitor) -> std::any override;
        [[nodiscard]] auto to_json() const -> Json override;

    private:
        Token m_operator;
        ExpressionPtr m_right;
    };
    class BinaryExpression final : public Expression {
    public:
        BinaryExpression(Token op, ExpressionPtr left, ExpressionPtr right)
            : Expression(SourceSpan::merge(left->span().start(), right->span().start()))
            , m_operator(std::move(op))
            , m_left(std::move(left))
            , m_right(std::move(right)) {}

        [[nodiscard]] auto op() const -> const Token& {
            return m_operator;
        }
        [[nodiscard]] auto left() const -> const Expression& {
            return *m_left;
        }
        [[nodiscard]] auto right() const -> const Expression& {
            return *m_right;
        }

        auto accept(NodeVisitor& visitor) -> std::any override;
        [[nodiscard]] auto to_json() const -> Json override;

    private:
        Token m_operator;
        ExpressionPtr m_left;
        ExpressionPtr m_right;
    };
    class IfExpression final : public Expression {
    public:
        IfExpression(ExpressionPtr condition, ExpressionPtr then_branch, ExpressionPtr else_branch)
            : Expression(SourceSpan::merge(
                  condition->span().start(),
                  else_branch ? else_branch->span().start() : then_branch->span().start()
              ))
            , m_condition(std::move(condition))
            , m_then_branch(std::move(then_branch))
            , m_else_branch(std::move(else_branch)) {}

        [[nodiscard]] auto condition() const -> const Expression& {
            return *m_condition;
        }
        [[nodiscard]] auto then_branch() const -> const Expression& {
            return *m_then_branch;
        }
        [[nodiscard]] auto else_branch() const -> const Expression& {
            return *m_else_branch;
        }

        auto accept(NodeVisitor& visitor) -> std::any override;
        [[nodiscard]] auto to_json() const -> Json override;

    private:
        ExpressionPtr m_condition;
        ExpressionPtr m_then_branch;
        ExpressionPtr m_else_branch;
    };
    class WhileExpression final : public Expression {
    public:
        WhileExpression(ExpressionPtr condition, ExpressionPtr body)
            : Expression(SourceSpan::merge(condition->span().start(), body->span().start()))
            , m_condition(std::move(condition))
            , m_body(std::move(body)) {}

        [[nodiscard]] auto condition() const -> const Expression& {
            return *m_condition;
        }
        [[nodiscard]] auto body() const -> const Expression& {
            return *m_body;
        }

        auto accept(NodeVisitor& visitor) -> std::any override;
        [[nodiscard]] auto to_json() const -> Json override;

    private:
        ExpressionPtr m_condition;
        ExpressionPtr m_body;
    };
    class UnlessExpression final : public Expression {
    public:
        UnlessExpression(ExpressionPtr condition, ExpressionPtr body)
            : Expression(SourceSpan::merge(
                  condition->span().start(),
                  body ? body->span().start() : condition->span().start()
              ))
            , m_condition(std::move(condition))
            , m_body(body ? std::make_optional(std::move(body)) : std::nullopt) {}

        [[nodiscard]] auto has_body() const -> bool {
            return m_body.has_value();
        }

        [[nodiscard]] auto condition() const -> const Expression& {
            return *m_condition;
        }
        [[nodiscard]] auto body() const -> const Expression& {
            return **m_body;
        }

        auto accept(NodeVisitor& visitor) -> std::any override;
        [[nodiscard]] auto to_json() const -> Json override;

    private:
        ExpressionPtr m_condition;
        Option<ExpressionPtr> m_body;
    };
    class BlockExpression final : public Expression {
    public:
        explicit BlockExpression(Vec<StatementPtr> statements)
            : Expression(statements.empty() ? SourceSpan() : statements.back()->span())
            , m_statements(std::move(statements)) {}

        [[nodiscard]] auto statements() const -> const Vec<StatementPtr>& {
            return m_statements;
        }

        auto accept(NodeVisitor& visitor) -> std::any override;
        [[nodiscard]] auto to_json() const -> Json override;

    private:
        Vec<StatementPtr> m_statements;
    };
    class CallExpression final : public Expression {
    public:
        CallExpression(ExpressionPtr callee, Vec<ExpressionPtr> arguments)
            : Expression(SourceSpan::merge(
                  callee->span().start(),
                  arguments.empty() ? callee->span().start() : arguments.back()->span().start()
              ))
            , m_callee(std::move(callee))
            , m_arguments(std::move(arguments)) {}

        [[nodiscard]] auto callee() const -> const Expression& {
            return *m_callee;
        }
        [[nodiscard]] auto arguments() const -> const Vec<ExpressionPtr>& {
            return m_arguments;
        }

        auto accept(NodeVisitor& visitor) -> std::any override;
        [[nodiscard]] auto to_json() const -> Json override;

    private:
        ExpressionPtr m_callee;
        Vec<ExpressionPtr> m_arguments;
    };
    class ReturnExpression final : public Expression {
    public:
        explicit ReturnExpression(ExpressionPtr value)
            : Expression(value ? value->span() : SourceSpan())
            , m_value(value ? std::make_optional(std::move(value)) : std::nullopt) {}

        [[nodiscard]] auto has_value() const -> bool {
            return m_value.has_value();
        }

        [[nodiscard]] auto value() const -> const Expression& {
            return **m_value;
        }

        auto accept(NodeVisitor& visitor) -> std::any override;
        [[nodiscard]] auto to_json() const -> Json override;

    private:
        Option<ExpressionPtr> m_value;
    };

    class ExpressionStatement final : public Statement {
    public:
        explicit ExpressionStatement(ExpressionPtr expression)
            : Statement(expression->span())
            , m_expression(std::move(expression)) {}

        [[nodiscard]] auto expression() const -> const Expression& {
            return *m_expression;
        }

        auto accept(NodeVisitor& visitor) -> std::any override;
        [[nodiscard]] auto to_json() const -> Json override;

    private:
        ExpressionPtr m_expression;
    };

    class VariableDeclaration final : public Declaration {
    public:
        VariableDeclaration(IdentifierExpressionPtr name, Token kind, ExpressionPtr value)
            : Declaration(name->span())
            , m_name(std::move(name))
            , m_kind(std::move(kind))
            , m_value(std::move(value)) {}

        [[nodiscard]] auto name() const -> const IdentifierExpression& {
            return *m_name;
        }
        [[nodiscard]] auto kind() const -> const Token& {
            return m_kind;
        }
        [[nodiscard]] auto value() const -> const Expression& {
            return *m_value;
        }

        auto accept(NodeVisitor& visitor) -> std::any override;
        [[nodiscard]] auto to_json() const -> Json override;

    private:
        IdentifierExpressionPtr m_name;
        Token m_kind;
        ExpressionPtr m_value;
    };
    class SubprogramDeclaration final : public Declaration {
    public:
        SubprogramDeclaration(
            Token kind,
            IdentifierExpressionPtr name,
            Vec<ExpressionPtr> parameters,
            ExpressionPtr body
        )
            : Declaration(name->span())
            , m_kind(std::move(kind))
            , m_name(std::move(name))
            , m_parameters(std::move(parameters))
            , m_body(std::move(body)) {}

        [[nodiscard]] auto kind() const -> const Token& {
            return m_kind;
        }
        [[nodiscard]] auto name() const -> const IdentifierExpression& {
            return *m_name;
        }
        [[nodiscard]] auto parameters() const -> const Vec<ExpressionPtr>& {
            return m_parameters;
        }
        [[nodiscard]] auto body() const -> const Expression& {
            return *m_body;
        }

        auto accept(NodeVisitor& visitor) -> std::any override;
        [[nodiscard]] auto to_json() const -> Json override;

    private:
        Token m_kind;
        IdentifierExpressionPtr m_name;
        Vec<ExpressionPtr> m_parameters;
        ExpressionPtr m_body;
    };
}  // namespace musi

template<>
struct std::formatter<std::vector<musi::Box<musi::Node>>> {
    static constexpr auto parse(std::format_parse_context& ctx)
        -> std::format_parse_context::iterator {
        return ctx.begin();
    }

    static auto format(const std::vector<musi::Box<musi::Node>>& nodes, std::format_context& ctx)
        -> std::format_context::iterator {
        musi::Json array = musi::Json::array();
        for (const auto& node : nodes) {
            array.push_back(node->to_json());
        }
        musi::Json result = { { "type", "Program" }, { "body", array } };
        return std::format_to(ctx.out(), "{}", result.dump());
    }
};
