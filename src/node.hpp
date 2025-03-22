#pragma once

#include <any>
#include <format>
#include <utility>

#include "common.hpp"
#include "token.hpp"
#include "visitor.hpp"

namespace musi {
    class Node {
    public:
        enum class Type : uint8_t {
            TypeAnnotation,
            Parameter,
            VariableDeclarator,
            Enumerator,
            Guard,

            // expressions
            LiteralExpression,
            IdentifierExpression,
            GroupingExpression,
            ArrayExpression,
            TupleExpression,
            CallExpression,
            MemberExpression,
            UnaryExpression,
            BinaryExpression,
            RangeExpression,
            AssignmentExpression,
            // ---
            BlockExpression,
            BreakExpression,
            ContinueExpression,
            ForInExpression,
            IfExpression,
            LoopExpression,
            ReturnExpression,
            UnlessExpression,
            UntilExpression,
            WhileExpression,
            YieldExpression,

            // statements
            ExpressionStatement,
            SubprogramDeclaration,
            VariableDeclaration,

            // patterns
            TuplePattern,
            ArrayPattern,
        };

        virtual ~Node() = default;
        explicit Node(SourceSpan span) : m_span(span) {}
        Node(const Node&) = default;
        Node(Node&&) = default;
        auto operator=(const Node&) -> Node& = default;
        auto operator=(Node&&) -> Node& = default;

        [[nodiscard]] virtual auto type() const -> Type = 0;
        [[nodiscard]] auto span() const -> const SourceSpan& {
            return m_span;
        }

        [[nodiscard]] virtual auto to_json() const -> Json {
            return { { "type", std::string(magic_enum::enum_name(type())) } };
        }

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

    using ExpressionPtr = Box<Expression>;
    using IdentifierExpressionPtr = Box<IdentifierExpression>;

    class Statement : public Node {
    public:
        using Node::Node;
    };

    using StatementPtr = Box<Statement>;

    class Enumerator : public Node {
    public:
        Enumerator(ExpressionPtr iterator, ExpressionPtr iterable)
            : Node(SourceSpan::merge(iterator->span().start(), iterable->span().start()))
            , m_iterator(std::move(iterator))
            , m_iterable(std::move(iterable)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::Enumerator;
        }
        [[nodiscard]] auto iterator() const -> const Expression& {
            return *m_iterator;
        }
        [[nodiscard]] auto iterable() const -> const Expression& {
            return *m_iterable;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["iterator"] = iterator().to_json();
            json["iterable"] = iterable().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_enumerator(*this);
        }

    private:
        ExpressionPtr m_iterator;
        ExpressionPtr m_iterable;
    };
    class Pattern : public Expression {
    public:
        using Expression::Expression;
    };

    using PatternPtr = Box<Pattern>;

    using TypeAnnotationPtr = Box<TypeAnnotation>;
    using ParameterPtr = Box<Parameter>;
    using EnumeratorPtr = Box<Enumerator>;
    using GuardPtr = Box<Guard>;

    class TypeAnnotation final : public Node {
    public:
        explicit TypeAnnotation(ExpressionPtr type) : Node(type->span()), m_type(std::move(type)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::TypeAnnotation;
        }
        [[nodiscard]] auto annotation_type() const -> const Expression& {
            return *m_type;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["type"] = annotation_type().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_type_annotation(*this);
        }

    private:
        ExpressionPtr m_type;
    };
    class Parameter final : public Node {
    public:
        Parameter(ExpressionPtr name, Option<TypeAnnotationPtr> type_annotation)
            : Node(name->span())
            , m_name(std::move(name))
            , m_type_annotation(std::move(type_annotation)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::Parameter;
        }
        [[nodiscard]] auto name() const -> const Expression& {
            return *m_name;
        }
        [[nodiscard]] auto has_type_annotation() const -> bool {
            return m_type_annotation.has_value();
        }
        [[nodiscard]] auto type_annotation() const -> const TypeAnnotation& {
            return *m_type_annotation.value();
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["name"] = name().to_json();
            if (m_type_annotation) {
                json["typeAnnotation"] = type_annotation().to_json();
            }
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_parameter(*this);
        }

    private:
        ExpressionPtr m_name;
        Option<TypeAnnotationPtr> m_type_annotation;
    };
    class VariableDeclarator final : public Node {
    public:
        VariableDeclarator(
            ExpressionPtr name,
            Option<TypeAnnotationPtr> type_annotation,
            ExpressionPtr init
        )
            : Node(SourceSpan::merge(name->span().start(), init->span().start()))
            , m_name(std::move(name))
            , m_type_annotation(std::move(type_annotation))
            , m_init(std::move(init)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::VariableDeclarator;
        }
        [[nodiscard]] auto name() const -> const Expression& {
            return *m_name;
        }
        [[nodiscard]] auto has_type_annotation() const -> bool {
            return m_type_annotation.has_value();
        }
        [[nodiscard]] auto type_annotation() const -> const TypeAnnotation& {
            return *m_type_annotation.value();
        }
        [[nodiscard]] auto init() const -> const Expression& {
            return *m_init;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["name"] = name().to_json();
            if (m_type_annotation) {
                json["typeAnnotation"] = type_annotation().to_json();
            }
            json["init"] = init().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_variable_declarator(*this);
        }

    private:
        ExpressionPtr m_name;
        Option<TypeAnnotationPtr> m_type_annotation;
        ExpressionPtr m_init;
    };
    class Guard : public Node {
    public:
        explicit Guard(ExpressionPtr condition)
            : Node(condition->span())
            , m_condition(std::move(condition)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::Guard;
        }
        [[nodiscard]] auto condition() const -> const Expression& {
            return *m_condition;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["condition"] = condition().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_guard(*this);
        }

    private:
        ExpressionPtr m_condition;
    };

    class LiteralExpression final : public Expression {
    public:
        explicit LiteralExpression(Token value)
            : Expression(value.span())
            , m_value(std::move(value)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::LiteralExpression;
        }
        [[nodiscard]] auto value() const -> const Token& {
            return m_value;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            switch (value().kind()) {
                case Token::Kind::IntLiteral:
                    json["value"] = std::stoi(std::string(value().lexeme()));
                    break;
                case Token::Kind::RealLiteral:
                case Token::Kind::NatLiteral:
                    json["value"] = std::stod(std::string(value().lexeme()));
                    break;
                case Token::Kind::StrLiteral:
                case Token::Kind::CharLiteral:
                default:
                    json["value"] = value().lexeme();
                    break;
            }
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_literal_expression(*this);
        }

    private:
        Token m_value;
    };
    class IdentifierExpression final : public Expression {
    public:
        explicit IdentifierExpression(Token name)
            : Expression(name.span())
            , m_name(std::move(name)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::IdentifierExpression;
        }
        [[nodiscard]] auto name() const -> const Token& {
            return m_name;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["name"] = name().lexeme();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_identifier_expression(*this);
        }

    private:
        Token m_name;
    };
    class GroupingExpression final : public Expression {
    public:
        explicit GroupingExpression(ExpressionPtr expression)
            : Expression(expression->span())
            , m_expression(std::move(expression)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::GroupingExpression;
        }
        [[nodiscard]] auto expression() const -> const Expression& {
            return *m_expression;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["expression"] = expression().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_grouping_expression(*this);
        }

    private:
        ExpressionPtr m_expression;
    };
    class ArrayExpression final : public Expression {
    public:
        explicit ArrayExpression(Vec<ExpressionPtr> elements)
            : Expression(
                  elements.empty() ? SourceSpan()
                                   : SourceSpan::merge(
                                         elements.front()->span().start(),
                                         elements.back()->span().start()
                                     )
              )
            , m_elements(std::move(elements)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::ArrayExpression;
        }
        [[nodiscard]] auto elements() const -> const Vec<ExpressionPtr>& {
            return m_elements;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            Json element_list = Json::array();
            for (const auto& element : elements()) {
                element_list.push_back(element->to_json());
            }

            auto json = Node::to_json();
            json["elements"] = element_list;
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_array_expression(*this);
        }

    private:
        Vec<ExpressionPtr> m_elements;
    };
    class TupleExpression final : public Expression {
    public:
        explicit TupleExpression(Vec<ExpressionPtr> elements)
            : Expression(
                  elements.empty() ? SourceSpan()
                                   : SourceSpan::merge(
                                         elements.front()->span().start(),
                                         elements.back()->span().start()
                                     )
              )
            , m_elements(std::move(elements)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::TupleExpression;
        }
        [[nodiscard]] auto elements() const -> const Vec<ExpressionPtr>& {
            return m_elements;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            Json element_list = Json::array();
            for (const auto& element : elements()) {
                element_list.push_back(element->to_json());
            }

            auto json = Node::to_json();
            json["elements"] = element_list;
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_tuple_expression(*this);
        }

    private:
        Vec<ExpressionPtr> m_elements;
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

        [[nodiscard]] auto type() const -> Type override {
            return Type::CallExpression;
        }
        [[nodiscard]] auto callee() const -> const Expression& {
            return *m_callee;
        }
        [[nodiscard]] auto arguments() const -> const Vec<ExpressionPtr>& {
            return m_arguments;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            Json arguments_array = Json::array();
            for (const auto& argument : arguments()) {
                arguments_array.push_back(argument->to_json());
            }

            auto json = Node::to_json();
            json["callee"] = callee().to_json();
            json["arguments"] = arguments_array;
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_call_expression(*this);
        }

    private:
        ExpressionPtr m_callee;
        Vec<ExpressionPtr> m_arguments;
    };
    class MemberExpression final : public Expression {
    public:
        MemberExpression(Token op, ExpressionPtr object, ExpressionPtr property)
            : Expression(SourceSpan::merge(object->span().start(), property->span().start()))
            , m_operator(std::move(op))
            , m_object(std::move(object))
            , m_property(std::move(property)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::MemberExpression;
        }
        [[nodiscard]] auto op() const -> const Token& {
            return m_operator;
        }
        [[nodiscard]] auto object() const -> const Expression& {
            return *m_object;
        }
        [[nodiscard]] auto property() const -> const Expression& {
            return *m_property;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["operator"] = op().lexeme();
            json["object"] = object().to_json();
            json["property"] = property().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_member_expression(*this);
        }

    private:
        Token m_operator;
        ExpressionPtr m_object;
        ExpressionPtr m_property;
    };
    class UnaryExpression final : public Expression {
    public:
        UnaryExpression(Token op, ExpressionPtr rhs)
            : Expression(SourceSpan::merge(op.span().start(), rhs->span().start()))
            , m_operator(std::move(op))
            , m_right(std::move(rhs)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::UnaryExpression;
        }
        [[nodiscard]] auto op() const -> const Token& {
            return m_operator;
        }
        [[nodiscard]] auto right() const -> const Expression& {
            return *m_right;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["operator"] = op().lexeme();
            json["right"] = right().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_unary_expression(*this);
        }

    private:
        Token m_operator;
        ExpressionPtr m_right;
    };
    class BinaryExpression final : public Expression {
    public:
        BinaryExpression(Token op, ExpressionPtr lhs, ExpressionPtr rhs)
            : Expression(SourceSpan::merge(lhs->span().start(), rhs->span().start()))
            , m_operator(std::move(op))
            , m_left(std::move(lhs))
            , m_right(std::move(rhs)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::BinaryExpression;
        }
        [[nodiscard]] auto op() const -> const Token& {
            return m_operator;
        }
        [[nodiscard]] auto left() const -> const Expression& {
            return *m_left;
        }
        [[nodiscard]] auto right() const -> const Expression& {
            return *m_right;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["operator"] = op().lexeme();
            json["left"] = left().to_json();
            json["right"] = right().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_binary_expression(*this);
        }

    private:
        Token m_operator;
        ExpressionPtr m_left;
        ExpressionPtr m_right;
    };
    class RangeExpression final : public Expression {
    public:
        RangeExpression(Token binary_op, ExpressionPtr start, ExpressionPtr end)
            : Expression(SourceSpan::merge(start->span().start(), end->span().start()))
            , m_operator(std::move(binary_op))
            , m_start(std::move(start))
            , m_end(std::move(end)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::RangeExpression;
        }
        [[nodiscard]] auto op() const -> const Token& {
            return m_operator;
        }
        [[nodiscard]] auto start() const -> const Expression& {
            return *m_start;
        }
        [[nodiscard]] auto end() const -> const Expression& {
            return *m_end;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["operator"] = op().lexeme();
            json["start"] = start().to_json();
            json["end"] = end().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_range_expression(*this);
        }

    private:
        Token m_operator;
        ExpressionPtr m_start;
        ExpressionPtr m_end;
    };
    class AssignmentExpression final : public Expression {
    public:
        AssignmentExpression(Token op, ExpressionPtr target, ExpressionPtr value)
            : Expression(SourceSpan::merge(target->span().start(), value->span().start()))
            , m_operator(std::move(op))
            , m_target(std::move(target))
            , m_value(std::move(value)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::AssignmentExpression;
        }
        [[nodiscard]] auto op() const -> const Token& {
            return m_operator;
        }
        [[nodiscard]] auto target() const -> const Expression& {
            return *m_target;
        }
        [[nodiscard]] auto value() const -> const Expression& {
            return *m_value;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["operator"] = op().lexeme();
            json["target"] = target().to_json();
            json["value"] = value().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_assignment_expression(*this);
        }

    private:
        Token m_operator;
        ExpressionPtr m_target;
        ExpressionPtr m_value;
    };
    // ---
    class BlockExpression final : public Expression {
    public:
        explicit BlockExpression(Vec<StatementPtr> statements)
            : Expression(statements.empty() ? SourceSpan() : statements.back()->span())
            , m_statements(std::move(statements)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::BlockExpression;
        }
        [[nodiscard]] auto statements() const -> const Vec<StatementPtr>& {
            return m_statements;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            Json statement_list = Json::array();
            for (const auto& statement : statements()) {
                statement_list.push_back(statement->to_json());
            }

            auto json = Node::to_json();
            json["statements"] = statement_list;
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_block_expression(*this);
        }

    private:
        Vec<StatementPtr> m_statements;
    };
    class BreakExpression final : public Expression {
    public:
        explicit BreakExpression(SourceSpan span) : Expression(span) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::BreakExpression;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            return Node::to_json();
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_break_expression(*this);
        }
    };
    class ContinueExpression final : public Expression {
    public:
        explicit ContinueExpression(SourceSpan span) : Expression(span) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::ContinueExpression;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            return Node::to_json();
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_continue_expression(*this);
        }
    };
    class YieldExpression final : public Expression {
    public:
        explicit YieldExpression(ExpressionPtr value)
            : Expression(value->span())
            , m_value(std::move(value)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::YieldExpression;
        }
        [[nodiscard]] auto value() const -> const Expression& {
            return *m_value;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["value"] = value().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_yield_expression(*this);
        }

    private:
        ExpressionPtr m_value;
    };
    class ForInExpression final : public Expression {
    public:
        ForInExpression(
            Vec<EnumeratorPtr> enumerators,
            Option<GuardPtr> guard,
            Option<Box<YieldExpression>> yield,
            ExpressionPtr body
        )
            : Expression(SourceSpan::merge(enumerators[0]->span().start(), body->span().start()))
            , m_enumerators(std::move(enumerators))
            , m_guard(std::move(guard))
            , m_yield(std::move(yield))
            , m_body(std::move(body)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::ForInExpression;
        }
        [[nodiscard]] auto enumerators() const -> const Vec<EnumeratorPtr>& {
            return m_enumerators;
        }
        [[nodiscard]] auto has_guard() const -> bool {
            return m_guard.has_value();
        }
        [[nodiscard]] auto guard() const -> const Guard& {
            return *m_guard.value();
        }
        [[nodiscard]] auto has_yield() const -> bool {
            return m_yield.has_value();
        }
        [[nodiscard]] auto yield() const -> const YieldExpression& {
            return *m_yield.value();
        }
        [[nodiscard]] auto body() const -> const Expression& {
            return *m_body;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            Json enumerators_json = Json::array();
            for (const auto& enumerator : m_enumerators) {
                if (enumerator) {
                    enumerators_json.push_back(enumerator->to_json());
                }
            }

            auto json = Node::to_json();
            json["enumerators"] = enumerators_json;
            if (m_guard.has_value() && (*m_guard != nullptr)) {
                json["guard"] = m_guard->get()->to_json();
            }
            if (m_yield.has_value() && (*m_yield != nullptr)) {
                json["yield"] = m_yield->get()->to_json();
            }
            if (m_body) {
                json["body"] = body().to_json();
            }
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_for_in_expression(*this);
        }

    private:
        Vec<EnumeratorPtr> m_enumerators;
        Option<GuardPtr> m_guard;
        Option<Box<YieldExpression>> m_yield;
        ExpressionPtr m_body;
    };
    class IfExpression final : public Expression {
    public:
        IfExpression(ExpressionPtr condition, ExpressionPtr consequent, ExpressionPtr alternative)
            : Expression(SourceSpan::merge(
                  condition->span().start(),
                  alternative ? alternative->span().start() : consequent->span().start()
              ))
            , m_condition(std::move(condition))
            , m_consequent(std::move(consequent))
            , m_alternative(std::move(alternative)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::IfExpression;
        }
        [[nodiscard]] auto condition() const -> const Expression& {
            return *m_condition;
        }
        [[nodiscard]] auto consequent() const -> const Expression& {
            return *m_consequent;
        }
        [[nodiscard]] auto alternative() const -> const Expression& {
            return *m_alternative;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["condition"] = condition().to_json();
            json["consequent"] = consequent().to_json();
            if (m_alternative) {
                json["alternative"] = alternative().to_json();
            }
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_if_expression(*this);
        }

    private:
        ExpressionPtr m_condition;
        ExpressionPtr m_consequent;
        ExpressionPtr m_alternative;
    };
    class LoopExpression final : public Expression {
    public:
        explicit LoopExpression(ExpressionPtr body)
            : Expression(body->span())
            , m_body(std::move(body)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::LoopExpression;
        }
        [[nodiscard]] auto body() const -> const Expression& {
            return *m_body;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["body"] = body().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_loop_expression(*this);
        }

    private:
        ExpressionPtr m_body;
    };
    class ReturnExpression final : public Expression {
    public:
        explicit ReturnExpression(ExpressionPtr value)
            : Expression(value ? value->span() : SourceSpan())
            , m_value(value ? std::make_optional(std::move(value)) : std::nullopt) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::ReturnExpression;
        }
        [[nodiscard]] auto has_value() const -> bool {
            return m_value.has_value();
        }
        [[nodiscard]] auto value() const -> const Expression& {
            return *m_value.value();
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            if (m_value) {
                json["value"] = value().to_json();
            }
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_return_expression(*this);
        }

    private:
        Option<ExpressionPtr> m_value;
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

        [[nodiscard]] auto type() const -> Type override {
            return Type::UnlessExpression;
        }

        [[nodiscard]] auto condition() const -> const Expression& {
            return *m_condition;
        }
        [[nodiscard]] auto body() const -> const Expression& {
            return *m_body.value();
        }
        [[nodiscard]] auto has_body() const -> bool {
            return m_body.has_value();
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["condition"] = condition().to_json();
            if (m_body) {
                json["body"] = body().to_json();
            }
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_unless_expression(*this);
        }

    private:
        ExpressionPtr m_condition;
        Option<ExpressionPtr> m_body;
    };
    class UntilExpression final : public Expression {
    public:
        UntilExpression(ExpressionPtr condition, ExpressionPtr body)
            : Expression(SourceSpan::merge(condition->span().start(), body->span().start()))
            , m_condition(std::move(condition))
            , m_body(std::move(body)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::UntilExpression;
        }
        [[nodiscard]] auto condition() const -> const Expression& {
            return *m_condition;
        }
        [[nodiscard]] auto body() const -> const Expression& {
            return *m_body;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["condition"] = condition().to_json();
            json["body"] = body().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_until_expression(*this);
        }

    private:
        ExpressionPtr m_condition;
        ExpressionPtr m_body;
    };
    class WhileExpression final : public Expression {
    public:
        WhileExpression(ExpressionPtr condition, ExpressionPtr body)
            : Expression(SourceSpan::merge(condition->span().start(), body->span().start()))
            , m_condition(std::move(condition))
            , m_body(std::move(body)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::WhileExpression;
        }
        [[nodiscard]] auto condition() const -> const Expression& {
            return *m_condition;
        }
        [[nodiscard]] auto body() const -> const Expression& {
            return *m_body;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["condition"] = condition().to_json();
            json["body"] = body().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_while_expression(*this);
        }

    private:
        ExpressionPtr m_condition;
        ExpressionPtr m_body;
    };

    class ExpressionStatement final : public Statement {
    public:
        explicit ExpressionStatement(ExpressionPtr expression)
            : Statement(expression->span())
            , m_expression(std::move(expression)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::ExpressionStatement;
        }
        [[nodiscard]] auto expression() const -> const Expression& {
            return *m_expression;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["expression"] = expression().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_expression_statement(*this);
        }

    private:
        ExpressionPtr m_expression;
    };
    class SubprogramDeclaration final : public Statement {
    public:
        SubprogramDeclaration(
            Token kind,
            ExpressionPtr callee,
            Vec<ParameterPtr> parameters,
            Option<TypeAnnotationPtr> return_type,
            ExpressionPtr body
        )
            : Statement(callee->span())
            , m_kind(std::move(kind))
            , m_callee(std::move(callee))
            , m_parameters(std::move(parameters))
            , m_return_type(std::move(return_type))
            , m_body(std::move(body)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::SubprogramDeclaration;
        }
        [[nodiscard]] auto kind() const -> const Token& {
            return m_kind;
        }
        [[nodiscard]] auto callee() const -> const Expression& {
            return *m_callee;
        }
        [[nodiscard]] auto parameters() const -> const Vec<ParameterPtr>& {
            return m_parameters;
        }
        [[nodiscard]] auto has_return_type() const -> bool {
            return m_return_type.has_value();
        }
        [[nodiscard]] auto return_type() const -> const TypeAnnotation& {
            return *m_return_type.value();
        }
        [[nodiscard]] auto body() const -> const Expression& {
            return *m_body;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            json["kind"] = kind().lexeme();
            json["callee"] = callee().to_json();
            json["parameters"] = Json::array();
            for (const auto& parameter : parameters()) {
                json["parameters"].push_back(parameter->to_json());
            }
            if (m_return_type) {
                json["returnType"] = return_type().to_json();
            }
            json["body"] = body().to_json();
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_subprogram_declaration(*this);
        }

    private:
        Token m_kind;
        ExpressionPtr m_callee;
        Vec<ParameterPtr> m_parameters;
        Option<TypeAnnotationPtr> m_return_type;
        ExpressionPtr m_body;
    };
    class VariableDeclaration final : public Statement {
    public:
        VariableDeclaration(Token kind, Vec<Box<VariableDeclarator>> declarations)
            : Statement(declarations.front()->span())
            , m_kind(std::move(kind))
            , m_declarations(std::move(declarations)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::VariableDeclaration;
        }
        [[nodiscard]] auto declarations() const -> const Vec<Box<VariableDeclarator>>& {
            return m_declarations;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            Json declarations_json = Json::array();
            for (const auto& declarator : declarations()) {
                declarations_json.push_back(declarator->to_json());
            }
            auto json = Node::to_json();
            json["declarations"] = declarations_json;
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_variable_declaration(*this);
        }

    private:
        Token m_kind;
        Vec<Box<VariableDeclarator>> m_declarations;
    };

    class ArrayPattern : public Pattern {
    public:
        explicit ArrayPattern(Vec<PatternPtr> elements)
            : Pattern(
                  elements.empty() ? SourceSpan()
                                   : SourceSpan::merge(
                                         elements.front()->span().start(),
                                         elements.back()->span().start()
                                     )
              )
            , m_elements(std::move(elements)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::ArrayPattern;
        }
        [[nodiscard]] auto elements() const -> const Vec<PatternPtr>& {
            return m_elements;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            Json elements_json = Json::array();
            for (const auto& element : elements()) {
                elements_json.push_back(element->to_json());
            }
            json["elements"] = elements_json;
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_array_pattern(*this);
        }

    private:
        Vec<PatternPtr> m_elements;
    };
    class TuplePattern : public Pattern {
    public:
        explicit TuplePattern(Vec<PatternPtr> elements)
            : Pattern(
                  elements.empty() ? SourceSpan()
                                   : SourceSpan::merge(
                                         elements.front()->span().start(),
                                         elements.back()->span().start()
                                     )
              )
            , m_elements(std::move(elements)) {}

        [[nodiscard]] auto type() const -> Type override {
            return Type::TuplePattern;
        }
        [[nodiscard]] auto elements() const -> const Vec<PatternPtr>& {
            return m_elements;
        }

        [[nodiscard]] auto to_json() const -> Json override {
            auto json = Node::to_json();
            Json elements_json = Json::array();
            for (const auto& element : elements()) {
                elements_json.push_back(element->to_json());
            }
            json["elements"] = elements_json;
            return json;
        }

        auto accept(NodeVisitor& visitor) -> std::any override {
            return visitor.visit_tuple_pattern(*this);
        }

    private:
        Vec<PatternPtr> m_elements;
    };
}  // namespace musi

template<>
struct std::formatter<musi::Vec<musi::Box<musi::Node>>> {
    static constexpr auto parse(std::format_parse_context& ctx)
        -> std::format_parse_context::iterator {
        return ctx.begin();
    }

    static auto format(const musi::Vec<musi::Box<musi::Node>>& nodes, std::format_context& ctx)
        -> std::format_context::iterator {
        musi::Json array = musi::Json::array();
        for (const auto& node : nodes) {
            array.push_back(node->to_json());
        }
        musi::Json result = { { "type", "Program" }, { "body", array } };
        return std::format_to(ctx.out(), "{}", result.dump());
    }
};
