#pragma once

#include <any>

namespace musi {
    class TypeAnnotation;
    class Parameter;
    class VariableDeclarator;
    class Enumerator;
    class Guard;

    // expressions (precedencelly)
    // 1
    class LiteralExpression;
    class IdentifierExpression;
    class GroupingExpression;
    class ArrayExpression;
    class TupleExpression;
    // 2
    class CallExpression;
    class MemberExpression;
    // 3
    class UnaryExpression;
    // 4, 5, 6, 8, 9
    class BinaryExpression;
    // 7
    class RangeExpression;
    // 10
    class AssignmentExpression;
    // ---
    // control flow expressions(alphabetically)
    class BlockExpression;
    class BreakExpression;
    class ContinueExpression;
    class ForInExpression;
    class IfExpression;
    class LoopExpression;
    class ReturnExpression;
    class UnlessExpression;
    class UntilExpression;
    class WhileExpression;
    class YieldExpression;

    // statements (alphabetically)
    class ExpressionStatement;
    class SubprogramDeclaration;
    class VariableDeclaration;

    // patterns (alphabetically)
    class ArrayPattern;
    class TuplePattern;

    class NodeVisitor {
    public:
        virtual ~NodeVisitor() = default;
        NodeVisitor() = default;
        NodeVisitor(const NodeVisitor&) = default;
        NodeVisitor(NodeVisitor&&) = default;
        auto operator=(const NodeVisitor&) -> NodeVisitor& = default;
        auto operator=(NodeVisitor&&) -> NodeVisitor& = default;

        virtual auto visit_type_annotation(TypeAnnotation& node) -> std::any = 0;
        virtual auto visit_parameter(Parameter& node) -> std::any = 0;
        virtual auto visit_variable_declarator(VariableDeclarator& node) -> std::any = 0;
        virtual auto visit_enumerator(Enumerator& node) -> std::any = 0;
        virtual auto visit_guard(Guard& node) -> std::any = 0;

        virtual auto visit_literal_expression(LiteralExpression& node) -> std::any = 0;
        virtual auto visit_identifier_expression(IdentifierExpression& node) -> std::any = 0;
        virtual auto visit_grouping_expression(GroupingExpression& node) -> std::any = 0;
        virtual auto visit_array_expression(ArrayExpression& node) -> std::any = 0;
        virtual auto visit_call_expression(CallExpression& node) -> std::any = 0;
        virtual auto visit_member_expression(MemberExpression& node) -> std::any = 0;
        virtual auto visit_tuple_expression(TupleExpression& node) -> std::any = 0;
        virtual auto visit_unary_expression(UnaryExpression& node) -> std::any = 0;
        virtual auto visit_binary_expression(BinaryExpression& node) -> std::any = 0;
        virtual auto visit_range_expression(RangeExpression& node) -> std::any = 0;
        virtual auto visit_assignment_expression(AssignmentExpression& node) -> std::any = 0;
        // ---
        virtual auto visit_block_expression(BlockExpression& node) -> std::any = 0;
        virtual auto visit_break_expression(BreakExpression& node) -> std::any = 0;
        virtual auto visit_continue_expression(ContinueExpression& node) -> std::any = 0;
        virtual auto visit_for_in_expression(ForInExpression& node) -> std::any = 0;
        virtual auto visit_if_expression(IfExpression& node) -> std::any = 0;
        virtual auto visit_loop_expression(LoopExpression& node) -> std::any = 0;
        virtual auto visit_return_expression(ReturnExpression& node) -> std::any = 0;
        virtual auto visit_unless_expression(UnlessExpression& node) -> std::any = 0;
        virtual auto visit_until_expression(UntilExpression& node) -> std::any = 0;
        virtual auto visit_while_expression(WhileExpression& node) -> std::any = 0;
        virtual auto visit_yield_expression(YieldExpression& node) -> std::any = 0;

        virtual auto visit_expression_statement(ExpressionStatement& node) -> std::any = 0;
        virtual auto visit_subprogram_declaration(SubprogramDeclaration& node) -> std::any = 0;
        virtual auto visit_variable_declaration(VariableDeclaration& node) -> std::any = 0;

        virtual auto visit_array_pattern(ArrayPattern& node) -> std::any = 0;
        virtual auto visit_tuple_pattern(TuplePattern& node) -> std::any = 0;
    };
}  // namespace musi
