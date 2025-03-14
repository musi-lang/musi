#include "node.hpp"

namespace musi {
    auto GroupingExpression::accept(NodeVisitor& visitor) -> std::any {
        return visitor.visit_grouping_expression(*this);
    }
    auto GroupingExpression::to_json() const -> Json {
        return { { "type", "GroupingExpressionession" }, { "expression", expression().to_json() } };
    }

    auto LiteralExpression::accept(NodeVisitor& visitor) -> std::any {
        return visitor.visit_literal_expression(*this);
    }
    auto LiteralExpression::to_json() const -> Json {
        Json value_json;
        switch (value().kind()) {
            case Token::Kind::StrLiteral:
                value_json = std::format("\"{}\"", value().lexeme());
                break;
            case Token::Kind::CharLiteral:
                value_json = std::format("'{}'", value().lexeme());
                break;
            case Token::Kind::NumericLiteral:
                value_json = std::stod(std::string(value().lexeme()));
                break;
            default:
                value_json = value().lexeme();
                break;
        }
        return { { "type", "LiteralExpression" }, { "value", value_json } };
    }

    auto IdentifierExpression::accept(NodeVisitor& visitor) -> std::any {
        return visitor.visit_identifier_expression(*this);
    }
    auto IdentifierExpression::to_json() const -> Json {
        return { { "type", "IdentifierExpression" }, { "identifier", identifier().lexeme() } };
    }

    auto UnaryExpression::accept(NodeVisitor& visitor) -> std::any {
        return visitor.visit_unary_expression(*this);
    }
    auto UnaryExpression::to_json() const -> Json {
        return { { "type", "UnaryExpression" },
                 { "operator", op().lexeme() },
                 { "right", right().to_json() } };
    }

    auto BinaryExpression::accept(NodeVisitor& visitor) -> std::any {
        return visitor.visit_binary_expression(*this);
    }
    auto BinaryExpression::to_json() const -> Json {
        return { { "type", "BinaryExpression" },
                 { "operator", op().lexeme() },
                 { "left", left().to_json() },
                 { "right", right().to_json() } };
    }

    auto IfExpression::accept(NodeVisitor& visitor) -> std::any {
        return visitor.visit_if_expression(*this);
    }
    auto IfExpression::to_json() const -> Json {
        Json json = { { "type", "IfExpression" },
                      { "condition", condition().to_json() },
                      { "thenBranch", then_branch().to_json() } };
        if (m_else_branch) {
            json["elseBranch"] = else_branch().to_json();
        }
        return json;
    }

    auto WhileExpression::accept(NodeVisitor& visitor) -> std::any {
        return visitor.visit_while_expression(*this);
    }
    auto WhileExpression::to_json() const -> Json {
        return { { "type", "WhileExpression" },
                 { "condition", condition().to_json() },
                 { "body", body().to_json() } };
    }

    auto UnlessExpression::accept(NodeVisitor& visitor) -> std::any {
        return visitor.visit_unless_expression(*this);
    }
    auto UnlessExpression::to_json() const -> Json {
        Json json = { { "type", "UnlessExpression" }, { "condition", condition().to_json() } };
        if (m_body) {
            json["body"] = body().to_json();
        }
        return json;
    }

    auto BlockExpression::accept(NodeVisitor& visitor) -> std::any {
        return visitor.visit_block_expression(*this);
    }
    auto BlockExpression::to_json() const -> Json {
        Json statement_list = Json::array();
        for (const auto& statement : statements()) {
            statement_list.push_back(statement->to_json());
        }
        return { { "type", "BlockExpression" }, { "statements", statement_list } };
    }

    auto CallExpression::accept(NodeVisitor& visitor) -> std::any {
        return visitor.visit_call_expression(*this);
    }
    auto CallExpression::to_json() const -> Json {
        Json args = Json::array();
        for (const auto& argument : arguments()) {
            args.push_back(argument->to_json());
        }
        return { { "type", "CallExpression" },
                 { "callee", callee().to_json() },
                 { "arguments", args } };
    }

    auto ReturnExpression::accept(NodeVisitor& visitor) -> std::any {
        return visitor.visit_return_expression(*this);
    }
    auto ReturnExpression::to_json() const -> Json {
        Json json = { { "type", "ReturnExpression" } };
        if (m_value) {
            json["value"] = value().to_json();
        }
        return json;
    }

    auto ExpressionStatement::accept(NodeVisitor& visitor) -> std::any {
        return visitor.visit_expression_statement(*this);
    }
    auto ExpressionStatement::to_json() const -> Json {
        return { { "type", "ExpressionStatement" }, { "expression", expression().to_json() } };
    }

    auto VariableDeclaration::accept(NodeVisitor& visitor) -> std::any {
        return visitor.visit_variable_declaration(*this);
    }
    auto VariableDeclaration::to_json() const -> Json {
        return { { "type", "VariableDeclaration" },
                 { "kind", kind().lexeme() },
                 { "name", name().identifier().lexeme() },
                 { "value", value().to_json() } };
    }

    auto SubprogramDeclaration::accept(NodeVisitor& visitor) -> std::any {
        return visitor.visit_subprogram_declaration(*this);
    }
    auto SubprogramDeclaration::to_json() const -> Json {
        auto json = Json {};
        json["type"] = "SubprogramDeclaration";
        json["kind"] = kind().lexeme();
        json["name"] = name().identifier().lexeme();
        json["parameters"] = Json::array();
        for (const auto& parameter : parameters()) {
            json["parameters"].push_back(parameter->to_json());
        }
        json["body"] = body().to_json();
        return json;
    }
}  // namespace musi
