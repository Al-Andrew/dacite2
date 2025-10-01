#include "parser.hpp"
#include <cstdio>

#define DBG_PRINT(X, ...) printf(X "\n", ##__VA_ARGS__)

namespace dacite {

    // Print function implementation for AST nodes
    void AST::print_node(NodeIndex index, int indent) const {
        if (index >= nodes.size()) {
            printf("%*s(invalid node index: %u)\n", indent * 2, "", index);
            return;
        }
        
        std::visit([this, indent](const auto& node) {
            using T = std::decay_t<decltype(node)>;
            
            if constexpr (std::is_same_v<T, Module>) {
                printf("%*sModule\n", indent * 2, "");
                for (NodeIndex statement_index : node.statements) {
                    print_node(statement_index, indent + 1);
                }
            } else if constexpr (std::is_same_v<T, VariableDeclaration>) {
                printf("%*sVariableDeclaration\n", indent * 2, "");
                print_node(node.identifier, indent + 1);
                print_node(node.type, indent + 1);
                print_node(node.initializer, indent + 1);
            } else if constexpr (std::is_same_v<T, IntrinsicPrint>) {
                printf("%*sIntrinsicPrint (@print)\n", indent * 2, "");
                print_node(node.expression, indent + 1);
            } else if constexpr (std::is_same_v<T, NumberLiteral>) {
                std::string lexeme{node.token.lexeme};
                printf("%*sNumberLiteral: %s\n", indent * 2, "", lexeme.c_str());
            } else if constexpr (std::is_same_v<T, Identifier>) {
                std::string lexeme{node.token.lexeme};
                printf("%*sIdentifier: %s\n", indent * 2, "", lexeme.c_str());
            } else if constexpr (std::is_same_v<T, Type>) {
                std::string lexeme{node.token.lexeme};
                printf("%*sType: %s\n", indent * 2, "", lexeme.c_str());
            } else if constexpr (std::is_same_v<T, BinaryExpression>) {
                std::string lexeme{node.operator_token.lexeme};
                printf("%*sBinaryExpression: %s\n", indent * 2, "", lexeme.c_str());
                print_node(node.left, indent + 1);
                print_node(node.right, indent + 1);
            } else if constexpr (std::is_same_v<T, UnaryPrefixExpression>) {
                std::string lexeme{node.operator_token.lexeme};
                printf("%*sUnaryPrefixExpression: %s\n", indent * 2, "", lexeme.c_str());
                print_node(node.operand, indent + 1);
            }
        }, nodes[index]);
    }

    Parser::Parser(const std::vector<Token>& tokens)
        : tokens(tokens), index(0) {
    }

    auto Parser::with_tokens(const std::vector<Token>& tokens) -> Parser {
        return Parser(tokens);
    }

    auto Parser::expect_token(Token::Type expected_type, const char* context) -> bool {
        if (is_at_end()) {
            fprintf(stderr, "Error: Unexpected end of tokens while expecting %s at token %zu\n", context, index);
            return false;
        }
        if (tokens[index].type != expected_type) {
            fprintf(stderr, "Error: Expected %s at token %zu but got token type %d\n", context, index, (int)tokens[index].type);
            return false;
        }
        return true;
    }

    auto Parser::consume_token(Token::Type expected_type, const char* context) -> bool {
            if (!expect_token(expected_type, context)) {
                return false;
            }
            index++;
            return true;
        }

    auto Parser::is_at_end() -> bool {
            return index >= tokens.size();
        }

    auto Parser::current_token() -> const Token& {
            return tokens[index];
        }

    auto Parser::advance() -> void {
            if (!is_at_end()) {
                index++;
            }
        }

    auto Parser::get_infix_binding(Token::Type type) -> std::pair<int, int> {
        switch(type) {
            case Token::Type::Plus:
                return {5, 6};
            case Token::Type::Minus:
                return {5, 6};
            case Token::Type::Star:
                return {7, 8};
            case Token::Type::Slash:
                return {7, 8};
        case Token::Type::Equals:
            return {2, 1};
        default:
            return {0, 0};
        }
    }

    auto Parser::get_prefix_binding(Token::Type type) -> std::pair<int, int> {
        switch(type) {
            case Token::Type::Minus:
                return {0, 9};
            case Token::Type::Plus:
                return {0, 9};
            default:
                return {0, 0};
        }
    }

    auto Parser::parse_expression_bp(int min_bp) -> NodeIndex {
        DBG_PRINT("parse_expression_bp: index=%zu, min_bp=%d", index, min_bp);

        if (is_at_end()) {
            fprintf(stderr, "Error: Unexpected end of tokens while parsing expression at token %zu\n", index);
            return INVALID_NODE_INDEX;
        }

        const auto lhs_token = tokens[index++];
        NodeIndex lhs = INVALID_NODE_INDEX;
        
        switch (lhs_token.type) {
            case Token::Type::Literal_Number: {
                lhs = ast.add_node(NumberLiteral(lhs_token));
                DBG_PRINT("lhs is NumberLiteral");
            } break;
            case Token::Type::Identifier: {
                lhs = ast.add_node(Identifier(lhs_token));
                DBG_PRINT("lhs is Identifier");
            } break;
            case Token::Type::Lparen: {
                lhs = parse_expression_bp(0);
                if(lhs == INVALID_NODE_INDEX) {
                    return INVALID_NODE_INDEX;
                }
                if (!consume_token(Token::Type::Rparen, ")")) {
                    return INVALID_NODE_INDEX;
                }
            } break;
            case Token::Type::Minus:
            case Token::Type::Plus: {
                auto [_, r_bp] = get_prefix_binding(lhs_token.type);
                auto rhs = parse_expression_bp(r_bp);
                if(rhs == INVALID_NODE_INDEX) {
                    return INVALID_NODE_INDEX;
                }
                UnaryPrefixExpression prefix_node(lhs_token);
                prefix_node.operand = rhs;
                lhs = ast.add_node(std::move(prefix_node));
            } break;
            default:
                fprintf(stderr, "Error: Unexpected token type %d at token %zu\n", (int)lhs_token.type, index - 1);
                return INVALID_NODE_INDEX;
        }

        while(!is_at_end()) {
            const auto op_token = current_token();
            DBG_PRINT("Considering operator token type %d at index %zu", (int)op_token.type, index);
            
            // Check for infix operators
            if(auto [l_bp, r_bp] = get_infix_binding(op_token.type); l_bp != 0) {
                if(l_bp < min_bp) {
                    break;
                }
                index++; // consume operator
                auto rhs = parse_expression_bp(r_bp);
                if(rhs == INVALID_NODE_INDEX) {
                    return INVALID_NODE_INDEX;
                }
                BinaryExpression binary_node(op_token);
                binary_node.left = lhs;
                binary_node.right = rhs;
                lhs = ast.add_node(std::move(binary_node));
                continue;
            }
            
            // No postfix operators implemented yet
            break; // No more operators
        }

        return lhs;
    }

    auto Parser::parse_expression() -> NodeIndex {
        if (is_at_end()) {
            fprintf(stderr, "Error: Unexpected end of tokens while parsing expression at token %zu\n", index);
            return INVALID_NODE_INDEX;
        }

        return parse_expression_bp(0);
    }

    auto Parser::parse_intrinsic_print() -> NodeIndex {
        DBG_PRINT("parse_intrinsic_print: index=%zu", index);
        
        // Expecting: @print ( <expression> ) ;
        if (!consume_token(Token::Type::Intrinsic_Print, "@print")) {
            return INVALID_NODE_INDEX;
        }

        if (!consume_token(Token::Type::Lparen, "( after @print")) {
            return INVALID_NODE_INDEX;
        }

        auto expression = parse_expression();
        if (expression == INVALID_NODE_INDEX) {
            fprintf(stderr, "Error: Failed to parse expression after @print at token %zu\n", index);
            return INVALID_NODE_INDEX;
        }

        if (!consume_token(Token::Type::Rparen, ") after expression")) {
            return INVALID_NODE_INDEX;
        }

        if (!consume_token(Token::Type::Semicolon, "; after )")) {
            return INVALID_NODE_INDEX;
        }

        // Construct AST node for @print statement
        IntrinsicPrint print_node;
        print_node.expression = expression;
        return ast.add_node(std::move(print_node));
    }

    auto Parser::parse_variable_declaration() -> NodeIndex {
        DBG_PRINT("parse_variable_declaration: index=%zu", index);
        
        // Expecting: let <identifier> : <type> = <expression> ;
        if (!consume_token(Token::Type::Keyword_Let, "let")) {
            return INVALID_NODE_INDEX;
        }

        if (!expect_token(Token::Type::Identifier, "identifier after 'let'")) {
            return INVALID_NODE_INDEX;
        }
        Token identifier_token = tokens[index++]; // consume identifier
        auto identifier_index = ast.add_node(Identifier(identifier_token));
        DBG_PRINT("Parsed identifier");

        if (!consume_token(Token::Type::Colon, ": after identifier")) {
            return INVALID_NODE_INDEX;
        }

        if (!expect_token(Token::Type::Identifier, "type identifier after ':'")) {
            return INVALID_NODE_INDEX;
        }
        Token type_token = tokens[index++]; // consume type identifier
        auto type_index = ast.add_node(Type(type_token));
        DBG_PRINT("Parsed type");

        if (!consume_token(Token::Type::Equals, "= after type")) {
            return INVALID_NODE_INDEX;
        }

        auto expression = parse_expression();
        if (expression == INVALID_NODE_INDEX) {
            fprintf(stderr, "Error: Failed to parse expression after '=' at token %zu\n", index);
            return INVALID_NODE_INDEX;
        }

        if (!consume_token(Token::Type::Semicolon, "; after expression")) {
            return INVALID_NODE_INDEX;
        }

        // Construct AST node for variable declaration
        VariableDeclaration var_decl_node;
        var_decl_node.identifier = identifier_index;
        var_decl_node.type = type_index;
        var_decl_node.initializer = expression;
        return ast.add_node(std::move(var_decl_node));
    }

    auto Parser::parse_statement() -> NodeIndex {
        if (is_at_end()) {
            return INVALID_NODE_INDEX;
        }

        const Token& token = current_token();
        
        switch(token.type) {
            case Token::Type::Unknown: {
                fprintf(stderr, "Error: Unknown token type %d at token %zu\n", (int)token.type, index);
                return INVALID_NODE_INDEX;
            } break;
            case Token::Type::Intrinsic_Print: {
                return parse_intrinsic_print();
            } break;
            case Token::Type::Keyword_Let: {
                return parse_variable_declaration();
            } break;
            case Token::Type::Identifier:
            case Token::Type::Literal_Number: {
                auto expr = parse_expression();
                if (expr == INVALID_NODE_INDEX) {
                    return INVALID_NODE_INDEX;
                }
                if (!consume_token(Token::Type::Semicolon, "; after expression")) {
                    return INVALID_NODE_INDEX;
                }
                return expr;
            } break;
            default: {
                fprintf(stderr, "Error: Unexpected token type %d at token %zu\n", (int)token.type, index);
                return INVALID_NODE_INDEX;
            } break;
        }
    }

    auto Parser::parse() -> AST {
        DBG_PRINT("Parsing %zu tokens", tokens.size());

        ast.root_index = ast.add_node(Module());

        while (!is_at_end() && current_token().type != Token::Type::_EOF) {
            auto stmt = parse_statement();
            if (stmt != INVALID_NODE_INDEX) {
                auto* module_node = ast.get_node<Module>(ast.root_index);
                if (module_node) {
                    module_node->statements.push_back(stmt);
                }
            } else {
                fprintf(stderr, "Error: Failed to parse statement at token %zu\n", index);
                ast.root_index = INVALID_NODE_INDEX; // Invalidate AST on error
                break;
            }
        }   

        return std::move(ast);
    }
}