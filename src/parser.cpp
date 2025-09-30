#include "parser.hpp"
#include <cstdio>

#define DBG_PRINT(X, ...) printf(X "\n", ##__VA_ARGS__)

namespace dacite {

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

    auto Parser::get_postfix_binding(Token::Type type) -> std::pair<int, int> {
        switch(type) {
            default:
                return {0, 0};
        }
    }

    auto Parser::parse_expression_bp(int min_bp) -> std::unique_ptr<AST::Node> {
        DBG_PRINT("parse_expression_bp: index=%zu, min_bp=%d", index, min_bp);

        if (is_at_end()) {
            fprintf(stderr, "Error: Unexpected end of tokens while parsing expression at token %zu\n", index);
            return nullptr;
        }

        const auto lhs_token = tokens[index++];
        std::unique_ptr<AST::Node> lhs;
        
        switch (lhs_token.type) {
            case Token::Type::Literal_Number: {
                lhs = std::make_unique<AST::NumberLiteral>(lhs_token);
                DBG_PRINT("lhs is NumberLiteral");
            } break;
            case Token::Type::Identifier: {
                lhs = std::make_unique<AST::Identifier>(lhs_token);
                DBG_PRINT("lhs is Identifier");
            } break;
            case Token::Type::Lparen: {
                lhs = parse_expression_bp(0);
                if(!lhs) {
                    return nullptr;
                }
                if (!consume_token(Token::Type::Rparen, ")")) {
                    return nullptr;
                }
            } break;
            case Token::Type::Minus:
            case Token::Type::Plus: {
                auto [_, r_bp] = get_prefix_binding(lhs_token.type);
                auto rhs = parse_expression_bp(r_bp);
                if(!rhs) {
                    return nullptr;
                }
                auto prefix_node = std::make_unique<AST::UnaryPrefixExpression>(lhs_token);
                prefix_node->children.push_back(std::move(rhs));
                lhs = std::move(prefix_node);
            } break;
            default:
                fprintf(stderr, "Error: Unexpected token type %d at token %zu\n", (int)lhs_token.type, index - 1);
                return nullptr;
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
                if(!rhs) {
                    return nullptr;
                }
                auto binary_node = std::make_unique<AST::BinaryExpression>(op_token);
                binary_node->children.push_back(std::move(lhs));
                binary_node->children.push_back(std::move(rhs));
                lhs = std::move(binary_node);
                continue;
            }
            
            // Check for postfix operators
            if(auto [l_bp, r_bp] = get_postfix_binding(op_token.type); l_bp != 0) {
                if(l_bp < min_bp) {
                    break;
                }
                index++; // consume operator
                auto postfix_node = std::make_unique<AST::UnaryPostfixExpression>(op_token);
                postfix_node->children.push_back(std::move(lhs));
                lhs = std::move(postfix_node);
                continue;
            }
            break; // No more operators
        }

        return lhs;
    }

    auto Parser::parse_expression() -> std::unique_ptr<AST::Node> {
        if (is_at_end()) {
            fprintf(stderr, "Error: Unexpected end of tokens while parsing expression at token %zu\n", index);
            return nullptr;
        }

        return parse_expression_bp(0);
    }

    auto Parser::parse_intrinsic_print() -> std::unique_ptr<AST::Node> {
        DBG_PRINT("parse_intrinsic_print: index=%zu", index);
        
        // Expecting: @print ( <expression> ) ;
        if (!consume_token(Token::Type::Intrinsic_Print, "@print")) {
            return nullptr;
        }

        if (!consume_token(Token::Type::Lparen, "( after @print")) {
            return nullptr;
        }

        auto expression = parse_expression();
        if (!expression) {
            fprintf(stderr, "Error: Failed to parse expression after @print at token %zu\n", index);
            return nullptr;
        }

        if (!consume_token(Token::Type::Rparen, ") after expression")) {
            return nullptr;
        }

        if (!consume_token(Token::Type::Semicolon, "; after )")) {
            return nullptr;
        }

        // Construct AST node for @print statement
        auto print_node = std::make_unique<AST::IntrinsicPrint>();
        print_node->children.push_back(std::move(expression));

        return print_node;
    }

    auto Parser::parse_variable_declaration() -> std::unique_ptr<AST::Node> {
        DBG_PRINT("parse_variable_declaration: index=%zu", index);
        
        // Expecting: let <identifier> : <type> = <expression> ;
        if (!consume_token(Token::Type::Keyword_Let, "let")) {
            return nullptr;
        }

        if (!expect_token(Token::Type::Identifier, "identifier after 'let'")) {
            return nullptr;
        }
        Token identifier_token = tokens[index++]; // consume identifier
        auto identifier_node = std::make_unique<AST::Identifier>(identifier_token);
        DBG_PRINT("Parsed identifier");

        if (!consume_token(Token::Type::Colon, ": after identifier")) {
            return nullptr;
        }

        if (!expect_token(Token::Type::Identifier, "type identifier after ':'")) {
            return nullptr;
        }
        Token type_token = tokens[index++]; // consume type identifier
        auto type_node = std::make_unique<AST::Type>(type_token);
        DBG_PRINT("Parsed type");

        if (!consume_token(Token::Type::Equals, "= after type")) {
            return nullptr;
        }

        auto expression = parse_expression();
        if (!expression) {
            fprintf(stderr, "Error: Failed to parse expression after '=' at token %zu\n", index);
            return nullptr;
        }

        if (!consume_token(Token::Type::Semicolon, "; after expression")) {
            return nullptr;
        }

        // Construct AST node for variable declaration
        auto var_decl_node = std::make_unique<AST::VariableDeclaration>();
        var_decl_node->children.push_back(std::move(identifier_node));
        var_decl_node->children.push_back(std::move(type_node));
        var_decl_node->children.push_back(std::move(expression));

        return var_decl_node;
    }

    auto Parser::parse_statement() -> std::unique_ptr<AST::Node> {
        if (is_at_end()) {
            return nullptr;
        }

        const Token& token = current_token();
        
        switch(token.type) {
            case Token::Type::Unknown: {
                fprintf(stderr, "Error: Unknown token type %d at token %zu\n", (int)token.type, index);
                return nullptr;
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
                if (!expr) {
                    return nullptr;
                }
                if (!consume_token(Token::Type::Semicolon, "; after expression")) {
                    return nullptr;
                }
                return expr;
            } break;
            default: {
                fprintf(stderr, "Error: Unexpected token type %d at token %zu\n", (int)token.type, index);
                return nullptr;
            } break;
        }
    }

    auto Parser::parse() -> AST {
        DBG_PRINT("Parsing %zu tokens", tokens.size());

        ast.root = std::make_unique<AST::Module>();

        while (!is_at_end() && current_token().type != Token::Type::_EOF) {
            auto stmt = parse_statement();
            if (stmt) {
                ast.root->children.push_back(std::move(stmt));
            } else {
                fprintf(stderr, "Error: Failed to parse statement at token %zu\n", index);
                ast.root = nullptr; // Invalidate AST on error
                break;
            }
        }   

        return std::move(ast);
    }
}