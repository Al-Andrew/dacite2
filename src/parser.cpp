#include "parser.hpp"
#include "codegen.hpp"
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
            } else if constexpr (std::is_same_v<T, StringLiteral>) {
                std::string lexeme{node.token.lexeme};
                printf("%*sStringLiteral: %s\n", indent * 2, "", lexeme.c_str());
            } else if constexpr (std::is_same_v<T, CharLiteral>) {
                std::string lexeme{node.token.lexeme};
                printf("%*sCharLiteral: %s\n", indent * 2, "", lexeme.c_str());
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
            } else if constexpr (std::is_same_v<T, FunctionDeclaration>) {
                std::string lexeme{node.name.lexeme};
                printf("%*sFunctionDeclaration: %s\n", indent * 2, "", lexeme.c_str());
                print_node(node.return_type, indent + 1);
                for (NodeIndex param_index : node.parameters) {
                    print_node(param_index, indent + 1);
                }
                print_node(node.body, indent + 1);
            } else if constexpr (std::is_same_v<T, Block>) {
                printf("%*sBlock\n", indent * 2, "");
                for (NodeIndex stmt_index : node.statements) {
                    print_node(stmt_index, indent + 1);
                }
            } else if constexpr (std::is_same_v<T, ReturnStatement>) {
                printf("%*sReturnStatement\n", indent * 2, "");
                print_node(node.expression, indent + 1);
            } else if constexpr (std::is_same_v<T, IntrinsicHalt>) {
                printf("%*sIntrinsicHalt\n", indent * 2, "");
            } else if constexpr (std::is_same_v<T, FunctionCall>) {
                printf("%*sFunctionCall\n", indent * 2, "");
                print_node(node.callee, indent + 1);
                for (NodeIndex arg_index : node.arguments) {
                    print_node(arg_index, indent + 1);
                }
            } else if constexpr (std::is_same_v<T, FunctionParameterDeclaration>) {
                std::string lexeme{node.name.lexeme};
                printf("%*sFunctionParameterDeclaration: %s\n", indent * 2, "", lexeme.c_str());
                print_node(node.type, indent + 1);
            } else if constexpr (std::is_same_v<T, IfStatement>) {
                printf("%*sIfStatement\n", indent * 2, "");
                printf("%*sCondition:\n", (indent + 1) * 2, "");
                print_node(node.condition, indent + 2);
                printf("%*sThen:\n", (indent + 1) * 2, "");
                print_node(node.then_block, indent + 2);
                if (node.else_block != INVALID_NODE_INDEX) {
                    printf("%*sElse:\n", (indent + 1) * 2, "");
                    print_node(node.else_block, indent + 2);
                }
            } else if constexpr (std::is_same_v<T, WhileStatement>) {
                printf("%*sWhileStatement\n", indent * 2, "");
                printf("%*sCondition:\n", (indent + 1) * 2, "");
                print_node(node.condition, indent + 2);
                printf("%*sBody:\n", (indent + 1) * 2, "");
                print_node(node.body, indent + 2);
            }
            else {
                printf("%*s(unknown node type)\n", indent * 2, "");
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
            case Token::Type::DoubleEquals:
            case Token::Type::NotEquals:
                return {3, 4};
            case Token::Type::LessThan:
            case Token::Type::GreaterThan:
                return {3, 4};
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
            case Token::Type::Lparen:
                return {7, 0};
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
                DBG_PRINT("lhs is NumberLiteral: %.*s", (int)lhs_token.lexeme.size(), lhs_token.lexeme.data());
                lhs = ast.add_node(NumberLiteral(lhs_token));
            } break;
            case Token::Type::Literal_String: {
                DBG_PRINT("lhs is StringLiteral: %.*s", (int)lhs_token.lexeme.size(), lhs_token.lexeme.data());
                lhs = ast.add_node(StringLiteral(lhs_token));
            } break;
            case Token::Type::Literal_Char: {
                DBG_PRINT("lhs is CharLiteral: %.*s", (int)lhs_token.lexeme.size(), lhs_token.lexeme.data());
                lhs = ast.add_node(CharLiteral(lhs_token));
            } break;
            case Token::Type::Identifier: {
                lhs = ast.add_node(Identifier(lhs_token));
                DBG_PRINT("lhs is Identifier: %.*s", (int)lhs_token.lexeme.size(), lhs_token.lexeme.data());
            } break;
            case Token::Type::Lparen: {
                DBG_PRINT("lhs is Lparen: %.*s", (int)lhs_token.lexeme.size(), lhs_token.lexeme.data());
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
                DBG_PRINT("lhs is UnaryPrefix operator: %.*s", (int)lhs_token.lexeme.size(), lhs_token.lexeme.data());
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
            DBG_PRINT("Considering operator %.*s at index %zu", (int)op_token.lexeme.size(), op_token.lexeme.data(), index);
            
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

            if(auto [l_bp, _] = get_postfix_binding(op_token.type); l_bp != 0) {
                
                if(op_token.type == Token::Type::Lparen) {
                    lhs = parse_function_call(lhs);
                    continue;
                } else {
                    fprintf(stderr, "Error: Unknown postfix operator %d at token %zu\n", (int)op_token.type, index - 1);
                    return INVALID_NODE_INDEX;
                }
            }

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

    auto Parser::parse_function_call(NodeIndex callee) -> NodeIndex {
        DBG_PRINT("parse_function_call: index=%zu", index);
        
        // Expecting: <callee> ( <arg1>, <arg2>, ... )
        if (!consume_token(Token::Type::Lparen, "( after function name")) {
            return INVALID_NODE_INDEX;
        }

        FunctionCall func_call_node(callee);

        while(!is_at_end() && current_token().type != Token::Type::Rparen) {
            auto arg = parse_expression();
            if (arg == INVALID_NODE_INDEX) {
                return INVALID_NODE_INDEX;
            }
            func_call_node.arguments.push_back(arg);

            if (current_token().type == Token::Type::Comma) {
                index++; // consume comma
            } else {
                break; // no more arguments
            }
        }

        if (!consume_token(Token::Type::Rparen, ") after arguments")) {
            return INVALID_NODE_INDEX;
        }

        return ast.add_node(std::move(func_call_node));
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
            case Token::Type::Intrinsic_Halt: {
                return parse_halt_statement();
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
            case Token::Type::Keyword_Return: {
                return parse_return_statement();
            } break;
            case Token::Type::Keyword_If: {
                return parse_if_statement();
            } break;
            case Token::Type::Keyword_While: {
                return parse_while_statement();
            } break;
            case Token::Type::Lbrace: {
                return parse_block();
            } break;
            default: {
                fprintf(stderr, "Error: Unexpected token type %d at token %zu\n", (int)token.type, index);
                return INVALID_NODE_INDEX;
            } break;
        }
    }

    auto Parser::parse_block() -> NodeIndex {
        DBG_PRINT("parse_block: index=%zu", index);
        
        if (!consume_token(Token::Type::Lbrace, "{ to start block")) {
            return INVALID_NODE_INDEX;
        }

        Block block_node;

        while (!is_at_end() && current_token().type != Token::Type::Rbrace) {
            auto stmt = parse_statement();
            if (stmt == INVALID_NODE_INDEX) {
                return INVALID_NODE_INDEX;
            }
            block_node.statements.push_back(stmt);
        }

        if (!consume_token(Token::Type::Rbrace, "} to end block")) {
            return INVALID_NODE_INDEX;
        }

        return ast.add_node(std::move(block_node));
    }

    auto Parser::parse_function_declaration() -> NodeIndex {
        DBG_PRINT("parse_function_declaration: index=%zu", index);
        
        // Expecting: fun <identifier> : ([<argname>: <argtype>], ...) -> <type> = <block>
        if (!consume_token(Token::Type::Keyword_Fun, "fun")) {
            return INVALID_NODE_INDEX;
        }

        if (!expect_token(Token::Type::Identifier, "function name after 'fun'")) {
            return INVALID_NODE_INDEX;
        }
        Token name_token = tokens[index++]; // consume function name
        DBG_PRINT("Parsed function name: %.*s", (int)name_token.lexeme.size(), name_token.lexeme.data());

        FunctionDeclaration func_decl_node(name_token);


        if (!consume_token(Token::Type::Colon, ": after function name")) {
            return INVALID_NODE_INDEX;
        }

        // TODO: parse parameters
        if (!consume_token(Token::Type::Lparen, "( after ':'")) {
            return INVALID_NODE_INDEX;
        }

        while (!is_at_end() && current_token().type != Token::Type::Rparen) {
            
            if(!expect_token(Token::Type::Identifier, "parameter name")) {
                return INVALID_NODE_INDEX;
            }
            Token param_name_token = tokens[index++]; // consume parameter name
            DBG_PRINT("Parsed parameter name: %.*s", (int)param_name_token.lexeme.size(), param_name_token.lexeme.data());

            if (!consume_token(Token::Type::Colon, ": after parameter name")) {
                return INVALID_NODE_INDEX;
            }

            if(!expect_token(Token::Type::Identifier, "parameter type")) {
                return INVALID_NODE_INDEX;
            }
            Token param_type_token = tokens[index++]; // consume parameter type // TODO: support complex types
            DBG_PRINT("Parsed parameter type: %.*s", (int)param_type_token.lexeme.size(), param_type_token.lexeme.data());

            auto param_type_index = ast.add_node(Type(param_type_token));
            FunctionParameterDeclaration param_decl_node(param_name_token, param_type_index);
            func_decl_node.parameters.push_back(ast.add_node(std::move(param_decl_node)));

        }

        if (!consume_token(Token::Type::Rparen, ") after '('")) {
            return INVALID_NODE_INDEX;
        }

        if (!consume_token(Token::Type::Arrow, "-> after ')'")) {
            return INVALID_NODE_INDEX;
        }

        if (!expect_token(Token::Type::Identifier, "return type after '->'")) {
            return INVALID_NODE_INDEX;
        }
        // TODO: support complex return types
        Token return_type_token = tokens[index++]; // consume return type
        auto return_type_index = ast.add_node(Type(return_type_token));
        DBG_PRINT("Parsed return type: %.*s", (int)return_type_token.lexeme.size(), return_type_token.lexeme.data());

        func_decl_node.return_type = return_type_index;

        if (!consume_token(Token::Type::Equals, "= after return type")) {
            return INVALID_NODE_INDEX;
        }

        auto body = parse_block();
        if (body == INVALID_NODE_INDEX) {
            fprintf(stderr, "Error: Failed to parse function body at token %zu\n", index);
            return INVALID_NODE_INDEX;
        }

        func_decl_node.body = body;

        if(expect_token(Token::Type::Semicolon, "; after function body")) {
            consume_token(Token::Type::Semicolon, "; after function body");
        }

        // Construct AST node for function declaration
        return ast.add_node(std::move(func_decl_node));
    }

    auto Parser::parse() -> AST {
        DBG_PRINT("Parsing %zu tokens", tokens.size());

        ast.root_index = ast.add_node(Module());
        
        while (!is_at_end() && current_token().type != Token::Type::_EOF) {
            auto stmt = parse_function_declaration();
            
            if (stmt != INVALID_NODE_INDEX) {
                auto* module_node = ast.get_node<Module>(ast.root_index);
                if (!module_node) {
                    fprintf(stderr, "Error: Failed to create root Module node\n");
                    return std::move(ast);
                }
                module_node->statements.push_back(stmt);
            } else {
                fprintf(stderr, "Error: Failed to parse statement at token %zu\n", index);
                ast.root_index = INVALID_NODE_INDEX; // Invalidate AST on error
                break;
            }
        }   

        return std::move(ast);
    }

    auto Parser::parse_return_statement() -> NodeIndex {
        DBG_PRINT("parse_return_statement: index=%zu", index);
        
        // Expecting: return <expression> ;
        if (!consume_token(Token::Type::Keyword_Return, "return")) {
            return INVALID_NODE_INDEX;
        }

        auto expression = parse_expression();
        if (expression == INVALID_NODE_INDEX) {
            fprintf(stderr, "Error: Failed to parse expression after 'return' at token %zu\n", index);
            return INVALID_NODE_INDEX;
        }

        if (!consume_token(Token::Type::Semicolon, "; after expression")) {
            return INVALID_NODE_INDEX;
        }

        // Construct AST node for return statement
        ReturnStatement return_node(expression);
        return ast.add_node(std::move(return_node));
    }

    auto Parser::parse_halt_statement() -> NodeIndex {
        DBG_PRINT("parse_halt_statement: index=%zu", index);
        
        // Expecting: @halt();
        if (!consume_token(Token::Type::Intrinsic_Halt, "@halt")) {
            return INVALID_NODE_INDEX;
        }

        if (!consume_token(Token::Type::Lparen, "( after @halt")) {
            return INVALID_NODE_INDEX;
        }

        if (!consume_token(Token::Type::Rparen, ") after @halt")) {
            return INVALID_NODE_INDEX;
        }

        if (!consume_token(Token::Type::Semicolon, "; after @halt")) {
            return INVALID_NODE_INDEX;
        }

        // Construct AST node for halt statement
        IntrinsicHalt halt_node;
        return ast.add_node(std::move(halt_node));
    }

    auto Parser::parse_if_statement() -> NodeIndex {
        DBG_PRINT("parse_if_statement: index=%zu", index);
        
        // Expecting: if ( <expression> ) <block> [else <block>]
        if (!consume_token(Token::Type::Keyword_If, "if")) {
            return INVALID_NODE_INDEX;
        }

        if (!consume_token(Token::Type::Lparen, "( after if")) {
            return INVALID_NODE_INDEX;
        }

        auto condition = parse_expression();
        if (condition == INVALID_NODE_INDEX) {
            fprintf(stderr, "Error: Failed to parse condition after 'if' at token %zu\n", index);
            return INVALID_NODE_INDEX;
        }

        if (!consume_token(Token::Type::Rparen, ") after condition")) {
            return INVALID_NODE_INDEX;
        }

        auto then_block = parse_block();
        if (then_block == INVALID_NODE_INDEX) {
            fprintf(stderr, "Error: Failed to parse then block at token %zu\n", index);
            return INVALID_NODE_INDEX;
        }

        // Construct AST node for if statement
        IfStatement if_node;
        if_node.condition = condition;
        if_node.then_block = then_block;
        if_node.else_block = INVALID_NODE_INDEX;

        // Check for optional else clause
        if (!is_at_end() && current_token().type == Token::Type::Keyword_Else) {
            consume_token(Token::Type::Keyword_Else, "else");
            
            auto else_block = parse_block();
            if (else_block == INVALID_NODE_INDEX) {
                fprintf(stderr, "Error: Failed to parse else block at token %zu\n", index);
                return INVALID_NODE_INDEX;
            }
            
            if_node.else_block = else_block;
        }

        return ast.add_node(std::move(if_node));
    }

    auto Parser::parse_while_statement() -> NodeIndex {
        DBG_PRINT("parse_while_statement: index=%zu", index);
        
        // Expecting: while ( <expression> ) <block>
        if (!consume_token(Token::Type::Keyword_While, "while")) {
            return INVALID_NODE_INDEX;
        }

        if (!consume_token(Token::Type::Lparen, "( after while")) {
            return INVALID_NODE_INDEX;
        }

        auto condition = parse_expression();
        if (condition == INVALID_NODE_INDEX) {
            fprintf(stderr, "Error: Failed to parse condition after 'while' at token %zu\n", index);
            return INVALID_NODE_INDEX;
        }

        if (!consume_token(Token::Type::Rparen, ") after condition")) {
            return INVALID_NODE_INDEX;
        }

        auto body = parse_block();
        if (body == INVALID_NODE_INDEX) {
            fprintf(stderr, "Error: Failed to parse while body at token %zu\n", index);
            return INVALID_NODE_INDEX;
        }

        // Construct AST node for while statement
        WhileStatement while_node;
        while_node.condition = condition;
        while_node.body = body;

        return ast.add_node(std::move(while_node));
    }

}