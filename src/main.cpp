#include <cmath>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>

#include "lexer.hpp"

using dacite::Token;

#define DBG_PRINT(X, ...) printf(X "\n", ##__VA_ARGS__)

#define ENABLE_TOKEN_DEBUG 1

template<class F>
struct Defer {
    Defer(F&& f) : func(std::forward<F>(f)) {}
    ~Defer() { func(); }
    F func;
};

void print_usage() {
    printf("Usage: <command> <arg1> <arg2>\n");

    printf("Commands:\n");
    printf("  run <script> \n");
    printf("    Runs the specified script.\n");
}

const char* read_file_to_allocated_buffer(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if(!file) {
        fprintf(stderr, "Error: Could not open file %s\n", filename);
        return nullptr;
    }
    Defer fclose_file([file]() { fclose(file); });

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* buffer = (char*)malloc(length + 1);
    if(!buffer) {
        fprintf(stderr, "Error: Could not allocate memory for file %s\n", filename);
        return nullptr;
    }

    size_t read_length = fread(buffer, 1, length, file);
    if(read_length != length) {
        fprintf(stderr, "Error: Could not read file %s\n", filename);
        free(buffer);
        return nullptr;
    }
    buffer[length] = '\0'; // Null-terminate the buffer

    return buffer;
}

struct CompiledModule {
    bool is_valid() const {
        return bytecode.size() > 0;
    }

    std::vector<uint32_t> bytecode;
    std::vector<uint64_t> constants;
};

struct AST {
    bool is_valid() const {
        return root != nullptr;
    }

    struct Node {
        virtual ~Node() = default;
        virtual void print(int indent = 0) const {
            printf("%*sNode (base)\n", indent * 2, "");
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        virtual const char* get_type_name() const { return "Node"; }
        std::vector<std::unique_ptr<Node>> children;
    };

    struct Module : public Node {
        void print(int indent = 0) const override {
            printf("%*sModule\n", indent * 2, "");
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        const char* get_type_name() const override { return "Module"; }
    };

    struct Statement : public Node {
        void print(int indent = 0) const override {
            printf("%*sStatement\n", indent * 2, "");
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        const char* get_type_name() const override { return "Statement"; }
    };

    struct VariableDeclaration : public Statement {
        void print(int indent = 0) const override {
            printf("%*sVariableDeclaration\n", indent * 2, "");
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        const char* get_type_name() const override { return "VariableDeclaration"; }
    };

    struct IntrinsicPrint : public Statement {
        void print(int indent = 0) const override {
            printf("%*sIntrinsicPrint (@print)\n", indent * 2, "");
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        const char* get_type_name() const override { return "IntrinsicPrint"; }
    };

    struct NumberLiteral : public Node {
        NumberLiteral(const Token& t) : token(t) {}
        
        void print(int indent = 0) const override {
            // Extract the actual number value from the token
            std::string lexeme{token.lexeme};
            printf("%*sNumberLiteral: %s\n", indent * 2, "", lexeme.c_str());
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        
        const char* get_type_name() const override { return "NumberLiteral"; }
        
        void set_source_ref(const char* source) { source_ref = source; }
        
        Token token;
        const char* source_ref = nullptr;
    };

    struct Identifier : public Node {
        Identifier(const Token& t) : token(t) {}
        
        void print(int indent = 0) const override {
            // Extract the actual identifier name from the token
            std::string lexeme{token.lexeme};
            printf("%*sIdentifier: %s\n", indent * 2, "", lexeme.c_str());
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        
        const char* get_type_name() const override { return "Identifier"; }
        
        void set_source_ref(const char* source) { source_ref = source; }
        
        Token token;
        const char* source_ref = nullptr;
    };

    struct Type : public Identifier {
        Type(const Token& t) : Identifier(t) {}

        void print(int indent = 0) const override {
            printf("%*sType\n", indent * 2, "");
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        const char* get_type_name() const override { return "Type"; }
    };

    struct Expression : public Node {
        void print(int indent = 0) const override {
            printf("%*sExpression\n", indent * 2, "");
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        const char* get_type_name() const override { return "Expression"; }
    };

    struct BinaryExpression : public Expression {
        BinaryExpression(const Token& op) : operator_token(op) {}
        
        void print(int indent = 0) const override {
            std::string lexeme{operator_token.lexeme};
            printf("%*sBinaryExpression: %s\n", indent * 2, "", lexeme.c_str());
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        
        const char* get_type_name() const override { return "BinaryExpression"; }
        
        void set_source_ref(const char* source) { source_ref = source; }
        
        Token operator_token;
        const char* source_ref = nullptr;
    };

    struct UnaryPrefixExpression : public Expression {
        UnaryPrefixExpression(const Token& op) : operator_token(op) {}
        
        void print(int indent = 0) const override {
            std::string lexeme{operator_token.lexeme};
            printf("%*sUnaryPrefixExpression: %s\n", indent * 2, "", lexeme.c_str());
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        
        const char* get_type_name() const override { return "UnaryPrefixExpression"; }
        
        void set_source_ref(const char* source) { source_ref = source; }
        
        Token operator_token;
        const char* source_ref = nullptr;
    };

     struct UnaryPostfixExpression : public Expression {
        UnaryPostfixExpression(const Token& op) : operator_token(op) {}
        
        void print(int indent = 0) const override {
            std::string lexeme{operator_token.lexeme};
            printf("%*sUnaryPostfixExpression: %s\n", indent * 2, "", lexeme.c_str());
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        
        const char* get_type_name() const override { return "UnaryPostfixExpression"; }
        
        void set_source_ref(const char* source) { source_ref = source; }
        
        Token operator_token;
        const char* source_ref = nullptr;
    };

    std::unique_ptr<Node> root;
    char const* source;
    
    // Utility function to print the entire AST
    void print_ast() const {
        printf("=== AST Structure ===\n");
        if (root) {
            root->print(0);
        } else {
            printf("(empty AST)\n");
        }
        printf("===================\n");
    }
};


std::pair<int, int> get_infix_binding(Token::Type type) {
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

std::pair<int, int> get_prefix_binding(Token::Type type) {
    switch(type) {
        case Token::Type::Minus:
            return {0, 9};
        case Token::Type::Plus:
            return {0, 9};
        default:
            return {0, 0};
    }
}

std::pair<int, int> get_postfix_binding(Token::Type type) {
    switch(type) {
        default:
            return {0, 0};
    }
}


std::unique_ptr<AST::Node> parse_expression_bp(AST& ast, const std::vector<Token>& tokens, char const* const source, size_t& index, int min_bp) {
    DBG_PRINT("parse_expression_bp: index=%zu, min_bp=%d", index, min_bp);

    const auto lhs_token = tokens[index++];
    std::unique_ptr<AST::Node> lhs;
    switch (lhs_token.type) {
        case Token::Type::Literal_Number: {
            auto number_node = std::make_unique<AST::NumberLiteral>(lhs_token);
            number_node->set_source_ref(source);
            lhs = std::move(number_node);
            DBG_PRINT("lhs is NumberLiteral");
        } break;
        case Token::Type::Identifier: {
            auto identifier_node = std::make_unique<AST::Identifier>(lhs_token);
            identifier_node->set_source_ref(source);
            lhs = std::move(identifier_node);
            DBG_PRINT("lhs is Identifier");
        } break;
        case Token::Type::Lparen: {
            lhs = parse_expression_bp(ast, tokens, source, index, 0);
            if(!lhs) {
                return nullptr;
            }
            if(index >= tokens.size() || tokens[index].type != Token::Type::Rparen) {
                fprintf(stderr, "Error: Expected ) at token %zu\n", index);
                return nullptr;
            }
            index++; // consume )
        } break;
        case Token::Type::Minus:
        case Token::Type::Plus: {
            auto [_, r_bp] = get_prefix_binding(lhs_token.type);
            auto rhs = parse_expression_bp(ast, tokens, source, index, r_bp);
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

    while(index < tokens.size()) {
        const auto op_token = tokens[index];
        DBG_PRINT("Considering operator token type %d at index %zu", (int)op_token.type, index);
        // Check for infix operators
        if(auto [l_bp, r_bp] = get_infix_binding(op_token.type); l_bp != 0) {
            if(l_bp < min_bp) {
                break;
            }
            index++; // consume operator
            auto rhs = parse_expression_bp(ast, tokens, source, index, r_bp);
            if(!rhs) {
                return nullptr;
            }
            auto binary_node = std::make_unique<AST::BinaryExpression>(op_token);
            binary_node->set_source_ref(source);
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
            postfix_node->set_source_ref(source);
            postfix_node->children.push_back(std::move(lhs));
            lhs = std::move(postfix_node);
            continue;
        }
        break; // No more operators
    }

    return lhs;
}

std::unique_ptr<AST::Node> parse_expression(AST& ast, const std::vector<Token>& tokens, char const* const source, size_t& index) {
    if(index >= tokens.size()) {
        fprintf(stderr, "Error: Unexpected end of tokens while parsing expression at token %zu\n", index);
        return nullptr;
    }

    return parse_expression_bp(ast, tokens, source, index, 0);
}

std::unique_ptr<AST::Node> parse_intrinsic_print(AST& ast, const std::vector<Token>& tokens, char const* const source, size_t& index) {
    DBG_PRINT("parse_intrinsic_print: index=%zu", index);
    
    // Expecting: @print ( <number> ) ;
    if(index >= tokens.size() || tokens[index].type != Token::Type::Intrinsic_Print) {
        fprintf(stderr, "Error: Expected @print at token %zu\n", index);
        return nullptr;
    }
    index++; // consume @print

    if(index >= tokens.size() || tokens[index].type != Token::Type::Lparen) {
        fprintf(stderr, "Error: Expected ( after @print at token %zu\n", index);
        return nullptr;
    }
    index++; // consume (

    std::unique_ptr<AST::Node> expression = parse_expression(ast, tokens, source, index);
    if(!expression) {
        fprintf(stderr, "Error: Failed to parse expression after @print at token %zu\n", index);
        return nullptr;
    }

    if(index >= tokens.size() || tokens[index].type != Token::Type::Rparen) {
        fprintf(stderr, "Error: Expected ) after number literal at token %zu\n", index);
        return nullptr;
    }
    index++; // consume )

    if(index >= tokens.size() || tokens[index].type != Token::Type::Semicolon) {
        fprintf(stderr, "Error: Expected ; after ) at token %zu\n", index);
        return nullptr;
    }
    index++; // consume ;

    // Construct AST node for @print statement
    std::unique_ptr<AST::IntrinsicPrint> print_node = std::make_unique<AST::IntrinsicPrint>();
    print_node->children.push_back(std::move(expression));

    return print_node;
}

std::unique_ptr<AST::Node> parse_variable_declaration(AST& ast, const std::vector<Token>& tokens, char const* const source, size_t& index) {
    DBG_PRINT("parse_variable_declaration: index=%zu", index);
    
    // Expecting: let <identifier> : <type> = <expression> ;
    if(index >= tokens.size() || tokens[index].type != Token::Type::Keyword_Let) {
        fprintf(stderr, "Error: Expected 'let' at token %zu\n", index);
        return nullptr;
    }
    index++; // consume 'let'

    if(index >= tokens.size() || tokens[index].type != Token::Type::Identifier) {
        fprintf(stderr, "Error: Expected identifier after 'let' at token %zu\n", index);
        return nullptr;
    }
    Token identifier_token = tokens[index++]; // consume identifier
    auto identifier_node = std::make_unique<AST::Identifier>(identifier_token);
    identifier_node->set_source_ref(source);
    DBG_PRINT("Parsed identifier");

    if(index >= tokens.size() || tokens[index].type != Token::Type::Colon) {
        fprintf(stderr, "Error: Expected ':' after identifier at token %zu\n", index);
        return nullptr;
    }
    index++; // consume ':'

    if(index >= tokens.size() || tokens[index].type != Token::Type::Identifier) {
        fprintf(stderr, "Error: Expected type identifier after ':' at token %zu\n", index);
        return nullptr;
    }
    Token type_token = tokens[index++]; // consume type identifier
    auto type_node = std::make_unique<AST::Type>(type_token);
    type_node->set_source_ref(source);
    DBG_PRINT("Parsed type");

    if(index >= tokens.size() || tokens[index].type != Token::Type::Equals) {
        fprintf(stderr, "Error: Expected '=' after identifier at token %zu\n", index);
        return nullptr;
    }
    index++; // consume '='

    std::unique_ptr<AST::Node> expression = parse_expression(ast, tokens, source, index);
    if(!expression) {
        fprintf(stderr, "Error: Failed to parse expression after '=' at token %zu\n", index);
        return nullptr;
    }

    if(index >= tokens.size() || tokens[index].type != Token::Type::Semicolon) {
        fprintf(stderr, "Error: Expected ';' after expression at token %zu\n", index);
        return nullptr;
    }

    index++; // consume ';'

    // Construct AST node for variable declaration
    std::unique_ptr<AST::VariableDeclaration> var_decl_node = std::make_unique<AST::VariableDeclaration>();
    var_decl_node->children.push_back(std::move(identifier_node));
    var_decl_node->children.push_back(std::move(type_node));
    var_decl_node->children.push_back(std::move(expression));

    return var_decl_node;
}

std::unique_ptr<AST::Node> parse_statement(AST& ast, const std::vector<Token>& tokens, char const* const source, size_t& index) {
    if(index >= tokens.size()) {
        return nullptr;
    }

    const Token& token = tokens[index];
    
    switch(token.type) {
        case Token::Type::Unknown: {
            fprintf(stderr, "Error: Unknown token type %d at token %zu\n", (int)token.type, index);
            return nullptr;
        } break;
        case Token::Type::Intrinsic_Print: {
            return parse_intrinsic_print(ast, tokens, source, index);
        } break;
        case Token::Type::Keyword_Let: {
            return parse_variable_declaration(ast, tokens, source, index);
        } break;
        case Token::Type::Identifier:
        case Token::Type::Literal_Number: {
            std::unique_ptr<AST::Node> expr = parse_expression(ast, tokens, source, index);
            if(!expr) {
                return nullptr;
            }
            if(index >= tokens.size() || tokens[index].type != Token::Type::Semicolon) {
                fprintf(stderr, "Error: Expected ';' after expression at token %zu\n", index);
                return nullptr;
            }
            index++; // consume ';' 
            return expr;
        } break;
        default: {
            fprintf(stderr, "Error: Unexpected token type %d at token %zu\n", (int)token.type, index);
            return nullptr;
        } break;
    }
}

AST parse(const std::vector<Token>& tokens, const char* source) {
    // Dummy parse function
    DBG_PRINT("Parsing %zu tokens", tokens.size());

    AST ast = {};
    ast.root = std::make_unique<AST::Module>();
    ast.source = source;
    size_t index = 0;

    while(index < tokens.size() && tokens[index].type != Token::Type::_EOF) {
        std::unique_ptr<AST::Node> stmt = parse_statement(ast, tokens, source, index);
        if(stmt) {
            ast.root->children.push_back(std::move(stmt));
        } else {
            fprintf(stderr, "Error: Failed to parse statement at token %zu\n", index);
            ast.root = nullptr; // Invalidate AST on error
            break;
        }
    }   

    return ast;
}

enum class BytecodeOp : uint32_t {
    PRINT = 1,
    ADD = 2,
    SUBTRACT = 3,
    MULTIPLY = 4,
    DIVIDE = 5,
    PUSH_CONST = 6,
    POP = 7,
    RESERVE = 8, // <how much> 
    STORE = 9, // <where>
    LOAD = 10, // <where>
};

CompiledModule codegen_from_ast(const AST& ast) {
    // Dummy codegen function
    DBG_PRINT("Generating code from AST");

    CompiledModule module;

    std::map<std::string, uint32_t> variable_stack_offset_table{};

    // dfs nodes
    std::function<void(const AST::Node*)> dfs;
    dfs = [&](const AST::Node* node) {
        if(const AST::Module* module_node = dynamic_cast<const AST::Module*>(node); module_node) {
            // Traverse children
            for(const auto& child : module_node->children) {
                if(child) {
                    dfs(child.get());
                }
            }
            return;
        }
        if(const AST::VariableDeclaration* var_decl_node = dynamic_cast<const AST::VariableDeclaration*>(node); var_decl_node) {
            // Generate bytecode for variable declaration
            if(var_decl_node->children.size() != 3) {
                fprintf(stderr, "Error: VariableDeclaration node has %zu children, expected 3\n", var_decl_node->children.size());
                return;
            }
            const AST::Node* identifier = var_decl_node->children[0].get();
            const AST::Node* type = var_decl_node->children[1].get();
            const AST::Node* expression = var_decl_node->children[2].get();

            const AST::Identifier* id_node = dynamic_cast<const AST::Identifier*>(identifier);
            if(!id_node) {
                fprintf(stderr, "Error: First child of VariableDeclaration is not Identifier\n");
                return;
            }
            std::string var_name{id_node->token.lexeme};
            DBG_PRINT("Declaring variable: %s", var_name.c_str());
            if(variable_stack_offset_table.find(var_name) != variable_stack_offset_table.end()) {
                fprintf(stderr, "Error: Variable %s already declared\n", var_name.c_str());
                return;
            }
            // Reserve space on stack for variable
            uint32_t var_offset = variable_stack_offset_table.size();
            variable_stack_offset_table[var_name] = var_offset * sizeof(uint64_t);

            module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::RESERVE));
            module.bytecode.push_back(sizeof(uint64_t)); // reserve 8 bytes for u64

            // Generate code for expression
            dfs(expression);
            
            // Store top of stack into variable location
            module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::STORE));
            module.bytecode.push_back(var_offset * sizeof(uint64_t));
            
            return;
        }

        if(const AST::Identifier* id_node = dynamic_cast<const AST::Identifier*>(node); id_node) {
            // Load variable value onto stack
            std::string var_name{id_node->token.lexeme};
            DBG_PRINT("Loading variable: %s", var_name.c_str());
            auto it = variable_stack_offset_table.find(var_name);
            if(it == variable_stack_offset_table.end()) {
                fprintf(stderr, "Error: Variable %s not declared\n", var_name.c_str());
                return;
            }
            uint32_t var_offset = it->second;
            module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::LOAD));
            module.bytecode.push_back(var_offset);
            return;
        }
        
        if(const AST::IntrinsicPrint* print_node = dynamic_cast<const AST::IntrinsicPrint*>(node); print_node) {
            // Generate bytecode for @print
            if(print_node->children.size() != 1) {
                fprintf(stderr, "Error: @print node has %zu children, expected 1\n", print_node->children.size());
                return;
            }
            const AST::Node* child = print_node->children[0].get();
            dfs(child);
            module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::PRINT));
            return;
        }
        
        if(const AST::NumberLiteral* number_node = dynamic_cast<const AST::NumberLiteral*>(node); number_node) {
            // Convert number token to integer
            uint64_t value = strtoull(number_node->token.lexeme.data(), nullptr, 10);
            // Add constant to module
            uint32_t const_index = module.constants.size();
            module.constants.push_back(value);
            // Generate bytecode to push constant
            module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::PUSH_CONST));
            module.bytecode.push_back(const_index);
            return;
        }
        
        if(const AST::BinaryExpression* binary_node = dynamic_cast<const AST::BinaryExpression*>(node); binary_node) {
            if(binary_node->children.size() != 2) {
                fprintf(stderr, "Error: BinaryExpression node has %zu children, expected 2\n", binary_node->children.size());
                return;
            }
            const AST::Node* lhs = binary_node->children[0].get();
            const AST::Node* rhs = binary_node->children[1].get();
            
            if(binary_node->operator_token.type == Token::Type::Equals) {
                // For assignment, rhs is evaluated first
                dfs(rhs);
            }
            else {
                dfs(lhs);
                dfs(rhs);
            }

            switch(binary_node->operator_token.type) {
                case Token::Type::Plus:
                    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::ADD));
                    break;
                case Token::Type::Minus:
                    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::SUBTRACT));
                    break;
                case Token::Type::Star:
                    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::MULTIPLY));
                    break;
                case Token::Type::Slash:
                    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::DIVIDE));
                    break;
                case Token::Type::Equals:
                    if(!lhs) {
                        fprintf(stderr, "Error: Assignment target is null\n");
                        return;
                    }
                    if(const AST::Identifier* id_node = dynamic_cast<const AST::Identifier*>(lhs); id_node) {
                        std::string var_name{id_node->token.lexeme};
                        DBG_PRINT("Assigning to variable: %s", var_name.c_str());
                        auto it = variable_stack_offset_table.find(var_name);
                        if(it == variable_stack_offset_table.end()) {
                            fprintf(stderr, "Error: Variable %s not declared\n", var_name.c_str());
                            return;
                        }
                        uint32_t var_offset = it->second;
                        module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::STORE));
                        module.bytecode.push_back(var_offset);
                    } else {
                        fprintf(stderr, "Error: Assignment target is not an Identifier\n");
                        return;
                    }
                    break;
                default:
                    fprintf(stderr, "Error: Unknown binary operator token type %d\n", (int)binary_node->operator_token.type);
                    return;
            }
            return;
        }
        
        if(const AST::UnaryPrefixExpression* prefix_node = dynamic_cast<const AST::UnaryPrefixExpression*>(node); prefix_node) {
            if(prefix_node->children.size() != 1) {
                fprintf(stderr, "Error: UnaryPrefixExpression node has %zu children, expected 1\n", prefix_node->children.size());
                return;
            }
            const AST::Node* operand = prefix_node->children[0].get();
            dfs(operand);
            switch(prefix_node->operator_token.type) {
                case Token::Type::Minus:
                    // Negate the top of the stack: PUSH_CONST 0; SUBTRACT
                    module.constants.push_back(0);
                    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::PUSH_CONST));
                    module.bytecode.push_back(module.constants.size() - 1);
                    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::SUBTRACT));
                    break;
                case Token::Type::Plus:
                    // No-op for unary plus
                    break;
                default:
                    fprintf(stderr, "Error: Unknown unary prefix operator token type %d\n", (int)prefix_node->operator_token.type);
                    return;
            }
            return;
        }
    };

    dfs(ast.root.get());

    DBG_PRINT("Generated module with %zu constants", module.constants.size());

    return module;
}

CompiledModule compile(const char* source) {    
    // Dummy compilation function
    DBG_PRINT("Compiling source:\n%s", source);

    auto lexer = dacite::Lexer::with_source(source);
    std::vector<Token> tokens = lexer.tokenize_all();

    if constexpr(ENABLE_TOKEN_DEBUG) {
        for(const Token& token : tokens) {
            // DBG_PRINT("Token: type=%d, length=%d, pos=%d, lexeme=%.*s", (int)token.type, token.lexeme.length(), token., token.lexeme.length(), token.lexeme.data());
            DBG_PRINT("Token[%d:%d]: type = %s, lexeme = %.*s", token.line, token.column, dacite::token_type_to_string_map[token.type].data(), (int)token.lexeme.length(), token.lexeme.data());
        }
    }

    AST ast = parse(tokens, source);
    if(!ast.is_valid()) {
        fprintf(stderr, "Error: Parsing failed\n");
        return CompiledModule{};
    }
    
    // Print the AST structure
    ast.print_ast();

    CompiledModule module = codegen_from_ast(ast);
    return module;
}


struct VM {
    bool load_module(const CompiledModule& module) {
        if(!module.is_valid()) {
            fprintf(stderr, "Error: Invalid module\n");
            return false;
        }
        modules.push_back(module);
        return true;
    }

    bool run() {
        uint32_t current_module_index = 0;
        if(modules.size() == 0) {
            fprintf(stderr, "Error: No module loaded\n");
            return false;
        }

        const CompiledModule& module = modules[current_module_index];
        size_t pc = 0; // program counter
        while(pc < module.bytecode.size()) {
            BytecodeOp op = static_cast<BytecodeOp>(module.bytecode[pc]);
            switch(op) {
                case BytecodeOp::PRINT: {
                    if(stack.size() < 1) {
                        fprintf(stderr, "Error: PRINT instruction requires at least 1 value on the stack\n");
                        return false;
                    }
                    uint64_t value = stack.back();
                    stack.pop_back();
                    printf("%llu\n", value);
                    pc += 1;
                } break;
                case BytecodeOp::ADD: {
                    if(stack.size() < 2) {
                        fprintf(stderr, "Error: ADD instruction requires at least 2 values on the stack\n");
                        return false;
                    }
                    uint64_t b = stack.back(); stack.pop_back();
                    uint64_t a = stack.back(); stack.pop_back();
                    stack.push_back(a + b);
                    pc += 1;
                } break;
                case BytecodeOp::SUBTRACT: {
                    if(stack.size() < 2) {
                        fprintf(stderr, "Error: SUBTRACT instruction requires at least 2 values on the stack\n");
                        return false;
                    }
                    uint64_t b = stack.back(); stack.pop_back();
                    uint64_t a = stack.back(); stack.pop_back();
                    stack.push_back(a - b);
                    pc += 1;
                } break;
                case BytecodeOp::MULTIPLY: {
                    if(stack.size() < 2) {
                        fprintf(stderr, "Error: MULTIPLY instruction requires at least 2 values on the stack\n");
                        return false;
                    }
                    uint64_t b = stack.back(); stack.pop_back();
                    uint64_t a = stack.back(); stack.pop_back();
                    stack.push_back(a * b);
                    pc += 1;
                } break;
                case BytecodeOp::DIVIDE: {
                    if(stack.size() < 2) {
                        fprintf(stderr, "Error: DIVIDE instruction requires at least 2 values on the stack\n");
                        return false;
                    }
                    uint64_t b = stack.back(); stack.pop_back();
                    if(b == 0) {
                        fprintf(stderr, "Error: Division by zero\n");
                        return false;
                    }
                    uint64_t a = stack.back(); stack.pop_back();
                    stack.push_back(a / b);
                    pc += 1;
                } break;
                case BytecodeOp::PUSH_CONST: {
                    if(pc + 1 >= module.bytecode.size()) {
                        fprintf(stderr, "Error: PUSH_CONST instruction missing operand\n");
                        return false;
                    }
                    uint32_t const_index = module.bytecode[pc + 1];
                    if(const_index >= module.constants.size()) {
                        fprintf(stderr, "Error: PUSH_CONST instruction has invalid constant index %u\n", const_index);
                        return false;
                    }
                    uint64_t value = module.constants[const_index];
                    stack.push_back(value);
                    pc += 2; // advance past PUSH_CONST and its operand
                } break;
                case BytecodeOp::POP: {
                    if(stack.size() < 1) {
                        fprintf(stderr, "Error: POP instruction requires at least 1 value on the stack\n");
                        return false;
                    }
                    stack.pop_back();
                    pc += 1;
                } break;
                case BytecodeOp::RESERVE: {
                    if(pc + 1 >= module.bytecode.size()) {
                        fprintf(stderr, "Error: RESERVE instruction missing operand\n");
                        return false;
                    }
                    uint32_t how_much = module.bytecode[pc + 1];
                    for(uint32_t i = 0; i < how_much / sizeof(uint64_t); i++) {
                        stack.push_back(0); // initialize reserved space with zeros
                    }
                    pc += 2; // advance past RESERVE and its operand
                } break;
                case BytecodeOp::STORE: {
                    if(pc + 1 >= module.bytecode.size()) {
                        fprintf(stderr, "Error: STORE instruction missing operand\n");
                        return false;
                    }
                    uint32_t where = module.bytecode[pc + 1];
                    if(stack.size() < 1) {
                        fprintf(stderr, "Error: STORE instruction requires at least 1 value on the stack\n");
                        return false;
                    }
                    if(where / sizeof(uint64_t) >= stack.size()) {
                        fprintf(stderr, "Error: STORE instruction has invalid stack offset %u\n", where);
                        return false;
                    }
                    uint64_t value = stack.back();
                    stack.pop_back();
                    stack[where / sizeof(uint64_t)] = value;
                    pc += 2; // advance past STORE and its operand
                } break;
                case BytecodeOp::LOAD: {
                    if(pc + 1 >= module.bytecode.size()) {
                        fprintf(stderr, "Error: LOAD instruction missing operand\n");
                        return false;
                    }
                    uint32_t where = module.bytecode[pc + 1];
                    if(where / sizeof(uint64_t) >= stack.size()) {
                        fprintf(stderr, "Error: LOAD instruction has invalid stack offset %u\n", where);
                        return false;
                    }
                    uint64_t value = stack[where / sizeof(uint64_t)];
                    stack.push_back(value);
                    pc += 2; // advance past LOAD and its operand
                } break;
                default: {
                    fprintf(stderr, "Error: Unknown bytecode operation %u at pc %zu\n", (uint32_t)op, pc);
                    return false;
                } break;
            }
        }
        return true;
    }

    std::vector<CompiledModule> modules;
    std::vector<uint64_t> stack;
};

int run_file(char const* const script) {
    DBG_PRINT("Running script: %s\n", script);
    
    const char* const source = read_file_to_allocated_buffer(script);
    if(!source) {
        fprintf(stderr, "Error: Could not read script file %s\n", script);
        return 1;
    }
    Defer free_source([source]() { free((void*)source); });

    CompiledModule module = compile(source);
    if(!module.is_valid()) {
        fprintf(stderr, "Error: Compilation failed for script %s\n", script);
        return 1;
    }

    VM vm;
    if(!vm.load_module(module)) {
        fprintf(stderr, "Error: Could not load module for script %s\n", script);
        return 1;
    }
    if(!vm.run()) {
        fprintf(stderr, "Error: Runtime error while executing script %s\n", script);
        return 1;
    }

    return 0;
}


int main(int argc, char** argv) {
        
    if(argc < 3) {
        print_usage();
        return 1;
    }
    
    char const* const command = argv[1];
    if(strcmp(command, "run") == 0) {
        char const* const script = argv[2];
        return run_file(script);
    } else {
        print_usage();
        return 1;
    }
    
    
    return 0;
}
