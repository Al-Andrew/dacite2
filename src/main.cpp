#include <cmath>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iostream>
#include <memory>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <vector>

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

struct Token {
    enum class Type : uint8_t {
        Intrinsic_Print,
        Literal_Number,
        
        Lparen,
        Rparen,
        
        Plus,
        Minus,
        Star,
        Slash,

        Semicolon,

        _EOF,
    };
    Type type;
    uint16_t length;
    uint32_t pos;
};

std::vector<Token> tokenize(const char* source) {
    std::vector<Token> tokens;

    uint32_t start_pos = 0;
    uint32_t current_pos = 0;

    while(true) {
        char c = source[current_pos];
        if(c == '\0') {
            tokens.push_back({Token::Type::_EOF, 0, current_pos});
            break;
        }
        else if(c == ' ' || c == '\n' || c == '\r' || c == '\t') {
            // Skip whitespace
            current_pos++;
            start_pos = current_pos;
        }
        else if(c == '#') {
            // Comment, skip to end of line
            while(source[current_pos] != '\0' && source[current_pos] != '\n') {
                current_pos++;
            }
            start_pos = current_pos;
        } else if(c == '`') {
            // multiline comment, skip to closing `
            current_pos++;
            while(source[current_pos] != '\0' && source[current_pos] != '`') {
                current_pos++;
            }
            start_pos = current_pos;
        }
        else if(c == '(') {
            tokens.push_back({Token::Type::Lparen, 1, current_pos});
            current_pos++;
            start_pos = current_pos;
        }
        else if(c == ')') {
            tokens.push_back({Token::Type::Rparen, 1, current_pos});
            current_pos++;
            start_pos = current_pos;
        }
        else if(c == ';') {
            tokens.push_back({Token::Type::Semicolon, 1, current_pos});
            current_pos++;
            start_pos = current_pos;
        }
        else if(*(source + current_pos) == '@') {
            // Intrinsic
            if(strncmp(source + current_pos, "@print", 6) == 0) {
                tokens.push_back({Token::Type::Intrinsic_Print, 6, current_pos});
                current_pos += 6;
                start_pos = current_pos;
            } else {
                fprintf(stderr, "Error: Unknown intrinsic at pos %d\n", current_pos);
                break;
            }
        }
        else if(c == '+') {
            tokens.push_back({Token::Type::Plus, 1, current_pos});
            current_pos++;
            start_pos = current_pos;
        }
        else if(c == '-') {
            tokens.push_back({Token::Type::Minus, 1, current_pos});
            current_pos++;
            start_pos = current_pos;
        }
        else if(c == '*') {
            tokens.push_back({Token::Type::Star, 1, current_pos});
            current_pos++;
            start_pos = current_pos;
        }
        else if(c == '/') {
            tokens.push_back({Token::Type::Slash, 1, current_pos});
            current_pos++;
            start_pos = current_pos;
        }
        else if(c >= '0' && c <= '9') {
            // Number literal
            while(source[current_pos] >= '0' && source[current_pos] <= '9') {
                current_pos++;
            }
            uint16_t length = current_pos - start_pos;
            tokens.push_back({Token::Type::Literal_Number, length, start_pos});
            start_pos = current_pos;
        }

        else {
            fprintf(stderr, "Error: Unknown character '%c' at pos %d\n", c, current_pos);
            break;
        }
    }

    return tokens;
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
            std::string lexeme(source_ref + token.pos, token.length);
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
            std::string lexeme(source_ref + operator_token.pos, operator_token.length);
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
            std::string lexeme(source_ref + operator_token.pos, operator_token.length);
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
            std::string lexeme(source_ref + operator_token.pos, operator_token.length);
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

void parse_intrinsic_print(AST& ast, const std::vector<Token>& tokens, char const* const source, size_t& index) {
    DBG_PRINT("parse_intrinsic_print: index=%zu", index);
    
    // Expecting: @print ( <number> ) ;
    if(index >= tokens.size() || tokens[index].type != Token::Type::Intrinsic_Print) {
        fprintf(stderr, "Error: Expected @print at token %zu\n", index);
        return;
    }
    index++; // consume @print

    if(index >= tokens.size() || tokens[index].type != Token::Type::Lparen) {
        fprintf(stderr, "Error: Expected ( after @print at token %zu\n", index);
        return;
    }
    index++; // consume (

    std::unique_ptr<AST::Node> expression = parse_expression(ast, tokens, source, index);
    if(!expression) {
        fprintf(stderr, "Error: Failed to parse expression after @print at token %zu\n", index);
        return;
    }

    if(index >= tokens.size() || tokens[index].type != Token::Type::Rparen) {
        fprintf(stderr, "Error: Expected ) after number literal at token %zu\n", index);
        return;
    }
    index++; // consume )

    if(index >= tokens.size() || tokens[index].type != Token::Type::Semicolon) {
        fprintf(stderr, "Error: Expected ; after ) at token %zu\n", index);
        return;
    }
    index++; // consume ;

    // Construct AST node for @print statement
    std::unique_ptr<AST::IntrinsicPrint> print_node = std::make_unique<AST::IntrinsicPrint>();
    print_node->children.push_back(std::move(expression));

    ast.root->children.emplace_back(std::move(print_node));
    // Successfully parsed @print statement
    DBG_PRINT("Successfully parsed @print statement");
}

void parse_statement(AST& ast, const std::vector<Token>& tokens, char const* const source, size_t& index) {
    if(index >= tokens.size()) {
        return;
    }

    const Token& token = tokens[index];
    
    switch(token.type) {
        case Token::Type::Intrinsic_Print: {
            parse_intrinsic_print(ast, tokens, source, index);
        } break;
        default: {
            fprintf(stderr, "Error: Unexpected token type %d at token %zu\n", (int)token.type, index);
            return;
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
        parse_statement(ast, tokens, source, index);
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
};

CompiledModule codegen_from_ast(const AST& ast) {
    // Dummy codegen function
    DBG_PRINT("Generating code from AST");

    CompiledModule module;

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
        else if(const AST::IntrinsicPrint* print_node = dynamic_cast<const AST::IntrinsicPrint*>(node); print_node) {
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
        else if(const AST::NumberLiteral* number_node = dynamic_cast<const AST::NumberLiteral*>(node); number_node) {
            // Convert number token to integer
            char const* lexeme = ast.source + number_node->token.pos;
            uint32_t length = number_node->token.length;

            uint64_t value = strtoull(std::string(lexeme, length).c_str(), nullptr, 10);
            // Add constant to module
            uint32_t const_index = module.constants.size();
            module.constants.push_back(value);
            // Generate bytecode to push constant
            module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::PUSH_CONST));
            module.bytecode.push_back(const_index);
            return;
        }
        else if(const AST::BinaryExpression* binary_node = dynamic_cast<const AST::BinaryExpression*>(node); binary_node) {
            if(binary_node->children.size() != 2) {
                fprintf(stderr, "Error: BinaryExpression node has %zu children, expected 2\n", binary_node->children.size());
                return;
            }
            const AST::Node* lhs = binary_node->children[0].get();
            const AST::Node* rhs = binary_node->children[1].get();
            dfs(lhs);
            dfs(rhs);

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
                default:
                    fprintf(stderr, "Error: Unknown binary operator token type %d\n", (int)binary_node->operator_token.type);
                    return;
            }
            return;
        }
        else if(const AST::UnaryPrefixExpression* prefix_node = dynamic_cast<const AST::UnaryPrefixExpression*>(node); prefix_node) {
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
    
    std::vector<Token> tokens = tokenize(source);

    if constexpr(ENABLE_TOKEN_DEBUG) {
        for(const Token& token : tokens) {
            DBG_PRINT("Token: type=%d, length=%d, pos=%d, lexeme=%.*s", (int)token.type, token.length, token.pos, token.length, source + token.pos);
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
