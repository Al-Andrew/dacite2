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
#include "parser.hpp"

using dacite::Token;
using dacite::AST;

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

    AST ast = dacite::Parser::with_tokens(tokens).parse();
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
