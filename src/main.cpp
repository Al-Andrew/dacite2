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
#include "codegen.hpp"

using dacite::Token;
using dacite::AST;
using dacite::CompiledModule;

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

    CompiledModule module = dacite::CodeGenerator::with_ast(ast).generate();
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
            dacite::BytecodeOp op = static_cast<dacite::BytecodeOp>(module.bytecode[pc]);
            switch(op) {
                case dacite::BytecodeOp::PRINT: {
                    if(stack.size() < 1) {
                        fprintf(stderr, "Error: PRINT instruction requires at least 1 value on the stack\n");
                        return false;
                    }
                    uint64_t value = stack.back();
                    stack.pop_back();
                    printf("%llu\n", value);
                    pc += 1;
                } break;
                case dacite::BytecodeOp::ADD: {
                    if(stack.size() < 2) {
                        fprintf(stderr, "Error: ADD instruction requires at least 2 values on the stack\n");
                        return false;
                    }
                    uint64_t b = stack.back(); stack.pop_back();
                    uint64_t a = stack.back(); stack.pop_back();
                    stack.push_back(a + b);
                    pc += 1;
                } break;
                case dacite::BytecodeOp::SUBTRACT: {
                    if(stack.size() < 2) {
                        fprintf(stderr, "Error: SUBTRACT instruction requires at least 2 values on the stack\n");
                        return false;
                    }
                    uint64_t b = stack.back(); stack.pop_back();
                    uint64_t a = stack.back(); stack.pop_back();
                    stack.push_back(a - b);
                    pc += 1;
                } break;
                case dacite::BytecodeOp::MULTIPLY: {
                    if(stack.size() < 2) {
                        fprintf(stderr, "Error: MULTIPLY instruction requires at least 2 values on the stack\n");
                        return false;
                    }
                    uint64_t b = stack.back(); stack.pop_back();
                    uint64_t a = stack.back(); stack.pop_back();
                    stack.push_back(a * b);
                    pc += 1;
                } break;
                case dacite::BytecodeOp::DIVIDE: {
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
                case dacite::BytecodeOp::PUSH_CONST: {
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
                case dacite::BytecodeOp::POP: {
                    if(stack.size() < 1) {
                        fprintf(stderr, "Error: POP instruction requires at least 1 value on the stack\n");
                        return false;
                    }
                    stack.pop_back();
                    pc += 1;
                } break;
                case dacite::BytecodeOp::RESERVE: {
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
                case dacite::BytecodeOp::STORE: {
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
                case dacite::BytecodeOp::LOAD: {
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
