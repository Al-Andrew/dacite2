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
#include "vm.hpp"

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

    dacite::VM vm;
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
