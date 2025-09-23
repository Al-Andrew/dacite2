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

void parse_intrinsic_print(AST& ast, const std::vector<Token>& tokens, char const* const source, size_t& index) {
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

    if(index >= tokens.size() || tokens[index].type != Token::Type::Literal_Number) {
        fprintf(stderr, "Error: Expected number literal after ( at token %zu\n", index);
        return;
    }
    const Token& number_token = tokens[index];
    std::unique_ptr<AST::NumberLiteral> number_literal = std::make_unique<AST::NumberLiteral>(number_token);
    number_literal->set_source_ref(source);
    index++; // consume number literal

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
    print_node->children.push_back(std::move(number_literal));

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
            module.bytecode.push_back(module.constants.size() - 1); // index of the constant to print
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
                    if(pc + 1 >= module.bytecode.size()) {
                        fprintf(stderr, "Error: PRINT instruction missing operand\n");
                        return false;
                    }
                    uint32_t const_index = module.bytecode[pc + 1];
                    if(const_index >= module.constants.size()) {
                        fprintf(stderr, "Error: PRINT instruction has invalid constant index %u\n", const_index);
                        return false;
                    }
                    uint64_t value = module.constants[const_index];
                    printf("%llu\n", (unsigned long long)value);
                    pc += 2; // advance past PRINT and its operand
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
