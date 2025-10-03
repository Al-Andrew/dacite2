#include "codegen.hpp"
#include "parser.hpp"
#include <cstdint>
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <variant>

#define DBG_PRINT(X, ...) printf(X "\n", ##__VA_ARGS__)

namespace dacite {

bool CompiledModule::is_valid() const {
    return bytecode.size() > 0;
}

auto CodeGenerator::with_ast(const AST& ast) -> CodeGenerator {
    CodeGenerator generator;
    generator.ast = &ast;
    return generator;
}

auto CodeGenerator::generate() -> CompiledModule {
    DBG_PRINT("Generating code from AST");
    
    if (!ast || !ast->is_valid()) {
        return CompiledModule{};
    }
    
    // Clear previous state
    module = CompiledModule{};
    variable_stack_offset_table.clear();

    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::CALL)); // We JMP to main not CALL but return out of it, hence the setup before

    module.bytecode.push_back(0); // Placeholder for main function offset
    auto main_function_offset_index = module.bytecode.size() - 1;
    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::HALT));

    // Start code generation from root
    visit_node(ast->root_index);

    // Now we patch the main function offset
    auto it = module.function_offsets.find("main");
    if (it == module.function_offsets.end()) {
        fprintf(stderr, "Error: No 'main' function defined\n");
        return CompiledModule{};
    }
    uint32_t main_offset = it->second;
    module.bytecode[main_function_offset_index] = main_offset;

    patch_defered_functions();

    DBG_PRINT("Generated module with %zu constants", module.constants.size());
    return module;
}

auto CodeGenerator::visit_node(NodeIndex node_index) -> void {
    if (node_index == INVALID_NODE_INDEX || node_index >= ast->nodes.size()) {
        return;
    }
    
    std::visit([this](const auto& node) { 
        visit_node(node); 
    }, ast->nodes[node_index]);
}

// Overloaded visit_node function implementations
auto CodeGenerator::visit_node(const Module& node) -> void {
    // Traverse children
    for (NodeIndex statement_index : node.statements) {
        visit_node(statement_index);
    }
}

auto CodeGenerator::visit_node(const VariableDeclaration& node) -> void {
    // Generate bytecode for variable declaration
    const Identifier* id_node = ast->get_node<Identifier>(node.identifier);
    if (!id_node) {
        fprintf(stderr, "Error: VariableDeclaration identifier is not an Identifier node\n");
        return;
    }
    
    std::string var_name{id_node->token.lexeme};
    DBG_PRINT("Declaring variable: %s", var_name.c_str());
    
    if (variable_stack_offset_table.find(var_name) != variable_stack_offset_table.end()) {
        fprintf(stderr, "Error: Variable %s already declared\n", var_name.c_str());
        return;
    }
    
    // Allocate space for local variable (positive offset from RBP)
    int32_t next_local_offset = 0;
    for (const auto& [name, offset] : variable_stack_offset_table) {
        if (offset >= 0) { // Only consider local variables (positive offsets)
            next_local_offset = std::max(next_local_offset, offset + (int32_t)sizeof(uint64_t));
        }
    }
    
    variable_stack_offset_table[var_name] = next_local_offset;

    // Allocate stack space
    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::ADD_RSP));
    module.bytecode.push_back(sizeof(uint64_t)); // allocate 8 bytes for u64

    // Generate code for expression
    visit_node(node.initializer);
    
    // Store top of stack into variable location
    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::STORE_RBP));
    module.bytecode.push_back(next_local_offset);
}

auto CodeGenerator::visit_node(const Identifier& node) -> void {
    // Load variable value onto stack
    std::string var_name{node.token.lexeme};
    DBG_PRINT("Loading variable: %s", var_name.c_str());
    
    auto it = variable_stack_offset_table.find(var_name);
    if (it == variable_stack_offset_table.end()) {
        fprintf(stderr, "Error: Variable %s not declared\n", var_name.c_str());
        return;
    }
    
    int32_t var_offset = it->second;
    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::LOAD_RBP));
    module.bytecode.push_back(var_offset);
}

auto CodeGenerator::visit_node(const IntrinsicPrint& node) -> void {
    // Generate bytecode for @print
    visit_node(node.expression);
    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::PRINT));
}

auto CodeGenerator::visit_node(const NumberLiteral& node) -> void {
    // Convert number token to integer
    uint64_t value = strtoull(node.token.lexeme.data(), nullptr, 10);
    
    // Add constant to module
    uint32_t const_index = module.constants.size();
    module.constants.push_back(value);
    
    // Generate bytecode to push constant
    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::PUSH_CONST));
    module.bytecode.push_back(const_index);
}

auto CodeGenerator::visit_node(const BinaryExpression& node) -> void {
    if (node.operator_token.type == Token::Type::Equals) {
        // For assignment, rhs is evaluated first
        visit_node(node.right);
    }
    else {
        visit_node(node.left);
        visit_node(node.right);
    }

    switch (node.operator_token.type) {
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
            if (node.left == INVALID_NODE_INDEX) {
                fprintf(stderr, "Error: Assignment target is invalid\n");
                return;
            }
            if (const Identifier* id_node = ast->get_node<Identifier>(node.left); id_node) {
                std::string var_name{id_node->token.lexeme};
                DBG_PRINT("Assigning to variable: %s", var_name.c_str());
                auto it = variable_stack_offset_table.find(var_name);
                if (it == variable_stack_offset_table.end()) {
                    fprintf(stderr, "Error: Variable %s not declared\n", var_name.c_str());
                    return;
                }
                int32_t var_offset = it->second;
                module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::STORE_RBP));
                module.bytecode.push_back(var_offset);
            } else {
                fprintf(stderr, "Error: Assignment target is not an Identifier\n");
                return;
            }
            break;
        default:
            fprintf(stderr, "Error: Unknown binary operator token type %d\n", (int)node.operator_token.type);
            return;
    }
}

auto CodeGenerator::visit_node(const UnaryPrefixExpression& node) -> void {
    visit_node(node.operand);
    
    switch (node.operator_token.type) {
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
            fprintf(stderr, "Error: Unknown unary prefix operator token type %d\n", (int)node.operator_token.type);
            return;
    }
}

    auto CodeGenerator::visit_node(const FunctionDeclaration& node) -> void {
        DBG_PRINT("Visiting FunctionDeclaration: %.*s", (int)node.name.lexeme.size(), node.name.lexeme.data());
        
        std::string func_name{node.name.lexeme};
        if (module.function_offsets.find(func_name) != module.function_offsets.end()) {
            fprintf(stderr, "Error: Function %s already defined\n", func_name.c_str());
            return;
        }
        uint32_t func_start_offset = module.bytecode.size();
        module.function_offsets[func_name] = func_start_offset;

        // x86-style function prologue:
        // 1. Push old RBP (done by CALL instruction in caller)
        // 2. Set RBP to current RSP (establish new frame)
        module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::SET_RBP));

        // Save the current variable table to restore later
        auto saved_var_table = variable_stack_offset_table;
        variable_stack_offset_table.clear();

        // Process parameters - they are already on the stack below the saved RBP and return address
        // Stack layout at function entry: [...] [param_n] ... [param_1] [return_addr] [old_rbp] <- RBP=RSP
        // So parameters are at negative offsets from RBP
        int32_t param_offset = -((int32_t)node.parameters.size() + 2) * sizeof(uint64_t); // +2 for return_addr and old_rbp
        
        for(auto param_index: node.parameters) {
            if(const FunctionParameterDeclaration* param_node = ast->get_node<FunctionParameterDeclaration>(param_index); param_node) {
                const Token& param_name_token = param_node->name;
                std::string param_name{param_name_token.lexeme};
                DBG_PRINT("Function parameter: %s at offset %d", param_name.c_str(), param_offset);
                
                // Store parameter offset relative to RBP
                variable_stack_offset_table[param_name] = param_offset;
                param_offset += sizeof(uint64_t); // next parameter
            } else {
                fprintf(stderr, "Error: Function parameter is not a FunctionParameterDeclaration node\n");
                return;
            }
        }

        visit_node(node.body);
        
        // Restore the variable table
        variable_stack_offset_table = saved_var_table;
    }

    auto CodeGenerator::visit_node(const Block& node) -> void {
        DBG_PRINT("Visiting Block with %zu statements", node.statements.size());
        for (NodeIndex stmt_index : node.statements) {
            visit_node(stmt_index);
        }
    }

    auto CodeGenerator::visit_node(const ReturnStatement& node) -> void {
        DBG_PRINT("Visiting ReturnStatement");
        visit_node(node.expression);
        module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::RETURN));
    }

    auto CodeGenerator::visit_node(const IntrinsicHalt& node) -> void {
        DBG_PRINT("Visiting IntrinsicHalt");
        module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::HALT));
    }
    
    auto CodeGenerator::visit_node(const FunctionCall& node) -> void {
        DBG_PRINT("Visiting FunctionCall");
        
        for (NodeIndex arg_index : node.arguments) {
            visit_node(arg_index);
        }

        if (const Identifier* id_node = ast->get_node<Identifier>(node.callee); id_node) {
            std::string func_name{id_node->token.lexeme};
            auto it = module.function_offsets.find(func_name);
            if (it != module.function_offsets.end()) {
                uint32_t func_offset = it->second;
                
                module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::CALL));
                module.bytecode.push_back(func_offset);
            } else {
                uint32_t func_offset = 0xcafebabe;

                module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::CALL));
                module.bytecode.push_back(func_offset);

                if (auto it = deffered_function_offsets.find(func_name); it != deffered_function_offsets.end()) {
                    fprintf(stderr, "Error:  trying to redefine function: %.*s\n", (int)func_name.size(), func_name.data());
                    exit(42);
                }

                deffered_function_offsets[func_name].push_back(module.bytecode.size() - 1); 
            }
            
        } else {
            fprintf(stderr, "Error: Function call callee is not an Identifier\n");
            return;
        }

    }

    auto CodeGenerator::patch_defered_functions() -> void {

        for(auto& [fname, offsets] : deffered_function_offsets) {

            if(auto it = module.function_offsets.find(fname); it != module.function_offsets.end()) {
                uint32_t func_offset = it->second;
                for(auto offset : offsets) {
                    module.bytecode[offset] = func_offset;
                }
            } else {
                fprintf(stderr, "Couldn't find function %s\n", fname.c_str());
            }
        }
    }
}