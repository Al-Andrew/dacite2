#include "codegen.hpp"
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
    
    // Start code generation from root
    visit_node(ast->root_index);
    
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
    
    // Reserve space on stack for variable
    uint32_t var_offset = variable_stack_offset_table.size();
    variable_stack_offset_table[var_name] = var_offset * sizeof(uint64_t);

    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::RESERVE));
    module.bytecode.push_back(sizeof(uint64_t)); // reserve 8 bytes for u64

    // Generate code for expression
    visit_node(node.initializer);
    
    // Store top of stack into variable location
    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::STORE));
    module.bytecode.push_back(var_offset * sizeof(uint64_t));
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
    
    uint32_t var_offset = it->second;
    module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::LOAD));
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
                uint32_t var_offset = it->second;
                module.bytecode.push_back(static_cast<uint32_t>(BytecodeOp::STORE));
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

}