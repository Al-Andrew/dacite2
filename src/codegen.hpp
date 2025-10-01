#pragma once

#include "parser.hpp"
#include <vector>
#include <map>
#include <string>
#include <cstdint>
#include <functional>

namespace dacite {

// Bytecode operations
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

// Compiled module structure
struct CompiledModule {
    bool is_valid() const;
    
    std::vector<uint32_t> bytecode;
    std::vector<uint64_t> constants;
};

// Code generator class
class CodeGenerator {
public:
    static auto with_ast(const AST& ast) -> CodeGenerator;
    auto generate() -> CompiledModule;
    
    ~CodeGenerator() = default;

private:
    CodeGenerator() = default;
    
    // Overloaded visit_node functions for different node types
    auto visit_node(const Module& node) -> void;
    auto visit_node(const VariableDeclaration& node) -> void;
    auto visit_node(const Identifier& node) -> void;
    auto visit_node(const IntrinsicPrint& node) -> void;
    auto visit_node(const NumberLiteral& node) -> void;
    auto visit_node(const BinaryExpression& node) -> void;
    auto visit_node(const UnaryPrefixExpression& node) -> void;
    auto visit_node(const Type& node) -> void { /* no-op - used only during parsing */ }
    
    // Helper method to visit any node by index
    auto visit_node(NodeIndex node_index) -> void;
    
    const AST* ast = nullptr;
    CompiledModule module;
    std::map<std::string, uint32_t> variable_stack_offset_table;
};

}