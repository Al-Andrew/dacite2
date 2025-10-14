#pragma once

#include "parser.hpp"
#include <vector>
#include <map>
#include <string>
#include <cstdint>

namespace dacite {

// Register IDs
enum class RegisterId : uint32_t {
    RSP = 0,  // Stack pointer
    RBP = 1,  // Base pointer
};

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
    JMP = 11, // <where>

    CALL = 12, // <function_offset>
    RETURN = 13,

    // Minimal register management opcodes
    PUSH_REG = 15,    // <reg_id> - Push register onto stack
    POP_REG = 16,     // <reg_id> - Pop from stack into register
    LOAD_REG = 20,    // <reg_id> <offset> - Load from [register + offset]
    STORE_REG = 21,   // <reg_id> <offset> - Store to [register + offset]

    HALT = 22,
};

// Compiled module structure
struct CompiledModule {
    bool is_valid() const;
    
    std::vector<uint32_t> bytecode;
    std::vector<uint64_t> constants;
    std::map<std::string, uint64_t> function_offsets;
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
    auto visit_node(const FunctionDeclaration& node) -> void;
    auto visit_node(const Block& node) -> void;
    auto visit_node(const ReturnStatement& node) -> void;
    auto visit_node(const IntrinsicHalt& node) -> void;
    auto visit_node(const FunctionCall& node) -> void;
    auto visit_node(const FunctionParameterDeclaration& node) -> void { /* no-op - used only during parsing */ }
    
    // Helper method to visit any node by index
    auto visit_node(NodeIndex node_index) -> void;
    
    // Helper methods for common operations
    auto emit_set_reg(RegisterId dst_reg, RegisterId src_reg) -> void;
    auto emit_add_reg(RegisterId reg, uint32_t amount) -> void;
    auto emit_sub_reg(RegisterId reg, uint32_t amount) -> void;
    
    auto patch_defered_functions() -> void;

    const AST* ast = nullptr;
    CompiledModule module;
    std::map<std::string, std::vector<uint32_t>> deffered_function_offsets;
    std::map<std::string, int32_t> variable_stack_offset_table;
};

}