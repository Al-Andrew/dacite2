#pragma once

#include "token.hpp"
#include <vector>
#include <variant>
#include <string>
#include <cstdint>
#include <cstdio>

namespace dacite {

// Forward declarations for all node types
struct Module;
struct VariableDeclaration;
struct IntrinsicPrint;
struct IntrinsicHalt;
struct NumberLiteral;
struct Identifier;
struct Type;
struct BinaryExpression;
struct UnaryPrefixExpression;
struct FunctionDeclaration;
struct Block;
struct ReturnStatement;
struct FunctionCall;

// Node variant type containing all possible node types
using ASTNodeVariant = std::variant<
    Module,
    VariableDeclaration,
    IntrinsicPrint,
    IntrinsicHalt,
    NumberLiteral,
    Identifier,
    Type,
    BinaryExpression,
    UnaryPrefixExpression,
    FunctionDeclaration,
    Block,
    ReturnStatement,
    FunctionCall
>;

// Index type for referencing nodes
using NodeIndex = uint32_t;
constexpr NodeIndex INVALID_NODE_INDEX = UINT32_MAX;

// Node structure definitions
struct Module {
    std::vector<NodeIndex> statements;
};

struct VariableDeclaration {
    NodeIndex identifier;
    NodeIndex type;
    NodeIndex initializer;
};

struct IntrinsicPrint {
    NodeIndex expression;
};

struct NumberLiteral {
    Token token;
    
    NumberLiteral(const Token& t) : token(t) {}
};

struct Identifier {
    Token token;
    
    Identifier(const Token& t) : token(t) {}
};

struct Type {
    Token token;
    
    Type(const Token& t) : token(t) {}
};

struct BinaryExpression {
    Token operator_token;
    NodeIndex left;
    NodeIndex right;
    
    BinaryExpression(const Token& op) : operator_token(op), left(INVALID_NODE_INDEX), right(INVALID_NODE_INDEX) {}
};

struct UnaryPrefixExpression {
    Token operator_token;
    NodeIndex operand;
    
    UnaryPrefixExpression(const Token& op) : operator_token(op), operand(INVALID_NODE_INDEX) {}
};

struct FunctionDeclaration {
    Token name;
    NodeIndex return_type;
    NodeIndex body;

    FunctionDeclaration(const Token& n, NodeIndex ret_type, NodeIndex b)
        : name(n), return_type(ret_type), body(b) {}
};

struct Block {
    std::vector<NodeIndex> statements;
};

struct ReturnStatement {
    NodeIndex expression;
    
    ReturnStatement(NodeIndex expr) : expression(expr) {}
};

struct IntrinsicHalt{
    IntrinsicHalt() = default;
};

struct FunctionCall {
    NodeIndex callee;
    std::vector<NodeIndex> arguments;
    
    FunctionCall(NodeIndex c) : callee(c) {}
};

struct AST {
    // Default constructor
    AST() = default;
    
    // Disable copy constructor and copy assignment
    AST(const AST&) = delete;
    AST& operator=(const AST&) = delete;
    
    // Enable move constructor and move assignment
    AST(AST&&) = default;
    AST& operator=(AST&&) = default;
    
    bool is_valid() const {
        return root_index != INVALID_NODE_INDEX;
    }

    // Storage for all nodes
    std::vector<ASTNodeVariant> nodes;
    NodeIndex root_index = INVALID_NODE_INDEX;
    
    // Utility functions for working with nodes
    template<typename T>
    NodeIndex add_node(T&& node) {
        NodeIndex index = static_cast<NodeIndex>(nodes.size());
        nodes.emplace_back(std::forward<T>(node));
        return index;
    }
    
    template<typename T>
    T* get_node(NodeIndex index) {
        if (index >= nodes.size()) return nullptr;
        return std::get_if<T>(&nodes[index]);
    }
    
    template<typename T>
    const T* get_node(NodeIndex index) const {
        if (index >= nodes.size()) return nullptr;
        return std::get_if<T>(&nodes[index]);
    }
    
    // Print function for AST nodes
    void print_node(NodeIndex index, int indent = 0) const;
    
    // Utility function to print the entire AST
    void print_ast() const {
        printf("=== AST Structure ===\n");
        if (is_valid()) {
            print_node(root_index, 0);
        } else {
            printf("(empty AST)\n");
        }
        printf("===================\n");
    }
};

class Parser {
public:
    static auto with_tokens(const std::vector<Token>& tokens) -> Parser;
    auto parse() -> AST;

private:
    // Private constructor for named constructor pattern
    Parser(const std::vector<Token>& tokens);
    
    // Data members
    const std::vector<Token>& tokens;
    size_t index;
    AST ast;
    
    // Precedence binding functions
    auto get_infix_binding(Token::Type type) -> std::pair<int, int>;
    auto get_prefix_binding(Token::Type type) -> std::pair<int, int>;
    auto get_postfix_binding(Token::Type type) -> std::pair<int, int>;

    // Core parsing functions
    auto parse_expression_bp(int min_bp) -> NodeIndex;
    auto parse_expression() -> NodeIndex;
    auto parse_intrinsic_print() -> NodeIndex;
    auto parse_variable_declaration() -> NodeIndex;
    auto parse_statement() -> NodeIndex;
    auto parse_block() -> NodeIndex;
    auto parse_function_declaration() -> NodeIndex;
    auto parse_return_statement() -> NodeIndex;
    auto parse_halt_statement() -> NodeIndex;
    auto parse_function_call(NodeIndex callee) -> NodeIndex;

    // Helper functions for common parsing patterns
    auto expect_token(Token::Type expected_type, const char* context) -> bool;
    auto consume_token(Token::Type expected_type, const char* context) -> bool;
    auto is_at_end() -> bool;
    auto current_token() -> const Token&;
    auto advance() -> void;
};

}