#pragma once

#include "token.hpp"
#include <vector>
#include <memory>
#include <string>

namespace dacite {

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

    struct VariableDeclaration : public Statement {
        void print(int indent = 0) const override {
            printf("%*sVariableDeclaration\n", indent * 2, "");
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        const char* get_type_name() const override { return "VariableDeclaration"; }
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
            std::string lexeme{token.lexeme};
            printf("%*sNumberLiteral: %s\n", indent * 2, "", lexeme.c_str());
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        
        const char* get_type_name() const override { return "NumberLiteral"; }
        
        Token token;
    };

    struct Identifier : public Node {
        Identifier(const Token& t) : token(t) {}
        
        void print(int indent = 0) const override {
            // Extract the actual identifier name from the token
            std::string lexeme{token.lexeme};
            printf("%*sIdentifier: %s\n", indent * 2, "", lexeme.c_str());
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        
        const char* get_type_name() const override { return "Identifier"; }
        
        Token token;
    };

    struct Type : public Identifier {
        Type(const Token& t) : Identifier(t) {}

        void print(int indent = 0) const override {
            printf("%*sType\n", indent * 2, "");
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        const char* get_type_name() const override { return "Type"; }
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
            std::string lexeme{operator_token.lexeme};
            printf("%*sBinaryExpression: %s\n", indent * 2, "", lexeme.c_str());
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        
        const char* get_type_name() const override { return "BinaryExpression"; }
        
        Token operator_token;
    };

    struct UnaryPrefixExpression : public Expression {
        UnaryPrefixExpression(const Token& op) : operator_token(op) {}
        
        void print(int indent = 0) const override {
            std::string lexeme{operator_token.lexeme};
            printf("%*sUnaryPrefixExpression: %s\n", indent * 2, "", lexeme.c_str());
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        
        const char* get_type_name() const override { return "UnaryPrefixExpression"; }
        
        Token operator_token;
    };

     struct UnaryPostfixExpression : public Expression {
        UnaryPostfixExpression(const Token& op) : operator_token(op) {}
        
        void print(int indent = 0) const override {
            std::string lexeme{operator_token.lexeme};
            printf("%*sUnaryPostfixExpression: %s\n", indent * 2, "", lexeme.c_str());
            for (const auto& child : children) {
                if (child) {
                    child->print(indent + 1);
                }
            }
        }
        
        const char* get_type_name() const override { return "UnaryPostfixExpression"; }
        
        Token operator_token;
    };

    std::unique_ptr<Node> root;
    
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
    auto parse_expression_bp(int min_bp) -> std::unique_ptr<AST::Node>;
    auto parse_expression() -> std::unique_ptr<AST::Node>;
    auto parse_intrinsic_print() -> std::unique_ptr<AST::Node>;
    auto parse_variable_declaration() -> std::unique_ptr<AST::Node>;
    auto parse_statement() -> std::unique_ptr<AST::Node>;
    
    // Helper functions for common parsing patterns
    auto expect_token(Token::Type expected_type, const char* context) -> bool;
    auto consume_token(Token::Type expected_type, const char* context) -> bool;
    auto is_at_end() -> bool;
    auto current_token() -> const Token&;
    auto advance() -> void;
};

}