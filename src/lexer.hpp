#pragma once

#include "token.hpp"
#include <vector>
#include <string_view>
#include <tuple>

namespace dacite {

    // TODO(AAL): lexing is embarrassingly parallel
    class Lexer {
    public:
        auto static with_source(std::string_view source) -> Lexer;
        auto tokenize_next() -> Token;
        auto tokenize_all() -> std::vector<Token>;

        ~Lexer() = default;
    private:
        auto advance() -> std::tuple<char, size_t, size_t, size_t>;
        auto peek() -> char;
        auto peek_next() -> char;

        auto make_token(Token::Type type, size_t start_pos, size_t start_line, size_t start_column) -> Token;

        Lexer() = default;

        std::string_view source;
        size_t current_pos = 0;
        size_t current_line = 1;
        size_t current_column = 1;
    };

}