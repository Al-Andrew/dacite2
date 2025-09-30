#include "lexer.hpp"

#include <tuple>

namespace dacite {

    auto Lexer::with_source(std::string_view source) -> Lexer {
        Lexer lexer;
        lexer.source = source;
        lexer.current_pos = 0;
        return lexer;
    }

    auto Lexer::advance() -> std::tuple<char, size_t, size_t, size_t> {
        if(this->current_pos >= this->source.size()) {
            return {'\0', this->current_pos, this->current_line, this->current_column};
        }
        char c = this->source[this->current_pos];
        size_t pos = this->current_pos;
        this->current_pos++;
        
        if(c == '\n') {
            this->current_line++;
            this->current_column = 1;
        } else {
            this->current_column++;
        }
        
        return {c, pos, this->current_line, this->current_column - 1};
    }

    auto Lexer::peek() -> char {
        if(this->current_pos >= this->source.size()) {
            return '\0';
        }
        return this->source[this->current_pos];
    }

    auto Lexer::make_token(Token::Type type, size_t start_pos, size_t start_line, size_t start_column) -> Token {
        size_t length = this->current_pos - start_pos;
        return Token{type, this->source.substr(start_pos, length), start_line, start_column};
    }

    auto Lexer::tokenize_next() -> Token {

        constexpr auto is_whitespace = [](char c) constexpr { return c == ' ' || c == '\n' || c == '\r' || c == '\t'; };

        while(is_whitespace(this->peek())) {
            this->advance();
        }

        auto [c, start_pos, start_line, start_column] = this->advance();

        if(c == '\0') {
            return this->make_token(Token::Type::_EOF, start_pos, start_line, start_column);
        }

        // Single line comments
        if(c == '#') {
            while(this->peek() != '\0' && this->peek() != '\n') {
                this->advance();
            }
            return this->tokenize_next();
        }
        // Multiline comments
        if(c == '`') {
            while(this->peek() != '\0' && this->peek() != '`') {
                this->advance();
            }
            if(this->peek() == '`') {
                this->advance(); // consume closing `
            } else {
                // TODO: error unterminated multiline comment
                return this->make_token(Token::Type::Unknown, start_pos, start_line, start_column);
            }
            return this->tokenize_next();
        }

        // Single character tokens
        if(auto type = single_char_tokens_map[(uint8_t)c]; type != Token::Type::Unknown) {
            return this->make_token(type, start_pos, start_line, start_column);
        }
        // Two character tokens
        // none for now

        // Literals
        constexpr static auto is_digit = [](char c) constexpr { return c >= '0' && c <= '9'; };

        if(is_digit(c)) {
            while(is_digit(this->peek())) {
                this->advance();
            }
            size_t length = this->current_pos - start_pos;
            return this->make_token(Token::Type::Literal_Number, start_pos, start_line, start_column);
        }

        // Identifiers and keywords
        constexpr static auto is_ident_starter = [](char c) constexpr { return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'; };
        constexpr static auto is_ident_char = [](char c) constexpr { return is_ident_starter(c) || is_digit(c); };

        constexpr static auto disambiguate_identifier_or_keyword = [](std::string_view lexeme) -> Token::Type {
            for(size_t i = 0; i < keyword_strings_list.size(); i++) {
                if(lexeme == keyword_strings_list[i]) {
                    return keyword_types_list[i];
                }
            }
            return Token::Type::Identifier;
        };

        if(is_ident_starter(c)) {
            while(is_ident_char(this->peek())) {
                this->advance();
            }

            std::string_view lexeme = this->source.substr(start_pos, this->current_pos - start_pos);
            Token::Type type = disambiguate_identifier_or_keyword(lexeme);

            return this->make_token(type, start_pos, start_line, start_column);
        }

        // Intrinsics
        if(c == '@') {
            while(is_ident_char(this->peek())) {
                this->advance();
            }
            std::string_view lexeme = this->source.substr(start_pos, this->current_pos - start_pos);
            for(size_t i = 0; i < intrinsic_strings_list.size(); i++) {
                if(lexeme == intrinsic_strings_list[i]) {
                    return this->make_token(intrinsic_types_list[i], start_pos, start_line, start_column);
                }
            }
        }

        // We don't know what this is...
        return this->make_token(Token::Type::Unknown, start_pos, start_line, start_column);
    }

    auto Lexer::tokenize_all() -> std::vector<Token> {
        std::vector<Token> tokens;

        while(true) {
            Token token = tokenize_next();
            tokens.push_back(token);
            if(token.type == Token::Type::_EOF) {
                break;
            }
        }

        return tokens;
    }

}