#pragma once

#include <cstdint>
#include <cstddef>
#include <string_view>
#include <array>


namespace dacite {

    
    struct Token {
        enum Type : uint8_t {
            Unknown,
    
            Keyword_Let,
    
            Intrinsic_Print,
            
            
            Literal_Number,
    
            Identifier,
            
            Lparen,
            Rparen,
            
            Plus,
            Minus,
            Star,
            Slash,
    
            Equals,
    
            Colon,
            Semicolon,
    
            _EOF,
        };

        Type type;
        std::string_view lexeme;
        size_t line;
        size_t column;
    };

    static constexpr std::array<std::string_view, Token::Type::_EOF + 1> token_type_to_string_map = []() constexpr {
        std::array<std::string_view, Token::Type::_EOF + 1> arr{};
        arr[Token::Type::Unknown] = "Unknown";
        arr[Token::Type::Keyword_Let] = "Keyword_Let";
        arr[Token::Type::Intrinsic_Print] = "Intrinsic_Print";
        arr[Token::Type::Literal_Number] = "Literal_Number";
        arr[Token::Type::Identifier] = "Identifier";
        arr[Token::Type::Lparen] = "Lparen";
        arr[Token::Type::Rparen] = "Rparen";
        arr[Token::Type::Plus] = "Plus";
        arr[Token::Type::Minus] = "Minus";
        arr[Token::Type::Star] = "Star";
        arr[Token::Type::Slash] = "Slash";
        arr[Token::Type::Equals] = "Equals";
        arr[Token::Type::Colon] = "Colon";
        arr[Token::Type::Semicolon] = "Semicolon";
        arr[Token::Type::_EOF] = "_EOF";
        return arr;
    }();

    static constexpr std::array<Token::Type, 256> single_char_tokens_map = []() constexpr {
        std::array<Token::Type, 256> map{};
        map['('] = Token::Type::Lparen;
        map[')'] = Token::Type::Rparen;
        map['+'] = Token::Type::Plus;
        map['-'] = Token::Type::Minus;
        map['*'] = Token::Type::Star;
        map['/'] = Token::Type::Slash;
        map['='] = Token::Type::Equals;
        map[':'] = Token::Type::Colon;
        map[';'] = Token::Type::Semicolon;
        return map;
    }();

    static constexpr std::array<Token::Type, 1> keyword_types_list = {
        Token::Type::Keyword_Let,
    };
    static constexpr std::array<std::string_view, 1> keyword_strings_list = {
        "let",
    };

    static constexpr std::array<Token::Type, 1> intrinsic_types_list = {
        Token::Type::Intrinsic_Print,
    };
    static constexpr std::array<std::string_view, 1> intrinsic_strings_list = {
        "@print",
    };
}