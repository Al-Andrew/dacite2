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
            Keyword_Fun,
            Keyword_Return,
            Keyword_If,
            Keyword_Else,
    
            Intrinsic_Print,
            Intrinsic_Halt,
            
            
            Literal_Number,
    
            Identifier,
            
            Lparen,
            Rparen,
            Lbrace,
            Rbrace,
            
            Plus,
            Minus,
            Star,
            Slash,
    
            Equals,
            DoubleEquals,
            NotEquals,
            LessThan,
            GreaterThan,

            Comma,
            Colon,
            Semicolon,
    
            Arrow,

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
        arr[Token::Type::Keyword_Fun] = "Keyword_Fun";
        arr[Token::Type::Keyword_Return] = "Keyword_Return";
        arr[Token::Type::Keyword_If] = "Keyword_If";
        arr[Token::Type::Keyword_Else] = "Keyword_Else";
        arr[Token::Type::Intrinsic_Print] = "Intrinsic_Print";
        arr[Token::Type::Literal_Number] = "Literal_Number";
        arr[Token::Type::Identifier] = "Identifier";
        arr[Token::Type::Lparen] = "Lparen";
        arr[Token::Type::Rparen] = "Rparen";
        arr[Token::Type::Lbrace] = "Lbrace";
        arr[Token::Type::Rbrace] = "Rbrace";
        arr[Token::Type::Plus] = "Plus";
        arr[Token::Type::Minus] = "Minus";
        arr[Token::Type::Star] = "Star";
        arr[Token::Type::Slash] = "Slash";
        arr[Token::Type::Equals] = "Equals";
        arr[Token::Type::DoubleEquals] = "DoubleEquals";
        arr[Token::Type::NotEquals] = "NotEquals";
        arr[Token::Type::LessThan] = "LessThan";
        arr[Token::Type::GreaterThan] = "GreaterThan";
        arr[Token::Type::Comma] = "Comma";
        arr[Token::Type::Colon] = "Colon";
        arr[Token::Type::Semicolon] = "Semicolon";
        arr[Token::Type::Arrow] = "Arrow";
        arr[Token::Type::_EOF] = "_EOF";
        return arr;
    }();

    static constexpr std::array<Token::Type, 256> single_char_tokens_map = []() constexpr {
        std::array<Token::Type, 256> map{};
        map['('] = Token::Type::Lparen;
        map[')'] = Token::Type::Rparen;
        map['{'] = Token::Type::Lbrace;
        map['}'] = Token::Type::Rbrace;
        map['+'] = Token::Type::Plus;
        map['-'] = Token::Type::Minus;
        map['*'] = Token::Type::Star;
        map['/'] = Token::Type::Slash;
        map['<'] = Token::Type::LessThan;
        map['>'] = Token::Type::GreaterThan;
        map['='] = Token::Type::Equals;
        map['!'] = Token::Type::Unknown;  // Will be handled by two_char_intruducers_map for !=
        map[':'] = Token::Type::Colon;
        map[';'] = Token::Type::Semicolon;
        map[','] = Token::Type::Comma;
        return map;
    }();

    template<class T, class U>
    struct Pair {
        T first;
        U second;
    };

    static constexpr std::array<Pair<const char*, Token::Type>, 256> two_char_intruducers_map = []() constexpr {
        std::array<Pair<const char*, Token::Type>, 256> map{};
        map['-'] = {"->", Token::Type::Arrow};
        map['='] = {"==", Token::Type::DoubleEquals};
        map['!'] = {"!=", Token::Type::NotEquals};
        return map;
    }();

    static constexpr std::array<Token::Type, 5> keyword_types_list = {
        Token::Type::Keyword_Let,
        Token::Type::Keyword_Fun,
        Token::Type::Keyword_Return,
        Token::Type::Keyword_If,
        Token::Type::Keyword_Else,
    };
    static constexpr std::array<std::string_view, 5> keyword_strings_list = {
        "let",
        "fun",
        "return",
        "if",
        "else",
    };

    static constexpr std::array<Token::Type, 2> intrinsic_types_list = {
        Token::Type::Intrinsic_Print,
        Token::Type::Intrinsic_Halt,
    };
    static constexpr std::array<std::string_view, 2> intrinsic_strings_list = {
        "@print",
        "@halt",
    };
}