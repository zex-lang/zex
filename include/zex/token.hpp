#ifndef ZEX_TOKEN_HPP
#define ZEX_TOKEN_HPP

#include <cstdint>
#include <string>

namespace zex {

enum class TokenType {
    // Keywords
    KW_FUN,     // fun
    KW_VAR,     // var
    KW_RETURN,  // return
    KW_INT,     // int

    // Symbols
    LPAREN,     // (
    RPAREN,     // )
    LBRACE,     // {
    RBRACE,     // }
    ARROW,      // ->
    COLON,      // :
    SEMICOLON,  // ;
    ASSIGN,     // =

    // Literals and identifiers
    IDENTIFIER,
    INT_LITERAL,

    // Special
    END_OF_FILE,
    INVALID
};

struct Token {
    TokenType type;
    std::string value;
    uint32_t line;
    uint32_t column;

    Token(TokenType t, std::string v, uint32_t ln, uint32_t col)
        : type(t), value(std::move(v)), line(ln), column(col) {}

    Token() : type(TokenType::INVALID), value(""), line(0), column(0) {}
};

// Convert token type to string for debugging
const char* token_type_to_string(TokenType type);

}  // namespace zex

#endif  // ZEX_TOKEN_HPP
