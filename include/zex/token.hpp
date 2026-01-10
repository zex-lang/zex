#ifndef ZEX_TOKEN_HPP
#define ZEX_TOKEN_HPP

#include <cstdint>
#include <string>

namespace zex {

enum class TokenType {
    KW_FUN,
    KW_VAR,
    KW_CONST,
    KW_RETURN,
    KW_INT,
    KW_BOOL,
    KW_TRUE,
    KW_FALSE,
    KW_IF,
    KW_ELSE,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    ARROW,
    COLON,
    SEMICOLON,
    COMMA,
    ASSIGN,

    PLUS,
    MINUS,
    STAR,
    SLASH,
    PERCENT,
    BANG,

    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,

    AND,
    OR,

    PLUS_ASSIGN,
    MINUS_ASSIGN,
    STAR_ASSIGN,
    SLASH_ASSIGN,
    PERCENT_ASSIGN,

    IDENTIFIER,
    INT_LITERAL,

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

const char* token_type_to_string(TokenType type);

}  // namespace zex

#endif  // ZEX_TOKEN_HPP
