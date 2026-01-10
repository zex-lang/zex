// Zex token definitions
// All lexical tokens recognized by the compiler

#ifndef ZEX_TOKEN_HPP
#define ZEX_TOKEN_HPP

#include <cstdint>
#include <string>

namespace zex {

// All token types recognized by the lexer
enum class TokenType {
    // Keywords
    KW_FUN,
    KW_VAR,
    KW_CONST,
    KW_RETURN,
    KW_VOID,
    KW_CHAR,
    KW_I8,
    KW_I16,
    KW_I32,
    KW_I64,
    KW_F32,
    KW_BOOL,
    KW_TRUE,
    KW_FALSE,
    KW_IF,
    KW_ELSE,
    KW_SIZEOF,

    // Delimiters
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    ARROW,
    COLON,
    SEMICOLON,
    COMMA,
    ASSIGN,

    // Operators
    PLUS,
    MINUS,
    STAR,
    SLASH,
    PERCENT,
    BANG,
    AMPERSAND,

    // Comparison
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,

    // Logical
    AND,
    OR,

    // Compound assignment
    PLUS_ASSIGN,
    MINUS_ASSIGN,
    STAR_ASSIGN,
    SLASH_ASSIGN,
    PERCENT_ASSIGN,

    // Literals and identifiers
    IDENTIFIER,
    INT_LITERAL,
    FLOAT_LITERAL,
    CHAR_LITERAL,
    STRING_LITERAL,

    // Special
    END_OF_FILE,
    INVALID
};

// Single token with type, value and source location
struct Token {
    TokenType type;
    std::string value;
    uint32_t line;
    uint32_t column;

    Token(TokenType t, std::string v, uint32_t ln, uint32_t col)
        : type(t), value(std::move(v)), line(ln), column(col) {}

    Token() : type(TokenType::INVALID), value(""), line(0), column(0) {}
};

// Returns string representation of token type for debugging
const char* token_type_to_string(TokenType type);

}  // namespace zex

#endif  // ZEX_TOKEN_HPP
