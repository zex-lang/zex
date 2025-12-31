/*
 * Zex Programming Language
 * Parser/token.h - Token definitions
 */

#ifndef ZEX_TOKEN_H
#define ZEX_TOKEN_H

#include "common.h"

typedef enum {
    /* Single-character tokens */
    TOKEN_LEFT_PAREN,       /* ( */
    TOKEN_RIGHT_PAREN,      /* ) */
    TOKEN_LEFT_BRACE,       /* { */
    TOKEN_RIGHT_BRACE,      /* } */
    TOKEN_COMMA,            /* , */
    TOKEN_DOT,              /* . */
    TOKEN_PLUS,             /* + */
    TOKEN_MINUS,            /* - */
    TOKEN_STAR,             /* * */
    TOKEN_SLASH,            /* / */
    TOKEN_HASH,             /* # (comment) */
    
    /* One or two character tokens */
    TOKEN_BANG,             /* ! */
    TOKEN_BANG_EQUAL,       /* != */
    TOKEN_EQUAL,            /* = */
    TOKEN_EQUAL_EQUAL,      /* == */
    TOKEN_GREATER,          /* > */
    TOKEN_GREATER_EQUAL,    /* >= */
    TOKEN_LESS,             /* < */
    TOKEN_LESS_EQUAL,       /* <= */
    TOKEN_PLUS_EQUAL,       /* += */
    TOKEN_MINUS_EQUAL,      /* -= */
    TOKEN_STAR_EQUAL,       /* *= */
    TOKEN_SLASH_EQUAL,      /* /= */
    TOKEN_AND_AND,          /* && */
    TOKEN_OR_OR,            /* || */
    
    /* Literals */
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_INT,
    TOKEN_FLOAT,
    
    /* Keywords */
    TOKEN_VAR,
    TOKEN_FUN,
    TOKEN_CLASS,
    TOKEN_IF,
    TOKEN_ELSE,
    TOKEN_WHILE,
    TOKEN_DO,
    TOKEN_BREAK,
    TOKEN_CONTINUE,
    TOKEN_RETURN,
    TOKEN_TRUE,
    TOKEN_FALSE,
    TOKEN_NULL,
    TOKEN_SELF,
    TOKEN_AND,              /* Reserved for future */
    TOKEN_OR,               /* Reserved for future */
    
    /* Special */
    TOKEN_NEWLINE,
    TOKEN_SEMICOLON,        /* Error token - semicolons not allowed */
    TOKEN_EOF,
    TOKEN_ERROR,
} TokenType;

typedef struct {
    TokenType type;
    const char* start;      /* Pointer to start in source */
    int length;             /* Token length */
    int line;
    int column;
} Token;

/* Get token type name for debugging/errors */
const char* token_type_name(TokenType type);

#endif /* ZEX_TOKEN_H */
