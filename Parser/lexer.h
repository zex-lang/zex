/*
 * Zex Programming Language
 * Parser/lexer.h - Lexer interface
 */

#ifndef ZEX_LEXER_H
#define ZEX_LEXER_H

#include "token.h"

typedef struct {
    const char* source;     /* Source code */
    const char* start;      /* Start of current token */
    const char* current;    /* Current character */
    int line;               /* Current line */
    int column;             /* Current column */
    int token_column;       /* Column where current token started */
} Lexer;

/* Initialize lexer with source code */
void lexer_init(Lexer* lexer, const char* source);

/* Scan the next token */
Token lexer_scan_token(Lexer* lexer);

/* Peek at current token without consuming */
Token lexer_peek_token(Lexer* lexer);

#endif /* ZEX_LEXER_H */
