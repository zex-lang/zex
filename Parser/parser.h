/*
 * Zex Programming Language
 * Parser/parser.h - Parser interface
 */

#ifndef ZEX_PARSER_H
#define ZEX_PARSER_H

#include "ast.h"
#include "lexer.h"

/* Parser state */
typedef struct {
    Lexer lexer;
    Token current;
    Token previous;
    bool had_error;
    bool panic_mode;
} Parser;

/* Initialize parser */
void parser_init(Parser* parser, const char* source);

/* Parse source code into AST */
ASTNode* parse(const char* source);

#endif /* ZEX_PARSER_H */
