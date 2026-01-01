/*
 * Zex Programming Language
 * Parser/lexer.c - Lexer implementation
 */

#include "lexer.h"
#include "error.h"
#include <ctype.h>

void lexer_init(Lexer* lexer, const char* source) {
    lexer->source = source;
    lexer->start = source;
    lexer->current = source;
    lexer->line = 1;
    lexer->column = 1;
    lexer->token_column = 1;
}

static bool is_at_end(Lexer* lexer) {
    return *lexer->current == '\0';
}

static char advance(Lexer* lexer) {
    lexer->current++;
    lexer->column++;
    return lexer->current[-1];
}

static char peek(Lexer* lexer) {
    return *lexer->current;
}

static char peek_next(Lexer* lexer) {
    if (is_at_end(lexer)) return '\0';
    return lexer->current[1];
}

static bool match(Lexer* lexer, char expected) {
    if (is_at_end(lexer)) return false;
    if (*lexer->current != expected) return false;
    lexer->current++;
    lexer->column++;
    return true;
}

static Token make_token(Lexer* lexer, TokenType type) {
    Token token;
    token.type = type;
    token.start = lexer->start;
    token.length = (int)(lexer->current - lexer->start);
    token.line = lexer->line;
    token.column = lexer->token_column;
    return token;
}

static Token error_token(Lexer* lexer, const char* message) {
    Token token;
    token.type = TOKEN_ERROR;
    token.start = message;
    token.length = (int)strlen(message);
    token.line = lexer->line;
    token.column = lexer->token_column;
    return token;
}

static void skip_whitespace(Lexer* lexer) {
    for (;;) {
        char c = peek(lexer);
        switch (c) {
            case ' ':
            case '\r':
            case '\t':
                advance(lexer);
                break;
            case '#':
                /* Comment extends to end of line */
                while (peek(lexer) != '\n' && !is_at_end(lexer)) {
                    advance(lexer);
                }
                break;
            default:
                return;
        }
    }
}

static TokenType check_keyword(Lexer* lexer, int start, int length,
                               const char* rest, TokenType type) {
    if (lexer->current - lexer->start == start + length &&
        memcmp(lexer->start + start, rest, length) == 0) {
        return type;
    }
    return TOKEN_IDENTIFIER;
}

static TokenType identifier_type(Lexer* lexer) {
    switch (lexer->start[0]) {
        case 'b': return check_keyword(lexer, 1, 4, "reak", TOKEN_BREAK);
        case 'c':
            if (lexer->current - lexer->start > 1) {
                switch (lexer->start[1]) {
                    case 'l': return check_keyword(lexer, 2, 3, "ass", TOKEN_CLASS);
                    case 'o': return check_keyword(lexer, 2, 6, "ntinue", TOKEN_CONTINUE);
                }
            }
            break;
        case 'd': return check_keyword(lexer, 1, 1, "o", TOKEN_DO);
        case 'e': return check_keyword(lexer, 1, 3, "lse", TOKEN_ELSE);
        case 'f':
            if (lexer->current - lexer->start > 1) {
                switch (lexer->start[1]) {
                    case 'a': return check_keyword(lexer, 2, 3, "lse", TOKEN_FALSE);
                    case 'o': return check_keyword(lexer, 2, 1, "r", TOKEN_FOR);
                    case 'u': return check_keyword(lexer, 2, 1, "n", TOKEN_FUN);
                }
            }
            break;
        case 'i':
            if (lexer->current - lexer->start > 1) {
                switch (lexer->start[1]) {
                    case 'f': return check_keyword(lexer, 2, 0, "", TOKEN_IF);
                    case 'n': return check_keyword(lexer, 2, 0, "", TOKEN_IN);
                }
            }
            break;
        case 'n': return check_keyword(lexer, 1, 3, "ull", TOKEN_NULL);
        case 'r': return check_keyword(lexer, 1, 5, "eturn", TOKEN_RETURN);
        case 's': return check_keyword(lexer, 1, 3, "elf", TOKEN_SELF);
        case 't': return check_keyword(lexer, 1, 3, "rue", TOKEN_TRUE);
        case 'v': return check_keyword(lexer, 1, 2, "ar", TOKEN_VAR);
        case 'w': return check_keyword(lexer, 1, 4, "hile", TOKEN_WHILE);
    }
    return TOKEN_IDENTIFIER;
}

static Token identifier(Lexer* lexer) {
    while (isalnum(peek(lexer)) || peek(lexer) == '_') {
        advance(lexer);
    }
    return make_token(lexer, identifier_type(lexer));
}

static Token number(Lexer* lexer) {
    bool is_float = false;
    
    while (isdigit(peek(lexer))) {
        advance(lexer);
    }
    
    /* Look for decimal part */
    if (peek(lexer) == '.' && isdigit(peek_next(lexer))) {
        is_float = true;
        advance(lexer);  /* Consume '.' */
        
        while (isdigit(peek(lexer))) {
            advance(lexer);
        }
    }
    
    return make_token(lexer, is_float ? TOKEN_FLOAT : TOKEN_INT);
}

static Token string(Lexer* lexer) {
    int start_line = lexer->line;
    
    while (peek(lexer) != '"' && !is_at_end(lexer)) {
        if (peek(lexer) == '\\' && peek_next(lexer) != '\0') {
            /* Escape sequence - skip both characters */
            advance(lexer);
            advance(lexer);
        } else if (peek(lexer) == '\n') {
            lexer->line++;
            lexer->column = 0;
            advance(lexer);
        } else {
            advance(lexer);
        }
    }
    
    if (is_at_end(lexer)) {
        UNUSED(start_line);
        return error_token(lexer, "Unterminated string literal");
    }
    
    /* Closing quote */
    advance(lexer);
    return make_token(lexer, TOKEN_STRING);
}

Token lexer_scan_token(Lexer* lexer) {
    skip_whitespace(lexer);
    
    lexer->start = lexer->current;
    lexer->token_column = lexer->column;
    
    if (is_at_end(lexer)) {
        return make_token(lexer, TOKEN_EOF);
    }
    
    char c = advance(lexer);
    
    /* Identifiers and keywords */
    if (isalpha(c) || c == '_') {
        return identifier(lexer);
    }
    
    /* Numbers */
    if (isdigit(c)) {
        return number(lexer);
    }
    
    switch (c) {
        /* Single character tokens */
        case '(': return make_token(lexer, TOKEN_LEFT_PAREN);
        case ')': return make_token(lexer, TOKEN_RIGHT_PAREN);
        case '{': return make_token(lexer, TOKEN_LEFT_BRACE);
        case '}': return make_token(lexer, TOKEN_RIGHT_BRACE);
        case '[': return make_token(lexer, TOKEN_LEFT_BRACKET);
        case ']': return make_token(lexer, TOKEN_RIGHT_BRACKET);
        case ',': return make_token(lexer, TOKEN_COMMA);
        case '.': return make_token(lexer, TOKEN_DOT);
        
        /* Operators with potential second character */
        case '+':
            return make_token(lexer, match(lexer, '=') ? TOKEN_PLUS_EQUAL : TOKEN_PLUS);
        case '-':
            return make_token(lexer, match(lexer, '=') ? TOKEN_MINUS_EQUAL : TOKEN_MINUS);
        case '*':
            return make_token(lexer, match(lexer, '=') ? TOKEN_STAR_EQUAL : TOKEN_STAR);
        case '/':
            return make_token(lexer, match(lexer, '=') ? TOKEN_SLASH_EQUAL : TOKEN_SLASH);
        
        case '!':
            return make_token(lexer, match(lexer, '=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
        case '=':
            return make_token(lexer, match(lexer, '=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
        case '<':
            return make_token(lexer, match(lexer, '=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
        case '>':
            return make_token(lexer, match(lexer, '=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
        
        case '&':
            if (match(lexer, '&')) return make_token(lexer, TOKEN_AND_AND);
            return error_token(lexer, "Expected '&&' for logical AND");
        case '|':
            if (match(lexer, '|')) return make_token(lexer, TOKEN_OR_OR);
            return error_token(lexer, "Expected '||' for logical OR");
        
        /* String literal */
        case '"': return string(lexer);
        
        /* Newline */
        case '\n':
            lexer->line++;
            lexer->column = 1;
            return make_token(lexer, TOKEN_NEWLINE);
        
        /* Semicolon */
        case ';': return make_token(lexer, TOKEN_SEMICOLON);
        
    }
    
    return error_token(lexer, "Unexpected character");
}

Token lexer_peek_token(Lexer* lexer) {
    /* Save state */
    const char* saved_start = lexer->start;
    const char* saved_current = lexer->current;
    int saved_line = lexer->line;
    int saved_column = lexer->column;
    int saved_token_column = lexer->token_column;
    
    Token token = lexer_scan_token(lexer);
    
    /* Restore state */
    lexer->start = saved_start;
    lexer->current = saved_current;
    lexer->line = saved_line;
    lexer->column = saved_column;
    lexer->token_column = saved_token_column;
    
    return token;
}
