/*
 * Zex Programming Language
 * Parser/parser.c - Recursive descent parser with Pratt expressions
 */

#include "parser.h"
#include "error.h"
#include "memory.h"
#include "utf8.h"
#include <errno.h>

/* Current parser instance */
static Parser* current_parser = NULL;

/* Forward declarations */
static ASTNode* expression(void);
static ASTNode* statement(void);
static ASTNode* declaration(void);
static ASTNode* block(void);

/*
 * Token handling
 */

static void advance(void) {
    current_parser->previous = current_parser->current;
    
    for (;;) {
        current_parser->current = lexer_scan_token(&current_parser->lexer);
        
        if (current_parser->current.type != TOKEN_ERROR) break;
        
        /* Report lexer error */
        zex_error(ERROR_SYNTAX, current_parser->current.line,
                  current_parser->current.column, 1, "%s",
                  current_parser->current.start);
        current_parser->had_error = true;
    }
}

static bool check(TokenType type) {
    return current_parser->current.type == type;
}

static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

static void skip_newlines(void) {
    while (match(TOKEN_NEWLINE)) {
        /* Skip */
    }
}

static void consume(TokenType type, const char* message) {
    if (current_parser->current.type == type) {
        advance();
        return;
    }
    
    zex_error(ERROR_SYNTAX, current_parser->current.line,
              current_parser->current.column, current_parser->current.length,
              "%s", message);
    current_parser->had_error = true;
    current_parser->panic_mode = true;
}

static void consume_line_end(void) {
    /* Statement must end with newline, semicolon, or EOF */
    if (check(TOKEN_NEWLINE) || check(TOKEN_EOF) || check(TOKEN_RIGHT_BRACE) || check(TOKEN_SEMICOLON)) {
        if (check(TOKEN_NEWLINE) || check(TOKEN_SEMICOLON)) advance();
        return;
    }
    
    zex_error(ERROR_SYNTAX, current_parser->current.line,
              current_parser->current.column, current_parser->current.length,
              "Expected end of line after statement");
    current_parser->had_error = true;
}

static void synchronize(void) {
    current_parser->panic_mode = false;
    
    while (current_parser->current.type != TOKEN_EOF) {
        if (current_parser->previous.type == TOKEN_NEWLINE) return;
        
        switch (current_parser->current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_IF:
            case TOKEN_RETURN:
                return;
            default:
                break;
        }
        
        advance();
    }
}

/*
 * Expression parsing (Pratt parser)
 */

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,    /* = */
    PREC_OR,            /* || */
    PREC_AND,           /* && */
    PREC_EQUALITY,      /* == != */
    PREC_COMPARISON,    /* < > <= >= */
    PREC_TERM,          /* + - */
    PREC_FACTOR,        /* * / */
    PREC_UNARY,         /* ! - + */
    PREC_CALL,          /* . () */
    PREC_PRIMARY,
} Precedence;

typedef ASTNode* (*PrefixFn)(bool can_assign);
typedef ASTNode* (*InfixFn)(ASTNode* left, bool can_assign);

typedef struct {
    PrefixFn prefix;
    InfixFn infix;
    Precedence precedence;
} ParseRule;

static ASTNode* parse_precedence(Precedence precedence);
static ParseRule* get_rule(TokenType type);

/* Prefix parsers */

static ASTNode* number(bool can_assign) {
    UNUSED(can_assign);
    Token token = current_parser->previous;
    
    if (token.type == TOKEN_INT) {
        errno = 0;
        int64_t value = strtoll(token.start, NULL, 10);
        if (errno == ERANGE) {
            zex_error(ERROR_SYNTAX, token.line, token.column, token.length,
                      "Integer literal is too large");
        }
        return ast_new_int_literal(value, token.line, token.column);
    } else {
        double value = strtod(token.start, NULL);
        return ast_new_float_literal(value, token.line, token.column);
    }
}

static int hex_digit_value(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return 10 + (c - 'a');
    if (c >= 'A' && c <= 'F') return 10 + (c - 'A');
    return -1;
}

static char* process_escape_sequences(const char* src, int src_len, int* out_len) {
    /* Allocate buffer (may be smaller due to escapes, larger for Unicode) */
    int capacity = src_len + 16;
    char* result = ALLOCATE(char, capacity);
    int dst = 0;
    
    for (int i = 0; i < src_len; i++) {
        if (src[i] == '\\' && i + 1 < src_len) {
            i++;  /* Skip backslash */
            switch (src[i]) {
                case 'n': result[dst++] = '\n'; break;
                case 't': result[dst++] = '\t'; break;
                case 'r': result[dst++] = '\r'; break;
                case '\\': result[dst++] = '\\'; break;
                case '"': result[dst++] = '"'; break;
                case '\'': result[dst++] = '\''; break;
                case '0': result[dst++] = '\0'; break;
                
                case 'x': {
                    /* \xNN - hex byte */
                    if (i + 2 < src_len) {
                        int h1 = hex_digit_value(src[i + 1]);
                        int h2 = hex_digit_value(src[i + 2]);
                        if (h1 >= 0 && h2 >= 0) {
                            result[dst++] = (char)((h1 << 4) | h2);
                            i += 2;
                            break;
                        }
                    }
                    result[dst++] = '\\';
                    result[dst++] = 'x';
                    break;
                }
                
                case 'u': {
                    uint32_t codepoint = 0;
                    if (i + 1 < src_len && src[i + 1] == '{') {
                        /* \u{N...} - 1-6 hex digits */
                        i += 2;  /* Skip 'u{' */
                        int digits = 0;
                        while (i < src_len && src[i] != '}') {
                            int d = hex_digit_value(src[i]);
                            if (d < 0) break;
                            codepoint = (codepoint << 4) | d;
                            i++;
                            digits++;
                        }
                        if (i < src_len && src[i] == '}' && digits > 0 && digits <= 6) {
                            /* Expand buffer if needed */
                            if (dst + 4 >= capacity) {
                                capacity *= 2;
                                result = GROW_ARRAY(char, result, capacity / 2, capacity);
                            }
                            dst += utf8_encode(codepoint, result + dst);
                        } else {
                            result[dst++] = '\\';
                            result[dst++] = 'u';
                            i--;  /* Reprocess */
                        }
                    } else if (i + 4 < src_len) {
                        /* \uNNNN - exactly 4 hex digits */
                        int ok = 1;
                        for (int j = 1; j <= 4; j++) {
                            int d = hex_digit_value(src[i + j]);
                            if (d < 0) { ok = 0; break; }
                            codepoint = (codepoint << 4) | d;
                        }
                        if (ok) {
                            if (dst + 4 >= capacity) {
                                capacity *= 2;
                                result = GROW_ARRAY(char, result, capacity / 2, capacity);
                            }
                            dst += utf8_encode(codepoint, result + dst);
                            i += 4;
                        } else {
                            result[dst++] = '\\';
                            result[dst++] = 'u';
                        }
                    } else {
                        result[dst++] = '\\';
                        result[dst++] = 'u';
                    }
                    break;
                }
                
                default:
                    /* Unknown escape - keep as-is */
                    result[dst++] = '\\';
                    result[dst++] = src[i];
                    break;
            }
        } else {
            result[dst++] = src[i];
        }
        
        /* Grow buffer if needed */
        if (dst + 4 >= capacity) {
            capacity *= 2;
            result = GROW_ARRAY(char, result, capacity / 2, capacity);
        }
    }
    
    *out_len = dst;
    return result;
}

static ASTNode* string(bool can_assign) {
    UNUSED(can_assign);
    Token token = current_parser->previous;
    
    /* Get string content (without quotes) */
    const char* src = token.start + 1;
    int src_len = token.length - 2;
    
    /* Process escape sequences */
    int processed_len;
    char* processed = process_escape_sequences(src, src_len, &processed_len);
    
    ASTNode* node = ast_new_string_literal(processed, processed_len, 
                                           token.line, token.column);
    
    FREE_ARRAY(char, processed, processed_len + 1);
    return node;
}

static ASTNode* literal(bool can_assign) {
    UNUSED(can_assign);
    Token token = current_parser->previous;
    
    switch (token.type) {
        case TOKEN_TRUE:
            return ast_new_bool_literal(true, token.line, token.column);
        case TOKEN_FALSE:
            return ast_new_bool_literal(false, token.line, token.column);
        case TOKEN_NULL:
            return ast_new_null_literal(token.line, token.column);
        default:
            return NULL;  /* Unreachable */
    }
}

static ASTNode* self_expr(bool can_assign) {
    UNUSED(can_assign);
    Token token = current_parser->previous;
    return ast_new_self(token.line, token.column);
}

static ASTNode* grouping(bool can_assign) {
    UNUSED(can_assign);
    Token token = current_parser->previous;
    skip_newlines();
    
    /* Empty tuple () */
    if (check(TOKEN_RIGHT_PAREN)) {
        advance();
        return ast_new_tuple(NULL, 0, token.line, token.column);
    }
    
    /* Parse first expression */
    ASTNode* first = expression();
    skip_newlines();
    
    /* Check for comma - if present, it's a tuple */
    if (match(TOKEN_COMMA)) {
        ASTNode** elements = NULL;
        int count = 0;
        int capacity = 0;
        
        /* Add first element */
        if (count >= capacity) {
            int old_cap = capacity;
            capacity = capacity < 8 ? 8 : capacity * 2;
            elements = GROW_ARRAY(ASTNode*, elements, old_cap, capacity);
        }
        elements[count++] = first;
        
        skip_newlines();
        
        /* Parse remaining elements (or could be empty after comma for single-element tuple) */
        while (!check(TOKEN_RIGHT_PAREN) && !check(TOKEN_EOF)) {
            ASTNode* elem = expression();
            
            if (count >= capacity) {
                int old_cap = capacity;
                capacity = capacity * 2;
                elements = GROW_ARRAY(ASTNode*, elements, old_cap, capacity);
            }
            elements[count++] = elem;
            
            skip_newlines();
            if (!match(TOKEN_COMMA)) break;
            skip_newlines();
        }
        
        consume(TOKEN_RIGHT_PAREN, "Expected ')' after tuple elements");
        return ast_new_tuple(elements, count, token.line, token.column);
    }
    
    /* No comma - it's a grouping */
    consume(TOKEN_RIGHT_PAREN, "Expected ')' after expression");
    return ast_new_grouping(first, token.line, token.column);
}

static ASTNode* array_literal(bool can_assign) {
    UNUSED(can_assign);
    Token token = current_parser->previous;
    
    ASTNode** elements = NULL;
    int count = 0;
    int capacity = 0;
    
    skip_newlines();
    
    if (!check(TOKEN_RIGHT_BRACKET)) {
        do {
            skip_newlines();
            ASTNode* element = expression();
            
            if (count >= capacity) {
                capacity = GROW_CAPACITY(capacity);
                elements = GROW_ARRAY(ASTNode*, elements, count, capacity);
            }
            elements[count++] = element;
            
            skip_newlines();
        } while (match(TOKEN_COMMA));
    }
    
    skip_newlines();
    consume(TOKEN_RIGHT_BRACKET, "Expected ']' after array elements");
    
    return ast_new_array(elements, count, token.line, token.column);
}

static ASTNode* unary(bool can_assign) {
    UNUSED(can_assign);
    Token op_token = current_parser->previous;
    
    UnaryOp op;
    switch (op_token.type) {
        case TOKEN_MINUS: op = UNOP_NEG; break;
        case TOKEN_BANG:  op = UNOP_NOT; break;
        case TOKEN_PLUS:  op = UNOP_POS; break;
        default: return NULL;
    }
    
    ASTNode* operand = parse_precedence(PREC_UNARY);
    return ast_new_unary(op, operand, op_token.line, op_token.column);
}

static ASTNode* identifier(bool can_assign) {
    Token name_token = current_parser->previous;
    char* name = zex_strndup(name_token.start, name_token.length);
    
    /* Check for assignment */
    if (can_assign && match(TOKEN_EQUAL)) {
        ASTNode* value = expression();
        ASTNode* node = ast_new_assign(name, value, name_token.line, name_token.column);
        zex_free(name, name_token.length + 1);
        return node;
    }
    
    /* Check for compound assignment */
    if (can_assign) {
        CompoundOp op;
        bool is_compound = false;
        
        if (match(TOKEN_PLUS_EQUAL))  { op = COMPOUND_ADD; is_compound = true; }
        else if (match(TOKEN_MINUS_EQUAL)) { op = COMPOUND_SUB; is_compound = true; }
        else if (match(TOKEN_STAR_EQUAL))  { op = COMPOUND_MUL; is_compound = true; }
        else if (match(TOKEN_SLASH_EQUAL)) { op = COMPOUND_DIV; is_compound = true; }
        else if (match(TOKEN_PERCENT_EQUAL)) { op = COMPOUND_MOD; is_compound = true; }
        
        if (is_compound) {
            ASTNode* value = expression();
            ASTNode* node = ast_new_compound_assign(name, op, value, 
                                                    name_token.line, name_token.column);
            zex_free(name, name_token.length + 1);
            return node;
        }
    }
    
    ASTNode* node = ast_new_identifier(name, name_token.line, name_token.column);
    zex_free(name, name_token.length + 1);
    return node;
}

/* Infix parsers */

static ASTNode* binary(ASTNode* left, bool can_assign) {
    UNUSED(can_assign);
    Token op_token = current_parser->previous;
    
    BinaryOp op;
    switch (op_token.type) {
        case TOKEN_PLUS:          op = BINOP_ADD; break;
        case TOKEN_MINUS:         op = BINOP_SUB; break;
        case TOKEN_STAR:          op = BINOP_MUL; break;
        case TOKEN_SLASH:         op = BINOP_DIV; break;
        case TOKEN_PERCENT:       op = BINOP_MOD; break;
        case TOKEN_EQUAL_EQUAL:   op = BINOP_EQ; break;
        case TOKEN_BANG_EQUAL:    op = BINOP_NE; break;
        case TOKEN_LESS:          op = BINOP_LT; break;
        case TOKEN_LESS_EQUAL:    op = BINOP_LE; break;
        case TOKEN_GREATER:       op = BINOP_GT; break;
        case TOKEN_GREATER_EQUAL: op = BINOP_GE; break;
        case TOKEN_AND_AND:       op = BINOP_AND; break;
        case TOKEN_OR_OR:         op = BINOP_OR; break;
        default: return NULL;
    }
    
    ParseRule* rule = get_rule(op_token.type);
    ASTNode* right = parse_precedence((Precedence)(rule->precedence + 1));
    
    return ast_new_binary(op, left, right, op_token.line, op_token.column);
}

static ASTNode* call(ASTNode* callee, bool can_assign) {
    UNUSED(can_assign);
    Token paren = current_parser->previous;
    
    /* Parse arguments */
    ASTNode** args = NULL;
    int arg_count = 0;
    int arg_capacity = 0;
    
    skip_newlines();
    
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            skip_newlines();
            
            if (arg_count >= arg_capacity) {
                arg_capacity = GROW_CAPACITY(arg_capacity);
                args = GROW_ARRAY(ASTNode*, args, arg_count, arg_capacity);
            }
            args[arg_count++] = expression();
            
            skip_newlines();
        } while (match(TOKEN_COMMA));
    }
    
    skip_newlines();
    consume(TOKEN_RIGHT_PAREN, "Expected ')' after arguments");
    
    return ast_new_call(callee, args, arg_count, paren.line, paren.column);
}

static ASTNode* dot(ASTNode* left, bool can_assign) {
    Token dot_token = current_parser->previous;
    Token name_token;
    bool needs_recursive_dot = false;
    int recursive_index = 0;
    
    /* Accept identifier, integer, or float for property name.
     * For float tokens like "1.1", we extract the integer part and 
     * recursively process the ".1" part to support tuple.1.1 syntax. */
    if (match(TOKEN_IDENTIFIER)) {
        name_token = current_parser->previous;
    } else if (match(TOKEN_INT)) {
        name_token = current_parser->previous;
    } else if (match(TOKEN_FLOAT)) {
        /* Float token like "1.1" - extract just the integer part before the dot */
        Token float_token = current_parser->previous;
        
        /* Find the dot position in the float literal */
        const char* float_str = float_token.start;
        int dot_pos = 0;
        while (dot_pos < float_token.length && float_str[dot_pos] != '.') {
            dot_pos++;
        }
        
        /* Create a synthetic token for just the integer part */
        name_token.type = TOKEN_INT;
        name_token.start = float_token.start;
        name_token.length = dot_pos;
        name_token.line = float_token.line;
        name_token.column = float_token.column;
        
        /* Mark that we need to do a recursive .N access */
        needs_recursive_dot = true;
        
        /* Parse the fractional part as integer index */
        const char* frac_start = float_str + dot_pos + 1;
        int frac_len = float_token.length - dot_pos - 1;
        recursive_index = 0;
        for (int i = 0; i < frac_len; i++) {
            recursive_index = recursive_index * 10 + (frac_start[i] - '0');
        }
    } else {
        zex_error(ERROR_SYNTAX, current_parser->current.line,
                  current_parser->current.column, current_parser->current.length,
                  "Expected property name after '.'");
        current_parser->had_error = true;
        return NULL;
    }
    
    char* property = zex_strndup(name_token.start, name_token.length);
    
    /* If we split a float, we can't allow assignments - just build nested gets */
    if (needs_recursive_dot) {
        ASTNode* node = ast_new_get(left, property, dot_token.line, dot_token.column);
        zex_free(property, name_token.length + 1);
        
        /* Now process the recursive .N access */
        char index_str[32];
        snprintf(index_str, sizeof(index_str), "%d", recursive_index);
        char* index_prop = zex_strndup(index_str, strlen(index_str));
        node = ast_new_get(node, index_prop, dot_token.line, dot_token.column);
        /* index_prop ownership transferred to AST node */
        return node;
    }
    
    /* Original flow for normal property access */
    
    /* Check for property assignment */
    if (can_assign && match(TOKEN_EQUAL)) {
        ASTNode* value = expression();
        ASTNode* node = ast_new_set(left, property, value, dot_token.line, dot_token.column);
        zex_free(property, name_token.length + 1);
        return node;
    }
    
    /* Check for compound assignment on property */
    if (can_assign) {
        BinaryOp op;
        bool is_compound = false;
        
        if (match(TOKEN_PLUS_EQUAL))  { op = BINOP_ADD; is_compound = true; }
        else if (match(TOKEN_MINUS_EQUAL)) { op = BINOP_SUB; is_compound = true; }
        else if (match(TOKEN_STAR_EQUAL))  { op = BINOP_MUL; is_compound = true; }
        else if (match(TOKEN_SLASH_EQUAL)) { op = BINOP_DIV; is_compound = true; }
        else if (match(TOKEN_PERCENT_EQUAL)) { op = BINOP_MOD; is_compound = true; }
        
        if (is_compound) {
            ASTNode* value = expression();
            ASTNode* node = ast_new_set_compound(left, property, op, value,
                                                  dot_token.line, dot_token.column);
            zex_free(property, name_token.length + 1);
            return node;
        }
    }
    
    ASTNode* node = ast_new_get(left, property, dot_token.line, dot_token.column);
    zex_free(property, name_token.length + 1);
    return node;
}

static ASTNode* subscript(ASTNode* left, bool can_assign) {
    Token bracket_token = current_parser->previous;
    
    skip_newlines();
    ASTNode* index = expression();
    skip_newlines();
    consume(TOKEN_RIGHT_BRACKET, "Expected ']' after index");
    
    /* Check for assignment: arr[index] = value */
    if (can_assign && match(TOKEN_EQUAL)) {
        ASTNode* value = expression();
        return ast_new_index_set(left, index, value, bracket_token.line, bracket_token.column);
    }
    
    return ast_new_index_get(left, index, bracket_token.line, bracket_token.column);
}

static ASTNode* closure(bool can_assign) {
    UNUSED(can_assign);
    Token start = current_parser->previous;
    
    /* Parse parameter list between pipes: |a, b, c| */
    ParameterList params = {NULL, 0, 0};
    
    skip_newlines();
    
    /* Check for empty params: || */
    if (!check(TOKEN_PIPE)) {
        do {
            skip_newlines();
            
            if (!check(TOKEN_IDENTIFIER)) {
                zex_error(ERROR_SYNTAX, current_parser->current.line,
                          current_parser->current.column, current_parser->current.length,
                          "Expected parameter name in closure");
                current_parser->had_error = true;
                current_parser->panic_mode = true;
                break;
            }
            advance();
            
            if (params.count >= params.capacity) {
                params.capacity = GROW_CAPACITY(params.capacity);
                params.names = GROW_ARRAY(char*, params.names, params.count, params.capacity);
            }
            params.names[params.count++] = zex_strndup(current_parser->previous.start,
                                                        current_parser->previous.length);
            
            skip_newlines();
        } while (match(TOKEN_COMMA));
    }
    
    consume(TOKEN_PIPE, "Expected '|' after closure parameters");
    
    skip_newlines();
    
    /* Parse body: block or expression */
    ASTNode* body;
    bool is_expression;
    
    if (match(TOKEN_LEFT_BRACE)) {
        /* Block body: |x, y| { ... } */
        body = block();
        is_expression = false;
    } else {
        /* Expression body: |x, y| x + y */
        body = expression();
        is_expression = true;
    }
    
    return ast_new_closure(params, body, is_expression, start.line, start.column);
}

static ASTNode* closure_empty(bool can_assign) {
    /* || is zero-parameter closure */
    UNUSED(can_assign);
    Token start = current_parser->previous;
    
    ParameterList params = {NULL, 0, 0};
    
    skip_newlines();
    
    /* Parse body: block or expression */
    ASTNode* body;
    bool is_expression;
    
    if (match(TOKEN_LEFT_BRACE)) {
        body = block();
        is_expression = false;
    } else {
        body = expression();
        is_expression = true;
    }
    
    return ast_new_closure(params, body, is_expression, start.line, start.column);
}

/* Parse rules table */
static ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
    [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACKET]  = {array_literal, subscript, PREC_CALL},
    [TOKEN_RIGHT_BRACKET] = {NULL,     NULL,   PREC_NONE},
    [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
    [TOKEN_PLUS]          = {unary,    binary, PREC_TERM},
    [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
    [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
    [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
    [TOKEN_PERCENT]       = {NULL,     binary, PREC_FACTOR},
    [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_PLUS_EQUAL]    = {NULL,     NULL,   PREC_NONE},
    [TOKEN_MINUS_EQUAL]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_STAR_EQUAL]    = {NULL,     NULL,   PREC_NONE},
    [TOKEN_SLASH_EQUAL]   = {NULL,     NULL,   PREC_NONE},
    [TOKEN_PERCENT_EQUAL] = {NULL,     NULL,   PREC_NONE},
    [TOKEN_AND_AND]       = {NULL,     binary, PREC_AND},
    [TOKEN_OR_OR]         = {closure_empty, binary, PREC_OR},
    [TOKEN_IDENTIFIER]    = {identifier, NULL, PREC_NONE},
    [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
    [TOKEN_INT]           = {number,   NULL,   PREC_NONE},
    [TOKEN_FLOAT]         = {number,   NULL,   PREC_NONE},
    [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
    [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
    [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
    [TOKEN_NULL]          = {literal,  NULL,   PREC_NONE},
    [TOKEN_SELF]          = {self_expr, NULL,  PREC_NONE},
    [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
    [TOKEN_NEWLINE]       = {NULL,     NULL,   PREC_NONE},
    [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
    [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
    [TOKEN_HASH]          = {NULL,     NULL,   PREC_NONE},
    [TOKEN_PIPE]          = {closure,  NULL,   PREC_NONE},
};

static ParseRule* get_rule(TokenType type) {
    return &rules[type];
}

static ASTNode* parse_precedence(Precedence precedence) {
    advance();
    
    PrefixFn prefix_rule = get_rule(current_parser->previous.type)->prefix;
    if (prefix_rule == NULL) {
        zex_error(ERROR_SYNTAX, current_parser->previous.line,
                  current_parser->previous.column, current_parser->previous.length,
                  "Expected an expression");
        current_parser->had_error = true;
        return NULL;
    }
    
    bool can_assign = precedence <= PREC_ASSIGNMENT;
    ASTNode* left = prefix_rule(can_assign);
    
    for (;;) {
        /* Check current token for infix */
        TokenType next = current_parser->current.type;
        
        /* Allow newlines before . [ ( */
        if (next == TOKEN_NEWLINE) {
            Token saved = current_parser->current;
            Lexer saved_lexer = current_parser->lexer;
            skip_newlines();
            next = current_parser->current.type;
            
            /* Only continue if next is chaining operator (dot) */
            if (next != TOKEN_DOT) {
                current_parser->current = saved;
                current_parser->lexer = saved_lexer;
                break;
            }
        }
        
        ParseRule* rule = get_rule(next);
        if (precedence > rule->precedence) {
            break;
        }
        
        advance();
        InfixFn infix_rule = get_rule(current_parser->previous.type)->infix;
        left = infix_rule(left, can_assign);
    }
    
    return left;
}

static ASTNode* expression(void) {
    return parse_precedence(PREC_ASSIGNMENT);
}

/*
 * Statement parsing
 */

static ASTNode* block(void) {
    Token start = current_parser->previous;
    skip_newlines();
    
    ASTNode** statements = NULL;
    int count = 0;
    int capacity = 0;
    
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        ASTNode* stmt = declaration();
        if (stmt != NULL) {
            if (count >= capacity) {
                capacity = GROW_CAPACITY(capacity);
                statements = GROW_ARRAY(ASTNode*, statements, count, capacity);
            }
            statements[count++] = stmt;
        }
        skip_newlines();
    }
    
    consume(TOKEN_RIGHT_BRACE, "Expected '}' after block");
    
    return ast_new_block(statements, count, start.line, start.column);
}

static ASTNode* var_declaration(void) {
    Token var_token = current_parser->previous;
    consume(TOKEN_IDENTIFIER, "Expected variable name after 'var'");
    
    Token name_token = current_parser->previous;
    char* name = zex_strndup(name_token.start, name_token.length);
    
    ASTNode* initializer = NULL;
    if (match(TOKEN_EQUAL)) {
        initializer = expression();
    }
    
    consume_line_end();
    
    ASTNode* node = ast_new_var_decl(name, initializer, var_token.line, var_token.column);
    zex_free(name, name_token.length + 1);
    return node;
}

static ASTNode* fun_declaration(void) {
    Token fun_token = current_parser->previous;
    consume(TOKEN_IDENTIFIER, "Expected function name after 'fun'");
    
    Token name_token = current_parser->previous;
    char* name = zex_strndup(name_token.start, name_token.length);
    
    consume(TOKEN_LEFT_PAREN, "Expected '(' after function name");
    
    /* Parse parameters */
    ParameterList params = {NULL, 0, 0};
    
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            /* Allow 'self' keyword as parameter name (for methods) */
            if (!check(TOKEN_IDENTIFIER) && !check(TOKEN_SELF)) {
                zex_error(ERROR_SYNTAX, current_parser->current.line,
                          current_parser->current.column, current_parser->current.length,
                          "Expected parameter name");
                current_parser->had_error = true;
                current_parser->panic_mode = true;
                break;
            }
            advance();
            
            if (params.count >= params.capacity) {
                params.capacity = GROW_CAPACITY(params.capacity);
                params.names = GROW_ARRAY(char*, params.names, params.count, params.capacity);
            }
            params.names[params.count++] = zex_strndup(current_parser->previous.start,
                                                       current_parser->previous.length);
        } while (match(TOKEN_COMMA));
    }
    
    consume(TOKEN_RIGHT_PAREN, "Expected ')' after parameters");
    skip_newlines();
    consume(TOKEN_LEFT_BRACE, "Expected '{' before function body");
    
    ASTNode* body = block();
    
    ASTNode* node = ast_new_fun_decl(name, params, body, fun_token.line, fun_token.column);
    zex_free(name, name_token.length + 1);
    return node;
}

static ASTNode* class_declaration(void) {
    Token class_token = current_parser->previous;
    consume(TOKEN_IDENTIFIER, "Expected class name after 'class'");
    
    Token name_token = current_parser->previous;
    char* name = zex_strndup(name_token.start, name_token.length);
    
    /* Check for inheritance: class Child < Parent */
    char* superclass = NULL;
    skip_newlines();
    if (match(TOKEN_LESS)) {
        consume(TOKEN_IDENTIFIER, "Expected superclass name after '<'");
        Token super_token = current_parser->previous;
        superclass = zex_strndup(super_token.start, super_token.length);
        skip_newlines();
    }
    
    consume(TOKEN_LEFT_BRACE, "Expected '{' before class body");
    skip_newlines();
    
    /* Parse class body */
    ASTNode** methods = NULL;
    int method_count = 0;
    int method_capacity = 0;
    
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        skip_newlines();
        
        if (match(TOKEN_FUN)) {
            /* Method */
            ASTNode* method = fun_declaration();
            
            if (method_count >= method_capacity) {
                method_capacity = GROW_CAPACITY(method_capacity);
                methods = GROW_ARRAY(ASTNode*, methods, method_count, method_capacity);
            }
            methods[method_count++] = method;
            
        } else if (check(TOKEN_NEWLINE)) {
            advance();
        } else if (!check(TOKEN_RIGHT_BRACE)) {
            zex_error(ERROR_SYNTAX, current_parser->current.line,
                      current_parser->current.column, current_parser->current.length,
                      "Expected 'fun' in class body");
            current_parser->had_error = true;
            synchronize();
        }
    }
    
    consume(TOKEN_RIGHT_BRACE, "Expected '}' after class body");
    
    ASTNode* node = ast_new_class_decl(name, superclass, methods, method_count,
                                       class_token.line, class_token.column);
    zex_free(name, name_token.length + 1);
    if (superclass) {
        zex_free(superclass, strlen(superclass) + 1);
    }
    return node;
}

static ASTNode* if_statement(void) {
    Token if_token = current_parser->previous;
    
    ASTNode* condition = expression();
    
    skip_newlines();
    consume(TOKEN_LEFT_BRACE, "Expected '{' after if condition");
    ASTNode* then_branch = block();
    
    ASTNode* else_branch = NULL;
    skip_newlines();
    
    if (match(TOKEN_ELSE)) {
        skip_newlines();
        if (match(TOKEN_IF)) {
            /* else if */
            else_branch = if_statement();
        } else {
            consume(TOKEN_LEFT_BRACE, "Expected '{' after else");
            else_branch = block();
        }
    }
    
    return ast_new_if(condition, then_branch, else_branch, if_token.line, if_token.column);
}

static ASTNode* return_statement(void) {
    Token return_token = current_parser->previous;
    
    ASTNode* value = NULL;
    if (!check(TOKEN_NEWLINE) && !check(TOKEN_EOF) && !check(TOKEN_RIGHT_BRACE)) {
        value = expression();
    }
    
    consume_line_end();
    
    return ast_new_return(value, return_token.line, return_token.column);
}

static ASTNode* while_statement(void) {
    Token while_token = current_parser->previous;
    
    ASTNode* condition = expression();
    
    skip_newlines();
    consume(TOKEN_LEFT_BRACE, "Expected '{' after while condition");
    ASTNode* body = block();
    
    return ast_new_while(condition, body, while_token.line, while_token.column);
}

static ASTNode* do_while_statement(void) {
    Token do_token = current_parser->previous;
    
    skip_newlines();
    consume(TOKEN_LEFT_BRACE, "Expected '{' after do");
    ASTNode* body = block();
    
    skip_newlines();
    consume(TOKEN_WHILE, "Expected 'while' after do block");
    ASTNode* condition = expression();
    
    consume_line_end();
    
    return ast_new_do_while(body, condition, do_token.line, do_token.column);
}

static ASTNode* for_statement(void) {
    Token for_token = current_parser->previous;
    
    /* Optional parenthesis */
    bool has_paren = match(TOKEN_LEFT_PAREN);
    
    /* Must start with 'var' for the loop variable */
    if (!match(TOKEN_VAR)) {
        zex_error(ERROR_SYNTAX, current_parser->current.line,
                  current_parser->current.column, current_parser->current.length,
                  "Expected 'var' after 'for'");
        current_parser->had_error = true;
        return NULL;
    }
    
    /* Get variable name */
    consume(TOKEN_IDENTIFIER, "Expected variable name");
    Token var_token = current_parser->previous;
    char* var_name = zex_strndup(var_token.start, var_token.length);
    
    /* Check if this is for-in or C-style for */
    if (match(TOKEN_IN)) {
        /* For-in loop: for var x in arr { } */
        ASTNode* iterable = expression();
        
        if (has_paren) {
            consume(TOKEN_RIGHT_PAREN, "Expected ')' after for-in expression");
        }
        
        skip_newlines();
        consume(TOKEN_LEFT_BRACE, "Expected '{' before for body");
        ASTNode* body = block();
        
        ASTNode* result = ast_new_for_in(var_name, iterable, body, for_token.line, for_token.column);
        zex_free(var_name, var_token.length + 1);
        return result;
    }
    
    /* C-style for: for var i = 0; i < 10; i += 1 { } */
    
    /* Initializer: var i = expr */
    ASTNode* initializer = NULL;
    if (match(TOKEN_EQUAL)) {
        ASTNode* init_value = expression();
        initializer = ast_new_var_decl(var_name, init_value, var_token.line, var_token.column);
    } else {
        initializer = ast_new_var_decl(var_name, NULL, var_token.line, var_token.column);
    }
    
    consume(TOKEN_SEMICOLON, "Expected ';' after for initializer");
    
    /* Condition */
    ASTNode* condition = NULL;
    if (!check(TOKEN_SEMICOLON)) {
        condition = expression();
    }
    
    consume(TOKEN_SEMICOLON, "Expected ';' after for condition");
    
    /* Update */
    ASTNode* update = NULL;
    if (has_paren) {
        if (!check(TOKEN_RIGHT_PAREN)) {
            update = expression();
        }
        consume(TOKEN_RIGHT_PAREN, "Expected ')' after for clauses");
    } else {
        if (!check(TOKEN_LEFT_BRACE) && !check(TOKEN_NEWLINE)) {
            update = expression();
        }
    }
    
    skip_newlines();
    consume(TOKEN_LEFT_BRACE, "Expected '{' before for body");
    ASTNode* body = block();
    
    zex_free(var_name, var_token.length + 1);
    return ast_new_for(initializer, condition, update, body, for_token.line, for_token.column);
}

static ASTNode* expression_statement(void) {
    ASTNode* expr = expression();
    Token token = current_parser->previous;
    consume_line_end();
    return ast_new_expr_stmt(expr, token.line, token.column);
}

static ASTNode* break_statement(void) {
    Token break_token = current_parser->previous;
    
    /* Check for optional break value */
    ASTNode* value = NULL;
    if (!check(TOKEN_NEWLINE) && !check(TOKEN_EOF) && !check(TOKEN_RIGHT_BRACE) && !check(TOKEN_SEMICOLON)) {
        value = expression();
    }
    
    consume_line_end();
    return ast_new_break(value, break_token.line, break_token.column);
}

static ASTNode* continue_statement(void) {
    Token continue_token = current_parser->previous;
    consume_line_end();
    return ast_new_continue(continue_token.line, continue_token.column);
}

static ASTNode* try_statement(void) {
    Token try_token = current_parser->previous;
    
    skip_newlines();
    consume(TOKEN_LEFT_BRACE, "Expected '{' after try");
    ASTNode* try_body = block();
    
    /* Parse except handlers */
    ExceptHandler* handlers = NULL;
    int handler_count = 0;
    int handler_capacity = 0;
    
    skip_newlines();
    while (match(TOKEN_EXCEPT)) {
        ExceptHandler handler = {NULL, NULL, NULL};
        
        skip_newlines();
        
        /* Optional exception type */
        if (check(TOKEN_IDENTIFIER)) {
            advance();
            handler.type = zex_strndup(current_parser->previous.start,
                                        current_parser->previous.length);
        }
        
        /* Optional 'as var' */
        skip_newlines();
        if (match(TOKEN_AS)) {
            consume(TOKEN_IDENTIFIER, "Expected variable name after 'as'");
            handler.var = zex_strndup(current_parser->previous.start,
                                       current_parser->previous.length);
        }
        
        skip_newlines();
        consume(TOKEN_LEFT_BRACE, "Expected '{' after except");
        handler.body = block();
        
        /* Add handler to array */
        if (handler_count >= handler_capacity) {
            handler_capacity = GROW_CAPACITY(handler_capacity);
            handlers = GROW_ARRAY(ExceptHandler, handlers, handler_count, handler_capacity);
        }
        handlers[handler_count++] = handler;
        
        skip_newlines();
    }
    
    /* Parse optional else */
    ASTNode* else_body = NULL;
    if (match(TOKEN_ELSE)) {
        skip_newlines();
        consume(TOKEN_LEFT_BRACE, "Expected '{' after else");
        else_body = block();
        skip_newlines();
    }
    
    /* Parse optional finally */
    ASTNode* finally_body = NULL;
    if (match(TOKEN_FINALLY)) {
        skip_newlines();
        consume(TOKEN_LEFT_BRACE, "Expected '{' after finally");
        finally_body = block();
    }
    
    return ast_new_try(try_body, handlers, handler_count, else_body, finally_body,
                       try_token.line, try_token.column);
}

static ASTNode* raise_statement(void) {
    Token raise_token = current_parser->previous;
    
    ASTNode* exception = NULL;
    if (!check(TOKEN_NEWLINE) && !check(TOKEN_EOF) && !check(TOKEN_RIGHT_BRACE)) {
        exception = expression();
    }
    
    consume_line_end();
    return ast_new_raise(exception, raise_token.line, raise_token.column);
}

static ASTNode* statement(void) {
    if (match(TOKEN_TRY)) {
        return try_statement();
    }
    
    if (match(TOKEN_RAISE)) {
        return raise_statement();
    }
    
    if (match(TOKEN_IF)) {
        return if_statement();
    }
    
    if (match(TOKEN_WHILE)) {
        return while_statement();
    }
    
    if (match(TOKEN_DO)) {
        return do_while_statement();
    }
    
    if (match(TOKEN_FOR)) {
        return for_statement();
    }
    
    if (match(TOKEN_BREAK)) {
        return break_statement();
    }
    
    if (match(TOKEN_CONTINUE)) {
        return continue_statement();
    }
    
    if (match(TOKEN_RETURN)) {
        return return_statement();
    }
    
    if (match(TOKEN_LEFT_BRACE)) {
        return block();
    }
    
    return expression_statement();
}

static ASTNode* declaration(void) {
    skip_newlines();
    
    if (check(TOKEN_EOF)) return NULL;
    
    ASTNode* node = NULL;
    
    if (match(TOKEN_VAR)) {
        node = var_declaration();
    } else if (match(TOKEN_FUN)) {
        node = fun_declaration();
    } else if (match(TOKEN_CLASS)) {
        node = class_declaration();
    } else {
        node = statement();
    }
    
    if (current_parser->panic_mode) {
        synchronize();
    }
    
    return node;
}

/*
 * Public API
 */

void parser_init(Parser* parser, const char* source) {
    lexer_init(&parser->lexer, source);
    parser->had_error = false;
    parser->panic_mode = false;
    parser->current.type = TOKEN_ERROR;
    parser->previous.type = TOKEN_ERROR;
}

ASTNode* parse(const char* source) {
    Parser parser;
    parser_init(&parser, source);
    current_parser = &parser;
    
    advance();
    
    ASTNode** statements = NULL;
    int count = 0;
    int capacity = 0;
    
    while (!check(TOKEN_EOF)) {
        ASTNode* stmt = declaration();
        if (stmt != NULL) {
            if (count >= capacity) {
                capacity = GROW_CAPACITY(capacity);
                statements = GROW_ARRAY(ASTNode*, statements, count, capacity);
            }
            statements[count++] = stmt;
        }
    }
    
    if (parser.had_error) {
        /* Free partial AST on error */
        for (int i = 0; i < count; i++) {
            ast_free(statements[i]);
        }
        FREE_ARRAY(ASTNode*, statements, capacity);
        return NULL;
    }
    
    return ast_new_program(statements, count);
}
