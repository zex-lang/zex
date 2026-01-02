/*
 * Zex Programming Language
 * Parser/token.c - Token utilities
 */

#include "token.h"

const char* token_type_name(TokenType type) {
    switch (type) {
        case TOKEN_LEFT_PAREN:      return "(";
        case TOKEN_RIGHT_PAREN:     return ")";
        case TOKEN_LEFT_BRACE:      return "{";
        case TOKEN_RIGHT_BRACE:     return "}";
        case TOKEN_LEFT_BRACKET:    return "[";
        case TOKEN_RIGHT_BRACKET:   return "]";
        case TOKEN_COMMA:           return ",";
        case TOKEN_DOT:             return ".";
        case TOKEN_PLUS:            return "+";
        case TOKEN_MINUS:           return "-";
        case TOKEN_STAR:            return "*";
        case TOKEN_SLASH:           return "/";
        case TOKEN_PERCENT:         return "%";
        case TOKEN_HASH:            return "#";
        case TOKEN_SEMICOLON:       return ";";
        case TOKEN_BANG:            return "!";
        case TOKEN_BANG_EQUAL:      return "!=";
        case TOKEN_EQUAL:           return "=";
        case TOKEN_EQUAL_EQUAL:     return "==";
        case TOKEN_GREATER:         return ">";
        case TOKEN_GREATER_EQUAL:   return ">=";
        case TOKEN_LESS:            return "<";
        case TOKEN_LESS_EQUAL:      return "<=";
        case TOKEN_PLUS_EQUAL:      return "+=";
        case TOKEN_MINUS_EQUAL:     return "-=";
        case TOKEN_STAR_EQUAL:      return "*=";
        case TOKEN_SLASH_EQUAL:     return "/=";
        case TOKEN_PERCENT_EQUAL:   return "%=";
        case TOKEN_AND_AND:         return "&&";
        case TOKEN_OR_OR:           return "||";
        case TOKEN_PIPE:            return "|";
        case TOKEN_IDENTIFIER:      return "identifier";
        case TOKEN_STRING:          return "string";
        case TOKEN_INT:             return "integer";
        case TOKEN_FLOAT:           return "float";
        case TOKEN_VAR:             return "var";
        case TOKEN_FUN:             return "fun";
        case TOKEN_CLASS:           return "class";
        case TOKEN_IF:              return "if";
        case TOKEN_ELSE:            return "else";
        case TOKEN_WHILE:           return "while";
        case TOKEN_DO:              return "do";
        case TOKEN_BREAK:           return "break";
        case TOKEN_CONTINUE:        return "continue";
        case TOKEN_RETURN:          return "return";
        case TOKEN_TRUE:            return "true";
        case TOKEN_FALSE:           return "false";
        case TOKEN_NULL:            return "null";
        case TOKEN_SELF:            return "self";
        case TOKEN_FOR:             return "for";
        case TOKEN_IN:              return "in";
        case TOKEN_AND:             return "and";
        case TOKEN_OR:              return "or";
        case TOKEN_TRY:             return "try";
        case TOKEN_EXCEPT:          return "except";
        case TOKEN_FINALLY:         return "finally";
        case TOKEN_RAISE:           return "raise";
        case TOKEN_AS:              return "as";
        case TOKEN_NEWLINE:         return "newline";
        case TOKEN_EOF:             return "end of file";
        case TOKEN_ERROR:           return "error";
        default:                    return "unknown";
    }
}
