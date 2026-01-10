#include "zex/lexer.hpp"

#include <unordered_map>

#include "zex/error.hpp"

namespace zex {

const char* token_type_to_string(TokenType type) {
    switch (type) {
        case TokenType::KW_FUN:
            return "fun";
        case TokenType::KW_VAR:
            return "var";
        case TokenType::KW_CONST:
            return "const";
        case TokenType::KW_RETURN:
            return "return";
        case TokenType::KW_VOID:
            return "void";
        case TokenType::KW_CHAR:
            return "char";
        case TokenType::KW_I8:
            return "i8";
        case TokenType::KW_I16:
            return "i16";
        case TokenType::KW_I32:
            return "i32";
        case TokenType::KW_I64:
            return "i64";
        case TokenType::KW_F32:
            return "f32";
        case TokenType::KW_BOOL:
            return "bool";
        case TokenType::KW_TRUE:
            return "true";
        case TokenType::KW_FALSE:
            return "false";
        case TokenType::KW_IF:
            return "if";
        case TokenType::KW_ELSE:
            return "else";
        case TokenType::KW_SIZEOF:
            return "sizeof";
        case TokenType::LPAREN:
            return "(";
        case TokenType::RPAREN:
            return ")";
        case TokenType::LBRACE:
            return "{";
        case TokenType::RBRACE:
            return "}";
        case TokenType::LBRACKET:
            return "[";
        case TokenType::RBRACKET:
            return "]";
        case TokenType::ARROW:
            return "->";
        case TokenType::COLON:
            return ":";
        case TokenType::SEMICOLON:
            return ";";
        case TokenType::COMMA:
            return ",";
        case TokenType::ASSIGN:
            return "=";
        case TokenType::PLUS:
            return "+";
        case TokenType::MINUS:
            return "-";
        case TokenType::STAR:
            return "*";
        case TokenType::SLASH:
            return "/";
        case TokenType::PERCENT:
            return "%";
        case TokenType::BANG:
            return "!";
        case TokenType::AMPERSAND:
            return "&";
        case TokenType::EQ:
            return "==";
        case TokenType::NE:
            return "!=";
        case TokenType::LT:
            return "<";
        case TokenType::GT:
            return ">";
        case TokenType::LE:
            return "<=";
        case TokenType::GE:
            return ">=";
        case TokenType::AND:
            return "&&";
        case TokenType::OR:
            return "||";
        case TokenType::PLUS_ASSIGN:
            return "+=";
        case TokenType::MINUS_ASSIGN:
            return "-=";
        case TokenType::STAR_ASSIGN:
            return "*=";
        case TokenType::SLASH_ASSIGN:
            return "/=";
        case TokenType::PERCENT_ASSIGN:
            return "%=";
        case TokenType::IDENTIFIER:
            return "identifier";
        case TokenType::INT_LITERAL:
            return "integer";
        case TokenType::FLOAT_LITERAL:
            return "float";
        case TokenType::CHAR_LITERAL:
            return "char";
        case TokenType::STRING_LITERAL:
            return "string";
        case TokenType::KW_ASM:
            return "asm";
        case TokenType::END_OF_FILE:
            return "eof";
        case TokenType::INVALID:
            return "invalid";
    }
    return "unknown";
}

Lexer::Lexer(std::string_view source) : source_(source), pos_(0), line_(1), column_(1) {}

std::vector<Token> Lexer::tokenize() {
    std::vector<Token> tokens;
    while (!at_end()) {
        skip_whitespace();
        if (!at_end()) {
            Token tok = scan_token();
            if (tok.type == TokenType::INVALID) {
                throw CompileError(ErrorCode::UNEXPECTED_CHARACTER, {tok.line, tok.column},
                                   tok.value);
            }
            tokens.push_back(tok);
        }
    }
    tokens.push_back(make_token(TokenType::END_OF_FILE));
    return tokens;
}

char Lexer::current() const {
    if (at_end())
        return '\0';
    return source_[pos_];
}

char Lexer::peek(size_t offset) const {
    if (pos_ + offset >= source_.size())
        return '\0';
    return source_[pos_ + offset];
}

void Lexer::advance() {
    if (!at_end()) {
        if (current() == '\n') {
            line_++;
            column_ = 1;
        } else {
            column_++;
        }
        pos_++;
    }
}

bool Lexer::at_end() const {
    return pos_ >= source_.size();
}

Token Lexer::make_token(TokenType type) {
    return Token(type, "", line_, column_);
}

Token Lexer::make_token(TokenType type, const std::string& value) {
    return Token(type, value, line_, column_);
}

void Lexer::skip_whitespace() {
    while (!at_end()) {
        char c = current();
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
            advance();
        } else if (c == '/' && peek() == '/') {
            while (!at_end() && current() != '\n') {
                advance();
            }
        } else {
            break;
        }
    }
}

bool Lexer::is_alpha(char c) const {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

bool Lexer::is_digit(char c) const {
    return c >= '0' && c <= '9';
}

bool Lexer::is_alnum(char c) const {
    return is_alpha(c) || is_digit(c);
}

char Lexer::parse_escape_sequence() {
    advance();  // skip backslash
    char c = current();
    advance();
    switch (c) {
        case 'n':
            return '\n';
        case 't':
            return '\t';
        case 'r':
            return '\r';
        case '\\':
            return '\\';
        case '\'':
            return '\'';
        case '\"':
            return '\"';
        case '0':
            return '\0';
        default:
            return c;
    }
}

TokenType Lexer::check_keyword(const std::string& word) const {
    static const std::unordered_map<std::string, TokenType> keywords = {
        {"fun", TokenType::KW_FUN},       {"var", TokenType::KW_VAR},
        {"const", TokenType::KW_CONST},   {"return", TokenType::KW_RETURN},
        {"void", TokenType::KW_VOID},     {"char", TokenType::KW_CHAR},
        {"i8", TokenType::KW_I8},         {"i16", TokenType::KW_I16},
        {"i32", TokenType::KW_I32},       {"i64", TokenType::KW_I64},
        {"f32", TokenType::KW_F32},       {"bool", TokenType::KW_BOOL},
        {"true", TokenType::KW_TRUE},     {"false", TokenType::KW_FALSE},
        {"if", TokenType::KW_IF},         {"else", TokenType::KW_ELSE},
        {"sizeof", TokenType::KW_SIZEOF}, {"asm", TokenType::KW_ASM}};

    auto it = keywords.find(word);
    if (it != keywords.end()) {
        return it->second;
    }
    return TokenType::IDENTIFIER;
}

Token Lexer::scan_identifier() {
    size_t start = pos_;
    uint32_t start_col = column_;
    while (!at_end() && is_alnum(current())) {
        advance();
    }
    std::string word(source_.substr(start, pos_ - start));
    TokenType type = check_keyword(word);
    return Token(type, word, line_, start_col);
}

Token Lexer::scan_number() {
    size_t start = pos_;
    uint32_t start_col = column_;
    while (!at_end() && is_digit(current())) {
        advance();
    }
    if (current() == '.' && is_digit(peek())) {
        advance();
        while (!at_end() && is_digit(current())) {
            advance();
        }
        std::string num(source_.substr(start, pos_ - start));
        return Token(TokenType::FLOAT_LITERAL, num, line_, start_col);
    }
    std::string num(source_.substr(start, pos_ - start));
    return Token(TokenType::INT_LITERAL, num, line_, start_col);
}

Token Lexer::scan_char_literal() {
    uint32_t start_col = column_;
    advance();  // skip opening '

    if (at_end()) {
        return make_token(TokenType::INVALID, "unterminated char literal");
    }

    char value;
    if (current() == '\\') {
        value = parse_escape_sequence();
    } else {
        value = current();
        advance();
    }

    if (current() != '\'') {
        return make_token(TokenType::INVALID, "unterminated char literal");
    }
    advance();  // skip closing '

    return Token(TokenType::CHAR_LITERAL, std::string(1, value), line_, start_col);
}

Token Lexer::scan_string_literal() {
    uint32_t start_col = column_;
    advance();  // skip opening "

    std::string value;
    while (!at_end() && current() != '"') {
        if (current() == '\\') {
            value += parse_escape_sequence();
        } else {
            value += current();
            advance();
        }
    }

    if (current() != '"') {
        return make_token(TokenType::INVALID, "unterminated string literal");
    }
    advance();  // skip closing "

    return Token(TokenType::STRING_LITERAL, value, line_, start_col);
}

Token Lexer::scan_token() {
    char c = current();

    if (c == '(') {
        advance();
        return make_token(TokenType::LPAREN);
    }
    if (c == ')') {
        advance();
        return make_token(TokenType::RPAREN);
    }
    if (c == '{') {
        advance();
        return make_token(TokenType::LBRACE);
    }
    if (c == '}') {
        advance();
        return make_token(TokenType::RBRACE);
    }
    if (c == '[') {
        advance();
        return make_token(TokenType::LBRACKET);
    }
    if (c == ']') {
        advance();
        return make_token(TokenType::RBRACKET);
    }
    if (c == ':') {
        advance();
        return make_token(TokenType::COLON);
    }
    if (c == ';') {
        advance();
        return make_token(TokenType::SEMICOLON);
    }
    if (c == ',') {
        advance();
        return make_token(TokenType::COMMA);
    }

    if (c == '+' && peek() == '=') {
        advance();
        advance();
        return make_token(TokenType::PLUS_ASSIGN);
    }
    if (c == '+') {
        advance();
        return make_token(TokenType::PLUS);
    }

    if (c == '*' && peek() == '=') {
        advance();
        advance();
        return make_token(TokenType::STAR_ASSIGN);
    }
    if (c == '*') {
        advance();
        return make_token(TokenType::STAR);
    }

    if (c == '/' && peek() == '=') {
        advance();
        advance();
        return make_token(TokenType::SLASH_ASSIGN);
    }
    if (c == '/') {
        advance();
        return make_token(TokenType::SLASH);
    }

    if (c == '%' && peek() == '=') {
        advance();
        advance();
        return make_token(TokenType::PERCENT_ASSIGN);
    }
    if (c == '%') {
        advance();
        return make_token(TokenType::PERCENT);
    }

    if (c == '=' && peek() == '=') {
        advance();
        advance();
        return make_token(TokenType::EQ);
    }
    if (c == '=') {
        advance();
        return make_token(TokenType::ASSIGN);
    }

    if (c == '!' && peek() == '=') {
        advance();
        advance();
        return make_token(TokenType::NE);
    }
    if (c == '!') {
        advance();
        return make_token(TokenType::BANG);
    }

    if (c == '<' && peek() == '=') {
        advance();
        advance();
        return make_token(TokenType::LE);
    }
    if (c == '<') {
        advance();
        return make_token(TokenType::LT);
    }

    if (c == '>' && peek() == '=') {
        advance();
        advance();
        return make_token(TokenType::GE);
    }
    if (c == '>') {
        advance();
        return make_token(TokenType::GT);
    }

    if (c == '&' && peek() == '&') {
        advance();
        advance();
        return make_token(TokenType::AND);
    }
    if (c == '&') {
        advance();
        return make_token(TokenType::AMPERSAND);
    }

    if (c == '|' && peek() == '|') {
        advance();
        advance();
        return make_token(TokenType::OR);
    }

    if (c == '-' && peek() == '>') {
        advance();
        advance();
        return make_token(TokenType::ARROW);
    }
    if (c == '-' && peek() == '=') {
        advance();
        advance();
        return make_token(TokenType::MINUS_ASSIGN);
    }
    if (c == '-') {
        advance();
        return make_token(TokenType::MINUS);
    }

    if (c == '\'') {
        return scan_char_literal();
    }
    if (c == '"') {
        return scan_string_literal();
    }

    if (is_alpha(c)) {
        return scan_identifier();
    }
    if (is_digit(c)) {
        return scan_number();
    }

    Token invalid = make_token(TokenType::INVALID, std::string(1, c));
    advance();
    return invalid;
}

}  // namespace zex
