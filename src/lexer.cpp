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
        case TokenType::KW_RETURN:
            return "return";
        case TokenType::KW_INT:
            return "int";
        case TokenType::LPAREN:
            return "(";
        case TokenType::RPAREN:
            return ")";
        case TokenType::LBRACE:
            return "{";
        case TokenType::RBRACE:
            return "}";
        case TokenType::ARROW:
            return "->";
        case TokenType::COLON:
            return ":";
        case TokenType::SEMICOLON:
            return ";";
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

TokenType Lexer::check_keyword(const std::string& word) const {
    static const std::unordered_map<std::string, TokenType> keywords = {
        {"fun", TokenType::KW_FUN},
        {"var", TokenType::KW_VAR},
        {"return", TokenType::KW_RETURN},
        {"int", TokenType::KW_INT}};

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

    std::string num(source_.substr(start, pos_ - start));
    return Token(TokenType::INT_LITERAL, num, line_, start_col);
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
    if (c == ':') {
        advance();
        return make_token(TokenType::COLON);
    }
    if (c == ';') {
        advance();
        return make_token(TokenType::SEMICOLON);
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

    if (c == '=') {
        advance();
        return make_token(TokenType::ASSIGN);
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
