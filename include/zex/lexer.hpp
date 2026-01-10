#ifndef ZEX_LEXER_HPP
#define ZEX_LEXER_HPP

#include <string>
#include <string_view>
#include <vector>

#include "zex/token.hpp"

namespace zex {

class Lexer {
   public:
    explicit Lexer(std::string_view source);

    // Tokenize entire source, returns all tokens including EOF
    std::vector<Token> tokenize();

   private:
    std::string_view source_;
    size_t pos_;
    uint32_t line_;
    uint32_t column_;

    // Character access
    char current() const;
    char peek(size_t offset = 1) const;
    void advance();
    bool at_end() const;

    // Token scanning
    Token scan_token();
    Token make_token(TokenType type);
    Token make_token(TokenType type, const std::string& value);

    // Specific token types
    Token scan_identifier();
    Token scan_number();

    // Utilities
    void skip_whitespace();
    bool is_alpha(char c) const;
    bool is_digit(char c) const;
    bool is_alnum(char c) const;

    // Keyword lookup
    TokenType check_keyword(const std::string& word) const;
};

}  // namespace zex

#endif  // ZEX_LEXER_HPP
