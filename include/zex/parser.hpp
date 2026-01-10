#ifndef ZEX_PARSER_HPP
#define ZEX_PARSER_HPP

#include <memory>
#include <vector>

#include "zex/ast.hpp"
#include "zex/error.hpp"
#include "zex/token.hpp"

namespace zex {

class Parser {
   public:
    explicit Parser(std::vector<Token> tokens);
    std::unique_ptr<Program> parse();

   private:
    std::vector<Token> tokens_;
    size_t current_;

    const Token& peek() const;
    const Token& previous() const;
    bool at_end() const;
    const Token& advance();
    bool check(TokenType type) const;
    bool match(TokenType type);

    CompileError error(ErrorCode code);
    CompileError error(ErrorCode code, const std::string& ctx);
    void expect(TokenType type, ErrorCode err_code);

    std::unique_ptr<Function> parse_function();
    std::unique_ptr<Statement> parse_statement();
    std::unique_ptr<VarDecl> parse_var_decl();
    std::unique_ptr<ReturnStmt> parse_return();
    std::unique_ptr<Expression> parse_expression();
    Type parse_type();
};

}  // namespace zex

#endif  // ZEX_PARSER_HPP
