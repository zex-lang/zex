// Zex parser
// Converts token stream into abstract syntax tree

#ifndef ZEX_PARSER_HPP
#define ZEX_PARSER_HPP

#include <memory>
#include <vector>

#include "zex/ast.hpp"
#include "zex/error.hpp"
#include "zex/token.hpp"

namespace zex {

// Recursive descent parser that builds AST from tokens
class Parser {
   public:
    explicit Parser(std::vector<Token> tokens);

    // Parse and return complete program AST
    std::unique_ptr<Program> parse();

   private:
    std::vector<Token> tokens_;
    size_t current_;

    // Token navigation
    const Token& peek() const;
    const Token& previous() const;
    bool at_end() const;
    const Token& advance();
    bool check(TokenType type) const;
    bool match(TokenType type);

    // Error handling
    CompileError error(ErrorCode code);
    CompileError error(ErrorCode code, const std::string& ctx);
    void expect(TokenType type, ErrorCode err_code);

    // Statement parsing
    std::unique_ptr<Function> parse_function();
    std::vector<Parameter> parse_parameters();
    std::unique_ptr<Statement> parse_statement();
    std::unique_ptr<Statement> parse_assign_or_expr_stmt();
    std::unique_ptr<VarDecl> parse_var_decl();
    std::unique_ptr<ConstDecl> parse_const_decl();
    std::unique_ptr<ReturnStmt> parse_return();
    std::unique_ptr<IfStmt> parse_if_stmt();
    std::unique_ptr<AsmBlock> parse_asm_block();
    std::vector<std::unique_ptr<Statement>> parse_block();

    // Expression parsing with precedence climbing
    std::unique_ptr<Expression> parse_expression();
    std::unique_ptr<Expression> parse_or();
    std::unique_ptr<Expression> parse_and();
    std::unique_ptr<Expression> parse_equality();
    std::unique_ptr<Expression> parse_comparison();
    std::unique_ptr<Expression> parse_additive();
    std::unique_ptr<Expression> parse_multiplicative();
    std::unique_ptr<Expression> parse_unary();
    std::unique_ptr<Expression> parse_postfix();
    std::unique_ptr<Expression> parse_primary();
    std::vector<std::unique_ptr<Expression>> parse_arguments();

    // Constant expression evaluation at compile time
    int64_t eval_const_expr(Expression* expr);

    // Type parsing
    Type parse_type();
};

}  // namespace zex

#endif  // ZEX_PARSER_HPP
