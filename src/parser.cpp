#include "zex/parser.hpp"

namespace zex {

Parser::Parser(std::vector<Token> tokens) : tokens_(std::move(tokens)), current_(0) {}

const Token& Parser::peek() const {
    return tokens_[current_];
}

const Token& Parser::previous() const {
    return tokens_[current_ - 1];
}

bool Parser::at_end() const {
    return peek().type == TokenType::END_OF_FILE;
}

const Token& Parser::advance() {
    if (!at_end())
        current_++;
    return previous();
}

bool Parser::check(TokenType type) const {
    if (at_end())
        return false;
    return peek().type == type;
}

bool Parser::match(TokenType type) {
    if (check(type)) {
        advance();
        return true;
    }
    return false;
}

CompileError Parser::error(ErrorCode code) {
    const Token& tok = peek();
    return CompileError(code, {tok.line, tok.column}, tok.value);
}

CompileError Parser::error(ErrorCode code, const std::string& ctx) {
    const Token& tok = peek();
    return CompileError(code, {tok.line, tok.column}, ctx);
}

void Parser::expect(TokenType type, ErrorCode err_code) {
    if (!match(type)) {
        throw error(err_code);
    }
}

std::unique_ptr<Program> Parser::parse() {
    auto program = std::make_unique<Program>();
    while (!at_end()) {
        program->functions.push_back(parse_function());
    }
    return program;
}

std::unique_ptr<Function> Parser::parse_function() {
    expect(TokenType::KW_FUN, ErrorCode::UNEXPECTED_TOKEN);

    expect(TokenType::IDENTIFIER, ErrorCode::EXPECTED_IDENTIFIER);
    std::string name = previous().value;

    expect(TokenType::LPAREN, ErrorCode::UNEXPECTED_TOKEN);
    expect(TokenType::RPAREN, ErrorCode::UNEXPECTED_TOKEN);
    expect(TokenType::ARROW, ErrorCode::UNEXPECTED_TOKEN);
    Type return_type = parse_type();
    expect(TokenType::LBRACE, ErrorCode::UNEXPECTED_TOKEN);

    auto func = std::make_unique<Function>(name, return_type);
    while (!check(TokenType::RBRACE) && !at_end()) {
        func->body.push_back(parse_statement());
    }

    expect(TokenType::RBRACE, ErrorCode::UNEXPECTED_TOKEN);
    return func;
}

std::unique_ptr<Statement> Parser::parse_statement() {
    if (check(TokenType::KW_VAR)) {
        return parse_var_decl();
    }
    if (check(TokenType::KW_RETURN)) {
        return parse_return();
    }
    if (check(TokenType::IDENTIFIER)) {
        return parse_assign_or_expr_stmt();
    }
    throw error(ErrorCode::EXPECTED_STATEMENT);
}

std::unique_ptr<Statement> Parser::parse_assign_or_expr_stmt() {
    expect(TokenType::IDENTIFIER, ErrorCode::EXPECTED_IDENTIFIER);
    std::string name = previous().value;

    expect(TokenType::ASSIGN, ErrorCode::UNEXPECTED_TOKEN);
    auto value = parse_expression();
    expect(TokenType::SEMICOLON, ErrorCode::UNEXPECTED_TOKEN);

    return std::make_unique<AssignStmt>(name, std::move(value));
}

std::unique_ptr<VarDecl> Parser::parse_var_decl() {
    expect(TokenType::KW_VAR, ErrorCode::UNEXPECTED_TOKEN);
    expect(TokenType::IDENTIFIER, ErrorCode::EXPECTED_IDENTIFIER);
    std::string name = previous().value;

    expect(TokenType::COLON, ErrorCode::UNEXPECTED_TOKEN);
    Type type = parse_type();
    expect(TokenType::ASSIGN, ErrorCode::UNEXPECTED_TOKEN);
    auto init = parse_expression();
    expect(TokenType::SEMICOLON, ErrorCode::UNEXPECTED_TOKEN);

    return std::make_unique<VarDecl>(name, type, std::move(init));
}

std::unique_ptr<ReturnStmt> Parser::parse_return() {
    expect(TokenType::KW_RETURN, ErrorCode::UNEXPECTED_TOKEN);
    auto value = parse_expression();
    expect(TokenType::SEMICOLON, ErrorCode::UNEXPECTED_TOKEN);
    return std::make_unique<ReturnStmt>(std::move(value));
}

std::unique_ptr<Expression> Parser::parse_expression() {
    return parse_additive();
}

std::unique_ptr<Expression> Parser::parse_additive() {
    auto left = parse_multiplicative();

    while (check(TokenType::PLUS) || check(TokenType::MINUS)) {
        BinaryOp op = check(TokenType::PLUS) ? BinaryOp::ADD : BinaryOp::SUB;
        advance();
        auto right = parse_multiplicative();
        left = std::make_unique<BinaryExpr>(op, std::move(left), std::move(right));
    }

    return left;
}

std::unique_ptr<Expression> Parser::parse_multiplicative() {
    auto left = parse_primary();

    while (check(TokenType::STAR) || check(TokenType::SLASH) || check(TokenType::PERCENT)) {
        BinaryOp op;
        if (check(TokenType::STAR)) {
            op = BinaryOp::MUL;
        } else if (check(TokenType::SLASH)) {
            op = BinaryOp::DIV;
        } else {
            op = BinaryOp::MOD;
        }
        advance();
        auto right = parse_primary();
        left = std::make_unique<BinaryExpr>(op, std::move(left), std::move(right));
    }

    return left;
}

std::unique_ptr<Expression> Parser::parse_primary() {
    if (match(TokenType::INT_LITERAL)) {
        int64_t value = std::stoll(previous().value);
        return std::make_unique<IntLiteral>(value);
    }

    if (match(TokenType::IDENTIFIER)) {
        std::string name = previous().value;
        if (match(TokenType::LPAREN)) {
            expect(TokenType::RPAREN, ErrorCode::UNEXPECTED_TOKEN);
            return std::make_unique<CallExpr>(name);
        }
        return std::make_unique<Identifier>(name);
    }

    if (match(TokenType::LPAREN)) {
        auto expr = parse_expression();
        expect(TokenType::RPAREN, ErrorCode::UNEXPECTED_TOKEN);
        return expr;
    }

    throw error(ErrorCode::EXPECTED_EXPRESSION);
}

Type Parser::parse_type() {
    if (match(TokenType::KW_INT)) {
        return Type(TypeKind::INT);
    }
    throw error(ErrorCode::EXPECTED_TYPE);
}

}  // namespace zex
