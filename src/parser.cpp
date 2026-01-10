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
    auto params = parse_parameters();
    expect(TokenType::RPAREN, ErrorCode::UNEXPECTED_TOKEN);
    expect(TokenType::ARROW, ErrorCode::UNEXPECTED_TOKEN);
    Type return_type = parse_type();
    expect(TokenType::LBRACE, ErrorCode::UNEXPECTED_TOKEN);

    auto func = std::make_unique<Function>(name, std::move(params), std::move(return_type));
    while (!check(TokenType::RBRACE) && !at_end()) {
        func->body.push_back(parse_statement());
    }

    expect(TokenType::RBRACE, ErrorCode::UNEXPECTED_TOKEN);
    return func;
}

std::vector<Parameter> Parser::parse_parameters() {
    std::vector<Parameter> params;
    if (check(TokenType::RPAREN)) {
        return params;
    }

    do {
        expect(TokenType::IDENTIFIER, ErrorCode::EXPECTED_IDENTIFIER);
        std::string name = previous().value;
        expect(TokenType::COLON, ErrorCode::UNEXPECTED_TOKEN);
        Type type = parse_type();
        params.emplace_back(name, std::move(type));
    } while (match(TokenType::COMMA));

    return params;
}

std::unique_ptr<Statement> Parser::parse_statement() {
    if (check(TokenType::KW_VAR)) {
        return parse_var_decl();
    }
    if (check(TokenType::KW_CONST)) {
        return parse_const_decl();
    }
    if (check(TokenType::KW_RETURN)) {
        return parse_return();
    }
    if (check(TokenType::KW_IF)) {
        return parse_if_stmt();
    }
    if (check(TokenType::IDENTIFIER)) {
        return parse_assign_or_expr_stmt();
    }
    throw error(ErrorCode::EXPECTED_STATEMENT);
}

std::vector<std::unique_ptr<Statement>> Parser::parse_block() {
    expect(TokenType::LBRACE, ErrorCode::UNEXPECTED_TOKEN);
    std::vector<std::unique_ptr<Statement>> stmts;
    while (!check(TokenType::RBRACE) && !at_end()) {
        stmts.push_back(parse_statement());
    }
    expect(TokenType::RBRACE, ErrorCode::UNEXPECTED_TOKEN);
    return stmts;
}

std::unique_ptr<IfStmt> Parser::parse_if_stmt() {
    expect(TokenType::KW_IF, ErrorCode::UNEXPECTED_TOKEN);
    auto condition = parse_expression();
    auto then_body = parse_block();

    std::vector<std::unique_ptr<Statement>> else_body;
    if (match(TokenType::KW_ELSE)) {
        if (check(TokenType::KW_IF)) {
            else_body.push_back(parse_if_stmt());
        } else {
            else_body = parse_block();
        }
    }

    return std::make_unique<IfStmt>(std::move(condition), std::move(then_body),
                                    std::move(else_body));
}

std::unique_ptr<Statement> Parser::parse_assign_or_expr_stmt() {
    auto expr = parse_expression();

    // Check for index assignment: arr[0] = value
    if (auto* idx = dynamic_cast<IndexExpr*>(expr.get())) {
        if (match(TokenType::ASSIGN)) {
            auto value = parse_expression();
            expect(TokenType::SEMICOLON, ErrorCode::UNEXPECTED_TOKEN);
            // Clone the index expression for assignment
            auto target =
                std::make_unique<IndexExpr>(std::move(const_cast<IndexExpr*>(idx)->array),
                                            std::move(const_cast<IndexExpr*>(idx)->index));
            return std::make_unique<IndexAssignStmt>(std::move(target), std::move(value));
        }
    }

    // Check for regular assignment: x = value
    if (auto* ident = dynamic_cast<Identifier*>(expr.get())) {
        std::string name = ident->name;

        if (match(TokenType::ASSIGN)) {
            auto value = parse_expression();
            expect(TokenType::SEMICOLON, ErrorCode::UNEXPECTED_TOKEN);
            return std::make_unique<AssignStmt>(name, std::move(value));
        }

        BinaryOp op;
        bool is_compound = true;
        if (match(TokenType::PLUS_ASSIGN)) {
            op = BinaryOp::ADD;
        } else if (match(TokenType::MINUS_ASSIGN)) {
            op = BinaryOp::SUB;
        } else if (match(TokenType::STAR_ASSIGN)) {
            op = BinaryOp::MUL;
        } else if (match(TokenType::SLASH_ASSIGN)) {
            op = BinaryOp::DIV;
        } else if (match(TokenType::PERCENT_ASSIGN)) {
            op = BinaryOp::MOD;
        } else {
            is_compound = false;
        }

        if (is_compound) {
            auto rhs = parse_expression();
            expect(TokenType::SEMICOLON, ErrorCode::UNEXPECTED_TOKEN);
            auto lhs = std::make_unique<Identifier>(name);
            auto binary = std::make_unique<BinaryExpr>(op, std::move(lhs), std::move(rhs));
            return std::make_unique<AssignStmt>(name, std::move(binary));
        }
    }

    throw error(ErrorCode::UNEXPECTED_TOKEN);
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

    return std::make_unique<VarDecl>(name, std::move(type), std::move(init));
}

std::unique_ptr<ReturnStmt> Parser::parse_return() {
    expect(TokenType::KW_RETURN, ErrorCode::UNEXPECTED_TOKEN);

    // Allow void return
    if (match(TokenType::SEMICOLON)) {
        return std::make_unique<ReturnStmt>(nullptr);
    }

    auto value = parse_expression();
    expect(TokenType::SEMICOLON, ErrorCode::UNEXPECTED_TOKEN);
    return std::make_unique<ReturnStmt>(std::move(value));
}

std::unique_ptr<Expression> Parser::parse_expression() {
    return parse_or();
}

std::unique_ptr<Expression> Parser::parse_or() {
    auto left = parse_and();
    while (match(TokenType::OR)) {
        auto right = parse_and();
        left = std::make_unique<BinaryExpr>(BinaryOp::OR, std::move(left), std::move(right));
    }
    return left;
}

std::unique_ptr<Expression> Parser::parse_and() {
    auto left = parse_equality();
    while (match(TokenType::AND)) {
        auto right = parse_equality();
        left = std::make_unique<BinaryExpr>(BinaryOp::AND, std::move(left), std::move(right));
    }
    return left;
}

std::unique_ptr<Expression> Parser::parse_equality() {
    auto left = parse_comparison();
    while (check(TokenType::EQ) || check(TokenType::NE)) {
        BinaryOp op = check(TokenType::EQ) ? BinaryOp::EQ : BinaryOp::NE;
        advance();
        auto right = parse_comparison();
        left = std::make_unique<BinaryExpr>(op, std::move(left), std::move(right));
    }
    return left;
}

std::unique_ptr<Expression> Parser::parse_comparison() {
    auto left = parse_additive();
    while (check(TokenType::LT) || check(TokenType::GT) || check(TokenType::LE) ||
           check(TokenType::GE)) {
        BinaryOp op;
        if (check(TokenType::LT))
            op = BinaryOp::LT;
        else if (check(TokenType::GT))
            op = BinaryOp::GT;
        else if (check(TokenType::LE))
            op = BinaryOp::LE;
        else
            op = BinaryOp::GE;
        advance();
        auto right = parse_additive();
        left = std::make_unique<BinaryExpr>(op, std::move(left), std::move(right));
    }
    return left;
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
    auto left = parse_unary();
    while (check(TokenType::STAR) || check(TokenType::SLASH) || check(TokenType::PERCENT)) {
        BinaryOp op;
        if (check(TokenType::STAR))
            op = BinaryOp::MUL;
        else if (check(TokenType::SLASH))
            op = BinaryOp::DIV;
        else
            op = BinaryOp::MOD;
        advance();
        auto right = parse_unary();
        left = std::make_unique<BinaryExpr>(op, std::move(left), std::move(right));
    }
    return left;
}

std::unique_ptr<Expression> Parser::parse_unary() {
    if (match(TokenType::MINUS)) {
        auto operand = parse_unary();
        return std::make_unique<UnaryExpr>(UnaryOp::NEG, std::move(operand));
    }
    if (match(TokenType::PLUS)) {
        auto operand = parse_unary();
        return std::make_unique<UnaryExpr>(UnaryOp::POS, std::move(operand));
    }
    if (match(TokenType::BANG)) {
        auto operand = parse_unary();
        return std::make_unique<UnaryExpr>(UnaryOp::NOT, std::move(operand));
    }
    if (match(TokenType::AMPERSAND)) {
        auto operand = parse_unary();
        return std::make_unique<UnaryExpr>(UnaryOp::ADDR, std::move(operand));
    }
    if (match(TokenType::STAR)) {
        auto operand = parse_unary();
        return std::make_unique<UnaryExpr>(UnaryOp::DEREF, std::move(operand));
    }
    return parse_postfix();
}

std::unique_ptr<Expression> Parser::parse_postfix() {
    auto expr = parse_primary();

    while (true) {
        if (match(TokenType::LBRACKET)) {
            auto index = parse_expression();
            expect(TokenType::RBRACKET, ErrorCode::UNEXPECTED_TOKEN);
            expr = std::make_unique<IndexExpr>(std::move(expr), std::move(index));
        } else {
            break;
        }
    }

    return expr;
}

std::unique_ptr<Expression> Parser::parse_primary() {
    if (match(TokenType::INT_LITERAL)) {
        int64_t value = std::stoll(previous().value);
        return std::make_unique<IntLiteral>(value);
    }

    if (match(TokenType::FLOAT_LITERAL)) {
        float value = std::stof(previous().value);
        return std::make_unique<FloatLiteral>(value);
    }

    if (match(TokenType::CHAR_LITERAL)) {
        char value = previous().value[0];
        return std::make_unique<CharLiteral>(value);
    }

    if (match(TokenType::STRING_LITERAL)) {
        std::string value = previous().value;
        return std::make_unique<StringLiteral>(value);
    }

    if (match(TokenType::KW_TRUE)) {
        return std::make_unique<BoolLiteral>(true);
    }

    if (match(TokenType::KW_FALSE)) {
        return std::make_unique<BoolLiteral>(false);
    }

    if (match(TokenType::KW_SIZEOF)) {
        expect(TokenType::LPAREN, ErrorCode::UNEXPECTED_TOKEN);
        Type t = parse_type();
        expect(TokenType::RPAREN, ErrorCode::UNEXPECTED_TOKEN);
        return std::make_unique<SizeofExpr>(std::move(t));
    }

    if (match(TokenType::LBRACKET)) {
        std::vector<std::unique_ptr<Expression>> elements;
        if (!check(TokenType::RBRACKET)) {
            do {
                elements.push_back(parse_expression());
            } while (match(TokenType::COMMA));
        }
        expect(TokenType::RBRACKET, ErrorCode::UNEXPECTED_TOKEN);
        return std::make_unique<ArrayLiteral>(std::move(elements));
    }

    if (match(TokenType::IDENTIFIER)) {
        std::string name = previous().value;
        if (match(TokenType::LPAREN)) {
            auto call = std::make_unique<CallExpr>(name);
            call->args = parse_arguments();
            expect(TokenType::RPAREN, ErrorCode::UNEXPECTED_TOKEN);
            return call;
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
    // Pointer type: *type
    if (match(TokenType::STAR)) {
        Type elem = parse_type();
        return Type::make_pointer(std::move(elem));
    }

    // Array type: [type; size]
    if (match(TokenType::LBRACKET)) {
        Type elem = parse_type();
        expect(TokenType::SEMICOLON, ErrorCode::UNEXPECTED_TOKEN);
        expect(TokenType::INT_LITERAL, ErrorCode::UNEXPECTED_TOKEN);
        size_t size = std::stoull(previous().value);
        expect(TokenType::RBRACKET, ErrorCode::UNEXPECTED_TOKEN);
        return Type::make_array(std::move(elem), size);
    }

    // Base types
    if (match(TokenType::KW_VOID)) {
        return Type(TypeKind::VOID);
    }
    if (match(TokenType::KW_CHAR)) {
        return Type(TypeKind::CHAR);
    }
    if (match(TokenType::KW_I8)) {
        return Type(TypeKind::I8);
    }
    if (match(TokenType::KW_I16)) {
        return Type(TypeKind::I16);
    }
    if (match(TokenType::KW_I32)) {
        return Type(TypeKind::I32);
    }
    if (match(TokenType::KW_I64)) {
        return Type(TypeKind::I64);
    }
    if (match(TokenType::KW_F32)) {
        return Type(TypeKind::F32);
    }
    if (match(TokenType::KW_BOOL)) {
        return Type(TypeKind::BOOL);
    }
    throw error(ErrorCode::EXPECTED_TYPE);
}

std::unique_ptr<ConstDecl> Parser::parse_const_decl() {
    expect(TokenType::KW_CONST, ErrorCode::UNEXPECTED_TOKEN);
    expect(TokenType::IDENTIFIER, ErrorCode::EXPECTED_IDENTIFIER);
    std::string name = previous().value;

    expect(TokenType::COLON, ErrorCode::UNEXPECTED_TOKEN);
    Type type = parse_type();
    expect(TokenType::ASSIGN, ErrorCode::UNEXPECTED_TOKEN);
    auto expr = parse_expression();
    expect(TokenType::SEMICOLON, ErrorCode::UNEXPECTED_TOKEN);

    int64_t value = eval_const_expr(expr.get());
    return std::make_unique<ConstDecl>(name, std::move(type), value);
}

int64_t Parser::eval_const_expr(Expression* expr) {
    if (auto* lit = dynamic_cast<IntLiteral*>(expr)) {
        return lit->value;
    }

    if (auto* bl = dynamic_cast<BoolLiteral*>(expr)) {
        return bl->value ? 1 : 0;
    }

    if (auto* cl = dynamic_cast<CharLiteral*>(expr)) {
        return static_cast<int64_t>(cl->value);
    }

    if (auto* sz = dynamic_cast<SizeofExpr*>(expr)) {
        return static_cast<int64_t>(sz->target_type.size());
    }

    if (auto* unary = dynamic_cast<UnaryExpr*>(expr)) {
        int64_t val = eval_const_expr(unary->operand.get());
        switch (unary->op) {
            case UnaryOp::NEG:
                return -val;
            case UnaryOp::POS:
                return val;
            case UnaryOp::NOT:
                return val == 0 ? 1 : 0;
            default:
                break;
        }
    }

    if (auto* binary = dynamic_cast<BinaryExpr*>(expr)) {
        int64_t left = eval_const_expr(binary->left.get());
        int64_t right = eval_const_expr(binary->right.get());
        switch (binary->op) {
            case BinaryOp::ADD:
                return left + right;
            case BinaryOp::SUB:
                return left - right;
            case BinaryOp::MUL:
                return left * right;
            case BinaryOp::DIV:
                return left / right;
            case BinaryOp::MOD:
                return left % right;
            case BinaryOp::EQ:
                return left == right ? 1 : 0;
            case BinaryOp::NE:
                return left != right ? 1 : 0;
            case BinaryOp::LT:
                return left < right ? 1 : 0;
            case BinaryOp::GT:
                return left > right ? 1 : 0;
            case BinaryOp::LE:
                return left <= right ? 1 : 0;
            case BinaryOp::GE:
                return left >= right ? 1 : 0;
            case BinaryOp::AND:
                return (left != 0 && right != 0) ? 1 : 0;
            case BinaryOp::OR:
                return (left != 0 || right != 0) ? 1 : 0;
        }
    }

    throw error(ErrorCode::EXPECTED_EXPRESSION);
}

std::vector<std::unique_ptr<Expression>> Parser::parse_arguments() {
    std::vector<std::unique_ptr<Expression>> args;
    if (check(TokenType::RPAREN)) {
        return args;
    }

    do {
        args.push_back(parse_expression());
    } while (match(TokenType::COMMA));

    return args;
}

}  // namespace zex
