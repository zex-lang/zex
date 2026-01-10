#ifndef ZEX_AST_HPP
#define ZEX_AST_HPP

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace zex {

struct Expression;
struct Statement;
struct Function;
struct Program;

enum class TypeKind { INT };

struct Type {
    TypeKind kind;

    Type() : kind(TypeKind::INT) {}
    explicit Type(TypeKind k) : kind(k) {}
};

enum class BinaryOp { ADD, SUB, MUL, DIV, MOD };

struct Expression {
    virtual ~Expression() = default;
};

struct IntLiteral : Expression {
    int64_t value;

    explicit IntLiteral(int64_t v) : value(v) {}
};

struct Identifier : Expression {
    std::string name;

    explicit Identifier(std::string n) : name(std::move(n)) {}
};

struct CallExpr : Expression {
    std::string callee;

    explicit CallExpr(std::string name) : callee(std::move(name)) {}
};

struct BinaryExpr : Expression {
    BinaryOp op;
    std::unique_ptr<Expression> left;
    std::unique_ptr<Expression> right;

    BinaryExpr(BinaryOp o, std::unique_ptr<Expression> l, std::unique_ptr<Expression> r)
        : op(o), left(std::move(l)), right(std::move(r)) {}
};

struct Statement {
    virtual ~Statement() = default;
};

struct ConstDecl : Statement {
    std::string name;
    Type type;
    int64_t value;

    ConstDecl(std::string n, Type t, int64_t v) : name(std::move(n)), type(t), value(v) {}
};

struct VarDecl : Statement {
    std::string name;
    Type type;
    std::unique_ptr<Expression> initializer;

    VarDecl(std::string n, Type t, std::unique_ptr<Expression> init)
        : name(std::move(n)), type(t), initializer(std::move(init)) {}
};

struct AssignStmt : Statement {
    std::string name;
    std::unique_ptr<Expression> value;

    AssignStmt(std::string n, std::unique_ptr<Expression> v)
        : name(std::move(n)), value(std::move(v)) {}
};

struct ReturnStmt : Statement {
    std::unique_ptr<Expression> value;

    explicit ReturnStmt(std::unique_ptr<Expression> v) : value(std::move(v)) {}
};

struct Function {
    std::string name;
    Type return_type;
    std::vector<std::unique_ptr<Statement>> body;

    Function(std::string n, Type ret) : name(std::move(n)), return_type(ret) {}
};

struct Program {
    std::vector<std::unique_ptr<Function>> functions;
};

}  // namespace zex

#endif  // ZEX_AST_HPP
