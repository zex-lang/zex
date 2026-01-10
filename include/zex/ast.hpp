#ifndef ZEX_AST_HPP
#define ZEX_AST_HPP

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

namespace zex {

// Forward declarations
struct Expression;
struct Statement;
struct Function;
struct Program;

// Type representation (only int for now)
enum class TypeKind { INT };

struct Type {
    TypeKind kind;

    Type() : kind(TypeKind::INT) {}
    explicit Type(TypeKind k) : kind(k) {}
};

// Base expression node
struct Expression {
    virtual ~Expression() = default;
};

// Integer literal: 42
struct IntLiteral : Expression {
    int64_t value;

    explicit IntLiteral(int64_t v) : value(v) {}
};

// Identifier: x
struct Identifier : Expression {
    std::string name;

    explicit Identifier(std::string n) : name(std::move(n)) {}
};

// Function call: foo()
struct CallExpr : Expression {
    std::string callee;

    explicit CallExpr(std::string name) : callee(std::move(name)) {}
};

// Base statement node
struct Statement {
    virtual ~Statement() = default;
};

// Variable declaration: var x: int = expr;
struct VarDecl : Statement {
    std::string name;
    Type type;
    std::unique_ptr<Expression> initializer;

    VarDecl(std::string n, Type t, std::unique_ptr<Expression> init)
        : name(std::move(n)), type(t), initializer(std::move(init)) {}
};

// Return statement: return expr;
struct ReturnStmt : Statement {
    std::unique_ptr<Expression> value;

    explicit ReturnStmt(std::unique_ptr<Expression> v) : value(std::move(v)) {}
};

// Function definition
struct Function {
    std::string name;
    Type return_type;
    std::vector<std::unique_ptr<Statement>> body;

    Function(std::string n, Type ret) : name(std::move(n)), return_type(ret) {}
};

// Program: collection of functions
struct Program {
    std::vector<std::unique_ptr<Function>> functions;
};

}  // namespace zex

#endif  // ZEX_AST_HPP
