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

enum class TypeKind { VOID, CHAR, I8, I16, I32, I64, F32, BOOL, POINTER, ARRAY };

struct Type {
    TypeKind kind;
    std::unique_ptr<Type> element_type;  // For pointers and arrays
    size_t array_size;                   // For arrays only

    Type() : kind(TypeKind::I64), element_type(nullptr), array_size(0) {}
    explicit Type(TypeKind k) : kind(k), element_type(nullptr), array_size(0) {}

    // Pointer type constructor
    static Type make_pointer(Type elem) {
        Type t(TypeKind::POINTER);
        t.element_type = std::make_unique<Type>(std::move(elem));
        return t;
    }

    // Array type constructor
    static Type make_array(Type elem, size_t size) {
        Type t(TypeKind::ARRAY);
        t.element_type = std::make_unique<Type>(std::move(elem));
        t.array_size = size;
        return t;
    }

    // Copy constructor
    Type(const Type& other) : kind(other.kind), array_size(other.array_size) {
        if (other.element_type) {
            element_type = std::make_unique<Type>(*other.element_type);
        }
    }

    // Move constructor
    Type(Type&& other) noexcept = default;

    // Copy assignment
    Type& operator=(const Type& other) {
        if (this != &other) {
            kind = other.kind;
            array_size = other.array_size;
            if (other.element_type) {
                element_type = std::make_unique<Type>(*other.element_type);
            } else {
                element_type.reset();
            }
        }
        return *this;
    }

    // Move assignment
    Type& operator=(Type&& other) noexcept = default;

    size_t size() const {
        switch (kind) {
            case TypeKind::VOID:
                return 0;
            case TypeKind::CHAR:
            case TypeKind::I8:
            case TypeKind::BOOL:
                return 1;
            case TypeKind::I16:
                return 2;
            case TypeKind::I32:
            case TypeKind::F32:
                return 4;
            case TypeKind::I64:
            case TypeKind::POINTER:
                return 8;
            case TypeKind::ARRAY:
                if (element_type) {
                    return element_type->size() * array_size;
                }
                return 0;
        }
        return 8;
    }

    bool equals(const Type& other) const {
        if (kind != other.kind)
            return false;
        if (kind == TypeKind::ARRAY) {
            if (array_size != other.array_size)
                return false;
        }
        if (element_type && other.element_type) {
            return element_type->equals(*other.element_type);
        }
        return !element_type && !other.element_type;
    }
};

enum class BinaryOp { ADD, SUB, MUL, DIV, MOD, EQ, NE, LT, GT, LE, GE, AND, OR };

enum class UnaryOp { NEG, NOT, POS, DEREF, ADDR };

struct Parameter {
    std::string name;
    Type type;

    Parameter(std::string n, Type t) : name(std::move(n)), type(std::move(t)) {}
};

struct Expression {
    virtual ~Expression() = default;
};

struct IntLiteral : Expression {
    int64_t value;
    explicit IntLiteral(int64_t v) : value(v) {}
};

struct FloatLiteral : Expression {
    float value;
    explicit FloatLiteral(float v) : value(v) {}
};

struct CharLiteral : Expression {
    char value;
    explicit CharLiteral(char v) : value(v) {}
};

struct StringLiteral : Expression {
    std::string value;
    explicit StringLiteral(std::string v) : value(std::move(v)) {}
};

struct BoolLiteral : Expression {
    bool value;
    explicit BoolLiteral(bool v) : value(v) {}
};

struct ArrayLiteral : Expression {
    std::vector<std::unique_ptr<Expression>> elements;
    explicit ArrayLiteral(std::vector<std::unique_ptr<Expression>> elems)
        : elements(std::move(elems)) {}
};

struct SizeofExpr : Expression {
    Type target_type;
    explicit SizeofExpr(Type t) : target_type(std::move(t)) {}
};

struct Identifier : Expression {
    std::string name;
    explicit Identifier(std::string n) : name(std::move(n)) {}
};

struct CallExpr : Expression {
    std::string callee;
    std::vector<std::unique_ptr<Expression>> args;
    explicit CallExpr(std::string name) : callee(std::move(name)) {}
};

struct IndexExpr : Expression {
    std::unique_ptr<Expression> array;
    std::unique_ptr<Expression> index;
    IndexExpr(std::unique_ptr<Expression> arr, std::unique_ptr<Expression> idx)
        : array(std::move(arr)), index(std::move(idx)) {}
};

struct UnaryExpr : Expression {
    UnaryOp op;
    std::unique_ptr<Expression> operand;
    UnaryExpr(UnaryOp o, std::unique_ptr<Expression> e) : op(o), operand(std::move(e)) {}
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
    ConstDecl(std::string n, Type t, int64_t v)
        : name(std::move(n)), type(std::move(t)), value(v) {}
};

struct VarDecl : Statement {
    std::string name;
    Type type;
    std::unique_ptr<Expression> initializer;
    VarDecl(std::string n, Type t, std::unique_ptr<Expression> init)
        : name(std::move(n)), type(std::move(t)), initializer(std::move(init)) {}
};

struct AssignStmt : Statement {
    std::string name;
    std::unique_ptr<Expression> value;
    AssignStmt(std::string n, std::unique_ptr<Expression> v)
        : name(std::move(n)), value(std::move(v)) {}
};

struct IndexAssignStmt : Statement {
    std::unique_ptr<Expression> target;
    std::unique_ptr<Expression> value;
    IndexAssignStmt(std::unique_ptr<Expression> tgt, std::unique_ptr<Expression> val)
        : target(std::move(tgt)), value(std::move(val)) {}
};

struct ReturnStmt : Statement {
    std::unique_ptr<Expression> value;
    explicit ReturnStmt(std::unique_ptr<Expression> v) : value(std::move(v)) {}
};

struct IfStmt : Statement {
    std::unique_ptr<Expression> condition;
    std::vector<std::unique_ptr<Statement>> then_body;
    std::vector<std::unique_ptr<Statement>> else_body;
    IfStmt(std::unique_ptr<Expression> cond, std::vector<std::unique_ptr<Statement>> tb,
           std::vector<std::unique_ptr<Statement>> eb)
        : condition(std::move(cond)), then_body(std::move(tb)), else_body(std::move(eb)) {}
};

struct Function {
    std::string name;
    std::vector<Parameter> params;
    Type return_type;
    std::vector<std::unique_ptr<Statement>> body;
    Function(std::string n, std::vector<Parameter> p, Type ret)
        : name(std::move(n)), params(std::move(p)), return_type(std::move(ret)) {}
};

struct Program {
    std::vector<std::unique_ptr<Function>> functions;
};

}  // namespace zex

#endif  // ZEX_AST_HPP
