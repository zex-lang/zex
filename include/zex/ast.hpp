// Zex Abstract Syntax Tree definitions
// Represents the parsed program structure

#ifndef ZEX_AST_HPP
#define ZEX_AST_HPP

#include <cstdint>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace zex {

struct Expression;
struct Statement;
struct Function;
struct Program;

// All supported type kinds in the language
enum class TypeKind { VOID, CHAR, I8, I16, I32, I64, F32, BOOL, POINTER, ARRAY };

// Type representation with support for composite types like arrays and pointers
struct Type {
    TypeKind kind;
    std::unique_ptr<Type> element_type;
    size_t array_size;

    Type() : kind(TypeKind::I64), element_type(nullptr), array_size(0) {}
    explicit Type(TypeKind k) : kind(k), element_type(nullptr), array_size(0) {}

    static Type make_pointer(Type elem) {
        Type t(TypeKind::POINTER);
        t.element_type = std::make_unique<Type>(std::move(elem));
        return t;
    }

    static Type make_array(Type elem, size_t size) {
        Type t(TypeKind::ARRAY);
        t.element_type = std::make_unique<Type>(std::move(elem));
        t.array_size = size;
        return t;
    }

    Type(const Type& other) : kind(other.kind), array_size(other.array_size) {
        if (other.element_type) {
            element_type = std::make_unique<Type>(*other.element_type);
        }
    }

    Type(Type&& other) noexcept = default;

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

    Type& operator=(Type&& other) noexcept = default;

    // Returns size in bytes for this type
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

// Binary operation types
enum class BinaryOp { ADD, SUB, MUL, DIV, MOD, EQ, NE, LT, GT, LE, GE, AND, OR };

// Unary operation types including pointer operations
enum class UnaryOp { NEG, NOT, POS, DEREF, ADDR };

// Function parameter with name and type
struct Parameter {
    std::string name;
    Type type;

    Parameter(std::string n, Type t) : name(std::move(n)), type(std::move(t)) {}
};

// Base class for all expression nodes
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

// Compile time sizeof expression
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

// Array or pointer indexing expression
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

// Explicit type cast using 'as' keyword
struct CastExpr : Expression {
    std::unique_ptr<Expression> expr;
    Type target_type;
    CastExpr(std::unique_ptr<Expression> e, Type t)
        : expr(std::move(e)), target_type(std::move(t)) {}
};

// Base class for all statement nodes
struct Statement {
    virtual ~Statement() = default;
};

// Compile time constant declaration
struct ConstDecl : Statement {
    std::string name;
    Type type;
    int64_t value;
    ConstDecl(std::string n, Type t, int64_t v)
        : name(std::move(n)), type(std::move(t)), value(v) {}
};

// Variable declaration with initializer
struct VarDecl : Statement {
    std::string name;
    Type type;
    std::unique_ptr<Expression> initializer;
    VarDecl(std::string n, Type t, std::unique_ptr<Expression> init)
        : name(std::move(n)), type(std::move(t)), initializer(std::move(init)) {}
};

// Simple variable assignment
struct AssignStmt : Statement {
    std::string name;
    std::unique_ptr<Expression> value;
    AssignStmt(std::string n, std::unique_ptr<Expression> v)
        : name(std::move(n)), value(std::move(v)) {}
};

// Expression statement for function calls and side effects
struct ExprStmt : Statement {
    std::unique_ptr<Expression> expr;
    explicit ExprStmt(std::unique_ptr<Expression> e) : expr(std::move(e)) {}
};

// Array element assignment
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

// Assembly instruction opcodes
enum class AsmOpcode {
    MOV,
    ADD,
    SUB,
    IMUL,
    IDIV,
    NEG,
    CQO,
    XOR,
    AND,
    OR,
    TEST,
    CMP,
    PUSH,
    POP,
    CALL,
    JMP,
    RET,
    JE,
    JNE,
    JL,
    JG,
    JLE,
    JGE,
    JZ,
    JNZ,
    SETE,
    SETNE,
    SETL,
    SETG,
    SETLE,
    SETGE,
    MOVZX,
    LEA,
    SYSCALL,
    NOP
};

// Operand types for assembly instructions
enum class AsmOperandKind { REG, IMM, MEM, VAR, LABEL };

// Register encoding for asm operands
enum class AsmReg : uint8_t {
    // 64 bit registers
    RAX = 0,
    RCX = 1,
    RDX = 2,
    RBX = 3,
    RSP = 4,
    RBP = 5,
    RSI = 6,
    RDI = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,

    // 32 bit registers
    EAX = 20,
    ECX = 21,
    EDX = 22,
    EBX = 23,
    ESP = 24,
    EBP = 25,
    ESI = 26,
    EDI = 27,
    R8D = 28,
    R9D = 29,
    R10D = 30,
    R11D = 31,
    R12D = 32,
    R13D = 33,
    R14D = 34,
    R15D = 35,

    // 16 bit registers
    AX = 40,
    CX = 41,
    DX = 42,
    BX = 43,
    SP = 44,
    BP = 45,
    SI = 46,
    DI = 47,

    // 8 bit low registers
    AL = 100,
    CL = 101,
    DL = 102,
    BL = 103,

    // 8 bit high registers
    AH = 104,
    CH = 105,
    DH = 106,
    BH = 107
};

// Single operand in an assembly instruction
struct AsmOperand {
    AsmOperandKind kind;
    AsmReg reg;
    int64_t imm;
    std::string var_name;
    AsmReg mem_base;
    int32_t mem_offset;

    static AsmOperand make_reg(AsmReg r) {
        AsmOperand op;
        op.kind = AsmOperandKind::REG;
        op.reg = r;
        return op;
    }

    static AsmOperand make_imm(int64_t v) {
        AsmOperand op;
        op.kind = AsmOperandKind::IMM;
        op.imm = v;
        return op;
    }

    static AsmOperand make_var(const std::string& name) {
        AsmOperand op;
        op.kind = AsmOperandKind::VAR;
        op.var_name = name;
        return op;
    }

    static AsmOperand make_mem(AsmReg base, int32_t offset) {
        AsmOperand op;
        op.kind = AsmOperandKind::MEM;
        op.mem_base = base;
        op.mem_offset = offset;
        return op;
    }

    static AsmOperand make_label(const std::string& name) {
        AsmOperand op;
        op.kind = AsmOperandKind::LABEL;
        op.var_name = name;
        return op;
    }
};

// Single assembly instruction with opcode and operands
struct AsmInstruction {
    AsmOpcode opcode;
    std::vector<AsmOperand> operands;
};

// Inline assembly block statement
struct AsmBlock : Statement {
    std::vector<AsmInstruction> instructions;
    std::unordered_map<std::string, size_t> labels;
};

// Function definition with parameters and body
struct Function {
    std::string name;
    std::vector<Parameter> params;
    Type return_type;
    std::vector<std::unique_ptr<Statement>> body;
    Function(std::string n, std::vector<Parameter> p, Type ret)
        : name(std::move(n)), params(std::move(p)), return_type(std::move(ret)) {}
};

// Top level program containing all functions
struct Program {
    std::vector<std::unique_ptr<Function>> functions;
};

}  // namespace zex

#endif  // ZEX_AST_HPP
