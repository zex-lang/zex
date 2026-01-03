/*
 * Zex Programming Language
 * Parser/ast.h - AST node definitions
 */

#ifndef ZEX_AST_H
#define ZEX_AST_H

#include "common.h"
#include "token.h"

/* AST node types */
typedef enum {
    /* Expressions */
    AST_INT_LITERAL,
    AST_FLOAT_LITERAL,
    AST_STRING_LITERAL,
    AST_BOOL_LITERAL,
    AST_NULL_LITERAL,
    AST_IDENTIFIER,
    AST_BINARY,
    AST_UNARY,
    AST_CALL,
    AST_GET,            /* obj.property */
    AST_SET,            /* obj.property = value */
    AST_SET_COMPOUND,   /* obj.property += value */
    AST_SELF,
    AST_GROUPING,
    AST_ARRAY,          /* [expr, expr, ...] */
    AST_INDEX_GET,      /* arr[index] */
    AST_INDEX_SET,      /* arr[index] = value */
    AST_CLOSURE,        /* |params| body */
    
    /* Statements */
    AST_VAR_DECL,
    AST_ASSIGN,
    AST_COMPOUND_ASSIGN,
    AST_EXPR_STMT,
    AST_BLOCK,
    AST_IF,
    AST_WHILE,
    AST_DO_WHILE,
    AST_FOR,            /* C-style for loop */
    AST_FOR_IN,         /* for-in iterator loop */
    AST_BREAK,
    AST_CONTINUE,
    AST_RETURN,
    AST_FUN_DECL,
    AST_CLASS_DECL,
    AST_PROGRAM,
    AST_TRY,
    AST_RAISE,
    AST_TUPLE,
    AST_SUPER,          /* super.method() or super() */
} ASTNodeType;

/* Binary operators */
typedef enum {
    BINOP_ADD,
    BINOP_SUB,
    BINOP_MUL,
    BINOP_DIV,
    BINOP_MOD,
    BINOP_EQ,
    BINOP_NE,
    BINOP_LT,
    BINOP_LE,
    BINOP_GT,
    BINOP_GE,
    BINOP_AND,
    BINOP_OR,
} BinaryOp;

/* Unary operators */
typedef enum {
    UNOP_NEG,     /* - */
    UNOP_NOT,     /* ! */
    UNOP_POS,     /* + (no-op) */
} UnaryOp;

/* Compound assignment operators */
typedef enum {
    COMPOUND_ADD,   /* += */
    COMPOUND_SUB,   /* -= */
    COMPOUND_MUL,   /* *= */
    COMPOUND_DIV,   /* /= */
    COMPOUND_MOD,   /* %= */
} CompoundOp;

/* Visibility modifiers for class members */
typedef enum {
    VISIBILITY_PRIVATE,     /* Default */
    VISIBILITY_PUBLIC,
    VISIBILITY_PROTECTED,
} Visibility;

/* Class member types */
typedef enum {
    MEMBER_FIELD,           /* Regular field declaration */
    MEMBER_METHOD,          /* Method (including constructor) */
    MEMBER_COMPUTED_PROP,   /* Computed property with get/set */
} ClassMemberType;

/* Forward declaration */
typedef struct ASTNode ASTNode;
typedef struct ClassMember ClassMember;

/* Parameter list - defined early for use in ClassMember */
typedef struct {
    char** names;
    int count;
    int capacity;
    bool has_rest;          /* True if last param is rest (..param) */
} ParameterList;

/* Class member: field, method, or computed property */
struct ClassMember {
    ClassMemberType member_type;
    Visibility visibility;
    bool is_static;
    bool is_override;       /* For methods only */
    char* name;             /* Member name */
    
    union {
        /* MEMBER_FIELD */
        struct {
            ASTNode* initializer;   /* Can be NULL */
        } field;
        
        /* MEMBER_METHOD */
        struct {
            ParameterList params;
            ASTNode* body;          /* Block */
            bool is_constructor;    /* True if name matches class name */
        } method;
        
        /* MEMBER_COMPUTED_PROP */
        struct {
            ASTNode* getter;        /* Getter body, can be NULL */
            ASTNode* setter;        /* Setter body, can be NULL */
            char* setter_param;     /* Parameter name in setter */
        } computed;
    } as;
};


/* Except handler: except ExceptionType as var { body } */
typedef struct {
    char* type;             /* Exception type name, NULL for bare except */
    char* var;              /* Variable name for exception, NULL if not captured */
    ASTNode* body;          /* Handler body (block) */
} ExceptHandler;

/* AST node structure - union of all node types */
struct ASTNode {
    ASTNodeType type;
    int line;
    int column;
    
    union {
        /* AST_INT_LITERAL */
        struct {
            int64_t value;
        } int_literal;
        
        /* AST_FLOAT_LITERAL */
        struct {
            double value;
        } float_literal;
        
        /* AST_STRING_LITERAL */
        struct {
            char* value;
            int length;
        } string_literal;
        
        /* AST_BOOL_LITERAL */
        struct {
            bool value;
        } bool_literal;
        
        /* AST_IDENTIFIER */
        struct {
            char* name;
        } identifier;
        
        /* AST_BINARY */
        struct {
            BinaryOp op;
            ASTNode* left;
            ASTNode* right;
        } binary;
        
        /* AST_UNARY */
        struct {
            UnaryOp op;
            ASTNode* operand;
        } unary;
        
        /* AST_CALL */
        struct {
            ASTNode* callee;
            ASTNode** arguments;
            bool* is_spread;        /* is_spread[i] = true if arg i is spread (..arg) */
            int arg_count;
        } call;
        
        /* AST_GET */
        struct {
            ASTNode* object;
            char* property;
        } get;
        
        /* AST_SET */
        struct {
            ASTNode* object;
            char* property;
            ASTNode* value;
        } set;
        
        /* AST_SET_COMPOUND */
        struct {
            ASTNode* object;
            char* property;
            BinaryOp op;
            ASTNode* value;
        } set_compound;
        
        /* AST_GROUPING */
        struct {
            ASTNode* expression;
        } grouping;
        
        /* AST_ARRAY */
        struct {
            ASTNode** elements;
            int count;
        } array;
        
        /* AST_TUPLE */
        struct {
            ASTNode** elements;
            int count;
        } tuple;
        
        /* AST_INDEX_GET - arr[index] */
        struct {
            ASTNode* object;
            ASTNode* index;
        } index_get;
        
        /* AST_INDEX_SET - arr[index] = value */
        struct {
            ASTNode* object;
            ASTNode* index;
            ASTNode* value;
        } index_set;
        
        /* AST_VAR_DECL */
        struct {
            char* name;
            ASTNode* initializer;  /* Can be NULL */
        } var_decl;
        
        /* AST_ASSIGN */
        struct {
            char* name;
            ASTNode* value;
        } assign;
        
        /* AST_COMPOUND_ASSIGN */
        struct {
            char* name;
            CompoundOp op;
            ASTNode* value;
        } compound_assign;
        
        /* AST_EXPR_STMT */
        struct {
            ASTNode* expression;
        } expr_stmt;
        
        /* AST_BLOCK */
        struct {
            ASTNode** statements;
            int count;
        } block;
        
        /* AST_IF */
        struct {
            ASTNode* condition;
            ASTNode* then_branch;
            ASTNode* else_branch;  /* Can be NULL or another IF node */
        } if_stmt;
        
        /* AST_WHILE */
        struct {
            ASTNode* condition;
            ASTNode* body;
        } while_stmt;
        
        /* AST_DO_WHILE */
        struct {
            ASTNode* body;
            ASTNode* condition;
        } do_while_stmt;
        
        /* AST_FOR */
        struct {
            ASTNode* initializer;  /* var decl or expr, can be NULL */
            ASTNode* condition;    /* can be NULL (infinite loop) */
            ASTNode* update;       /* can be NULL */
            ASTNode* body;
        } for_stmt;
        
        /* AST_FOR_IN */
        struct {
            char* var_name;
            ASTNode* iterable;
            ASTNode* body;
        } for_in_stmt;
        
        /* AST_RETURN */
        struct {
            ASTNode* value;        /* Can be NULL */
        } return_stmt;
        
        /* AST_BREAK */
        struct {
            ASTNode* value;        /* Can be NULL - break with value like Rust */
        } break_stmt;
        
        /* AST_FUN_DECL */
        struct {
            char* name;
            ParameterList params;
            ASTNode* body;         /* Block */
        } fun_decl;
        
        /* AST_CLASS_DECL */
        struct {
            char* name;
            char** superclasses;    /* Multiple inheritance parents, can be NULL */
            int superclass_count;
            ClassMember* members;   /* Unified list: fields, methods, computed props */
            int member_count;
        } class_decl;
        
        /* AST_SUPER */
        struct {
            char* method;           /* Method name, NULL for super() constructor call */
            ASTNode** arguments;
            int arg_count;
        } super_expr;
        
        /* AST_CLOSURE */
        struct {
            ParameterList params;
            ASTNode* body;         /* Block or single expression */
            bool is_expression;    /* true if body is single expression */
        } closure;
        
        /* AST_PROGRAM */
        struct {
            ASTNode** statements;
            int count;
        } program;
        
        /* AST_TRY */
        struct {
            ASTNode* try_body;
            ExceptHandler* handlers;
            int handler_count;
            ASTNode* else_body;     /* Can be NULL */
            ASTNode* finally_body;  /* Can be NULL */
        } try_stmt;
        
        /* AST_RAISE */
        struct {
            ASTNode* exception;     /* Expression, NULL for re-raise */
        } raise_stmt;
    } as;
};

/* Create AST nodes */
ASTNode* ast_new_int_literal(int64_t value, int line, int column);
ASTNode* ast_new_float_literal(double value, int line, int column);
ASTNode* ast_new_string_literal(const char* value, int length, int line, int column);
ASTNode* ast_new_bool_literal(bool value, int line, int column);
ASTNode* ast_new_null_literal(int line, int column);
ASTNode* ast_new_identifier(const char* name, int line, int column);
ASTNode* ast_new_binary(BinaryOp op, ASTNode* left, ASTNode* right, int line, int column);
ASTNode* ast_new_unary(UnaryOp op, ASTNode* operand, int line, int column);
ASTNode* ast_new_call(ASTNode* callee, ASTNode** args, bool* is_spread, int arg_count, int line, int column);
ASTNode* ast_new_get(ASTNode* object, const char* property, int line, int column);
ASTNode* ast_new_set(ASTNode* object, const char* property, ASTNode* value, int line, int column);
ASTNode* ast_new_set_compound(ASTNode* object, const char* property, BinaryOp op, ASTNode* value, int line, int column);
ASTNode* ast_new_self(int line, int column);
ASTNode* ast_new_grouping(ASTNode* expression, int line, int column);
ASTNode* ast_new_var_decl(const char* name, ASTNode* initializer, int line, int column);
ASTNode* ast_new_assign(const char* name, ASTNode* value, int line, int column);
ASTNode* ast_new_compound_assign(const char* name, CompoundOp op, ASTNode* value, int line, int column);
ASTNode* ast_new_expr_stmt(ASTNode* expression, int line, int column);
ASTNode* ast_new_block(ASTNode** statements, int count, int line, int column);
ASTNode* ast_new_if(ASTNode* condition, ASTNode* then_branch, ASTNode* else_branch, int line, int column);
ASTNode* ast_new_while(ASTNode* condition, ASTNode* body, int line, int column);
ASTNode* ast_new_do_while(ASTNode* body, ASTNode* condition, int line, int column);
ASTNode* ast_new_for(ASTNode* initializer, ASTNode* condition, ASTNode* update, ASTNode* body, int line, int column);
ASTNode* ast_new_for_in(const char* var_name, ASTNode* iterable, ASTNode* body, int line, int column);
ASTNode* ast_new_break(ASTNode* value, int line, int column);
ASTNode* ast_new_continue(int line, int column);
ASTNode* ast_new_return(ASTNode* value, int line, int column);
ASTNode* ast_new_fun_decl(const char* name, ParameterList params, ASTNode* body, int line, int column);
ASTNode* ast_new_class_decl(const char* name, char** superclasses, int superclass_count,
                            ClassMember* members, int member_count, int line, int column);
ASTNode* ast_new_super(const char* method, ASTNode** arguments, int arg_count, int line, int column);
ASTNode* ast_new_array(ASTNode** elements, int count, int line, int column);
ASTNode* ast_new_index_get(ASTNode* object, ASTNode* index, int line, int column);
ASTNode* ast_new_index_set(ASTNode* object, ASTNode* index, ASTNode* value, int line, int column);
ASTNode* ast_new_closure(ParameterList params, ASTNode* body, bool is_expression, int line, int column);
ASTNode* ast_new_program(ASTNode** statements, int count);
ASTNode* ast_new_try(ASTNode* try_body, ExceptHandler* handlers, int handler_count,
                     ASTNode* else_body, ASTNode* finally_body, int line, int column);
ASTNode* ast_new_raise(ASTNode* exception, int line, int column);
ASTNode* ast_new_tuple(ASTNode** elements, int count, int line, int column);

/* Free AST */
void ast_free(ASTNode* node);

/* Debug print AST */
void ast_print(ASTNode* node, int indent);

#endif /* ZEX_AST_H */
