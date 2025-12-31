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
    
    /* Statements */
    AST_VAR_DECL,
    AST_ASSIGN,
    AST_COMPOUND_ASSIGN,
    AST_EXPR_STMT,
    AST_BLOCK,
    AST_IF,
    AST_WHILE,
    AST_DO_WHILE,
    AST_BREAK,
    AST_CONTINUE,
    AST_RETURN,
    AST_FUN_DECL,
    AST_CLASS_DECL,
    AST_PROPERTY_DECL,
    AST_PROGRAM,
} ASTNodeType;

/* Binary operators */
typedef enum {
    BINOP_ADD,
    BINOP_SUB,
    BINOP_MUL,
    BINOP_DIV,
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
} CompoundOp;

/* Forward declaration */
typedef struct ASTNode ASTNode;

/* Parameter list */
typedef struct {
    char** names;
    int count;
    int capacity;
} ParameterList;

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
        
        /* AST_RETURN */
        struct {
            ASTNode* value;        /* Can be NULL */
        } return_stmt;
        
        /* AST_FUN_DECL */
        struct {
            char* name;
            ParameterList params;
            ASTNode* body;         /* Block */
        } fun_decl;
        
        /* AST_CLASS_DECL */
        struct {
            char* name;
            ASTNode** properties;  /* var declarations */
            int property_count;
            ASTNode** methods;     /* function declarations */
            int method_count;
        } class_decl;
        
        /* AST_PROPERTY_DECL (inside class) */
        struct {
            char* name;
            ASTNode* initializer;
        } property_decl;
        
        /* AST_PROGRAM */
        struct {
            ASTNode** statements;
            int count;
        } program;
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
ASTNode* ast_new_call(ASTNode* callee, ASTNode** args, int arg_count, int line, int column);
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
ASTNode* ast_new_break(int line, int column);
ASTNode* ast_new_continue(int line, int column);
ASTNode* ast_new_return(ASTNode* value, int line, int column);
ASTNode* ast_new_fun_decl(const char* name, ParameterList params, ASTNode* body, int line, int column);
ASTNode* ast_new_class_decl(const char* name, ASTNode** properties, int property_count,
                            ASTNode** methods, int method_count, int line, int column);
ASTNode* ast_new_property_decl(const char* name, ASTNode* initializer, int line, int column);
ASTNode* ast_new_program(ASTNode** statements, int count);

/* Free AST */
void ast_free(ASTNode* node);

/* Debug print AST */
void ast_print(ASTNode* node, int indent);

#endif /* ZEX_AST_H */
