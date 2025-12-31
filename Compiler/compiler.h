/*
 * Zex Programming Language
 * Compiler/compiler.h - Compiler interface
 */

#ifndef ZEX_COMPILER_H
#define ZEX_COMPILER_H

#include "common.h"
#include "bytecode.h"
#include "ast.h"
#include "funcobject.h"

/* Compilation result */
typedef struct {
    ObjFunction* function;
    bool had_error;
} CompileResult;

/* Compile AST to bytecode */
CompileResult compile(ASTNode* ast);

/* Compile source code directly */
CompileResult compile_source(const char* source);

#endif /* ZEX_COMPILER_H */
