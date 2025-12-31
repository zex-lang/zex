/*
 * Zex Programming Language
 * Compiler/symtable.h - Symbol table for scoping
 */

#ifndef ZEX_SYMTABLE_H
#define ZEX_SYMTABLE_H

#include "common.h"

/* Maximum local variables per scope */
#define MAX_LOCALS 256

/* Local variable */
typedef struct {
    const char* name;
    int length;
    int depth;          /* Scope depth */
    int reg;            /* Register slot */
    bool is_captured;   /* Captured by closure */
} Local;

/* Symbol table scope */
typedef struct Scope {
    Local locals[MAX_LOCALS];
    int local_count;
    int scope_depth;
    struct Scope* enclosing;
} Scope;

/* Initialize scope */
void scope_init(Scope* scope, Scope* enclosing);

/* Begin a new scope */
void scope_begin(Scope* scope);

/* End current scope, returns number of locals to pop */
int scope_end(Scope* scope);

/* Add a local variable, returns register slot or -1 on error */
int scope_add_local(Scope* scope, const char* name, int length);

/* Resolve local variable, returns register slot or -1 if not found */
int scope_resolve_local(Scope* scope, const char* name, int length);

#endif /* ZEX_SYMTABLE_H */
