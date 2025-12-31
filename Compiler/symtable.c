/*
 * Zex Programming Language
 * Compiler/symtable.c - Symbol table implementation
 */

#include "symtable.h"
#include <string.h>

void scope_init(Scope* scope, Scope* enclosing) {
    scope->local_count = 0;
    scope->scope_depth = enclosing ? enclosing->scope_depth : 0;
    scope->enclosing = enclosing;
}

void scope_begin(Scope* scope) {
    scope->scope_depth++;
}

int scope_end(Scope* scope) {
    scope->scope_depth--;
    
    int count = 0;
    while (scope->local_count > 0 &&
           scope->locals[scope->local_count - 1].depth > scope->scope_depth) {
        scope->local_count--;
        count++;
    }
    
    return count;
}

int scope_add_local(Scope* scope, const char* name, int length) {
    if (scope->local_count >= MAX_LOCALS) {
        return -1;  /* Too many locals */
    }
    
    /* Check for duplicate in current scope */
    for (int i = scope->local_count - 1; i >= 0; i--) {
        Local* local = &scope->locals[i];
        if (local->depth < scope->scope_depth) {
            break;  /* Different scope */
        }
        if (local->length == length && memcmp(local->name, name, length) == 0) {
            return -2;  /* Duplicate variable */
        }
    }
    
    Local* local = &scope->locals[scope->local_count];
    local->name = name;
    local->length = length;
    local->depth = scope->scope_depth;
    local->reg = scope->local_count;
    local->is_captured = false;
    
    return scope->local_count++;
}

int scope_resolve_local(Scope* scope, const char* name, int length) {
    for (int i = scope->local_count - 1; i >= 0; i--) {
        Local* local = &scope->locals[i];
        if (local->length == length && memcmp(local->name, name, length) == 0) {
            return local->reg;
        }
    }
    return -1;  /* Not found */
}
