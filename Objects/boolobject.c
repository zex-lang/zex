/*
 * Zex Programming Language
 * Objects/boolobject.c - Boolean object implementation
 */

#include "boolobject.h"
#include "stringobject.h"
#include "classobject.h"

static ObjClass* bool_class = NULL;

/* Singleton instances */
ObjBool* bool_true = NULL;
ObjBool* bool_false = NULL;

ObjClass* get_bool_class(void) {
    return bool_class;
}

void init_bool_class(void) {
    bool_class = new_class(new_string_cstr("bool"));
    
    /* Create singleton true */
    bool_true = ALLOCATE_OBJ(ObjBool, OBJ_BOOL, bool_class);
    bool_true->value = true;
    
    /* Create singleton false */
    bool_false = ALLOCATE_OBJ(ObjBool, OBJ_BOOL, bool_class);
    bool_false->value = false;
}
