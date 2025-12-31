/*
 * Zex Programming Language
 * Objects/nullobject.c - Null object implementation
 */

#include "nullobject.h"
#include "stringobject.h"
#include "classobject.h"

static ObjClass* null_class = NULL;

/* Singleton instance */
ObjNull* null_instance = NULL;

ObjClass* get_null_class(void) {
    return null_class;
}

void init_null_class(void) {
    null_class = new_class(new_string_cstr("null"));
    
    /* Create singleton null */
    null_instance = ALLOCATE_OBJ(ObjNull, OBJ_NULL, null_class);
}
