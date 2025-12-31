/*
 * Zex Programming Language
 * Objects/intobject.c - Integer object implementation
 */

#include "intobject.h"
#include "stringobject.h"
#include "classobject.h"

static ObjClass* int_class = NULL;

ObjInt* new_int(int64_t value) {
    ObjInt* obj = ALLOCATE_OBJ(ObjInt, OBJ_INT, int_class);
    obj->value = value;
    return obj;
}

ObjClass* get_int_class(void) {
    return int_class;
}

void init_int_class(void) {
    int_class = new_class(new_string_cstr("int"));
}
