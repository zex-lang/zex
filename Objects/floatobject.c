/*
 * Zex Programming Language
 * Objects/floatobject.c - Float object implementation
 */

#include "floatobject.h"
#include "stringobject.h"
#include "classobject.h"

static ObjClass* float_class = NULL;

ObjFloat* new_float(double value) {
    ObjFloat* obj = ALLOCATE_OBJ(ObjFloat, OBJ_FLOAT, float_class);
    obj->value = value;
    return obj;
}

ObjClass* get_float_class(void) {
    return float_class;
}

void init_float_class(void) {
    float_class = new_class(new_string_cstr("float"));
}
