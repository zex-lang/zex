/*
 * Zex Programming Language
 * Objects/floatobject.c - Float object implementation
 */

#include "floatobject.h"
#include "stringobject.h"
#include "classobject.h"
#include "funcobject.h"
#include "intobject.h"
#include <math.h>

static ObjClass* float_class = NULL;

ObjFloat* new_float(double value) {
    ObjFloat* obj = ALLOCATE_OBJ(ObjFloat, OBJ_FLOAT, float_class);
    obj->value = value;
    return obj;
}

/* Native float methods */

/* f.abs() -> float */
static Value float_abs(VM* vm, int argc, Value* args) {
    UNUSED(vm); UNUSED(argc);
    ObjFloat* f = (ObjFloat*)AS_OBJ(args[0]);
    return FLOAT_VAL(fabs(f->value));
}

/* f.floor() -> int */
static Value float_floor(VM* vm, int argc, Value* args) {
    UNUSED(vm); UNUSED(argc);
    ObjFloat* f = (ObjFloat*)AS_OBJ(args[0]);
    return INT_VAL((int64_t)floor(f->value));
}

/* f.ceil() -> int */
static Value float_ceil(VM* vm, int argc, Value* args) {
    UNUSED(vm); UNUSED(argc);
    ObjFloat* f = (ObjFloat*)AS_OBJ(args[0]);
    return INT_VAL((int64_t)ceil(f->value));
}

/* f.round() -> int */
static Value float_round(VM* vm, int argc, Value* args) {
    UNUSED(vm); UNUSED(argc);
    ObjFloat* f = (ObjFloat*)AS_OBJ(args[0]);
    return INT_VAL((int64_t)round(f->value));
}

/* Method registration table */
typedef struct {
    const char* name;
    NativeFn function;
    int arity;
    bool has_rest;
} FloatMethodDef;

static FloatMethodDef float_methods[] = {
    {"abs",   float_abs,   1, false},
    {"floor", float_floor, 1, false},
    {"ceil",  float_ceil,  1, false},
    {"round", float_round, 1, false},
    {NULL, NULL, 0, false}
};

ObjClass* get_float_class(void) {
    return float_class;
}

void init_float_class(void) {
    float_class = new_class(new_string_cstr("float"));
    
    /* Register all float methods */
    for (FloatMethodDef* def = float_methods; def->name != NULL; def++) {
        ObjNative* native = new_native(def->function, def->arity, def->has_rest, def->name);
        table_set(&float_class->methods, new_string_cstr(def->name), OBJ_VAL(native));
    }
}
