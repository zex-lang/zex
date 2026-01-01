/*
 * Zex Programming Language
 * Objects/intobject.c - Integer object implementation
 */

#include "intobject.h"
#include "stringobject.h"
#include "classobject.h"
#include "funcobject.h"

static ObjClass* int_class = NULL;

ObjInt* new_int(int64_t value) {
    ObjInt* obj = ALLOCATE_OBJ(ObjInt, OBJ_INT, int_class);
    obj->value = value;
    return obj;
}

/* Native int methods */

/* i.abs() -> int */
static Value int_abs(VM* vm, int argc, Value* args) {
    UNUSED(vm); UNUSED(argc);
    ObjInt* i = (ObjInt*)AS_OBJ(args[0]);
    int64_t val = i->value < 0 ? -i->value : i->value;
    return INT_VAL(val);
}

/* Method registration table */
typedef struct {
    const char* name;
    NativeFn function;
    int arity;
} IntMethodDef;

static IntMethodDef int_methods[] = {
    {"abs", int_abs, 1},
    {NULL, NULL, 0}
};

ObjClass* get_int_class(void) {
    return int_class;
}

void init_int_class(void) {
    int_class = new_class(new_string_cstr("int"));
    
    /* Register all int methods */
    for (IntMethodDef* def = int_methods; def->name != NULL; def++) {
        ObjNative* native = new_native(def->function, def->arity, def->name);
        table_set(&int_class->methods, new_string_cstr(def->name), OBJ_VAL(native));
    }
}
