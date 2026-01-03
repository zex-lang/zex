/*
 * Zex Programming Language
 * Objects/tupleobject.c - Tuple object implementation
 */

#include "tupleobject.h"
#include "stringobject.h"
#include "intobject.h"
#include "boolobject.h"
#include "nullobject.h"
#include "classobject.h"
#include "funcobject.h"
#include "memory.h"

static ObjClass* tuple_class = NULL;

ObjTuple* new_tuple(Value* items, int count) {
    ObjTuple* tuple = ALLOCATE_OBJ(ObjTuple, OBJ_TUPLE, tuple_class);
    tuple->count = count;
    if (count > 0) {
        tuple->items = ALLOCATE(Value, count);
        for (int i = 0; i < count; i++) {
            tuple->items[i] = items[i];
        }
    } else {
        tuple->items = NULL;
    }
    return tuple;
}

Value tuple_get(ObjTuple* tuple, int index) {
    if (index < 0) index = tuple->count + index;
    if (index < 0 || index >= tuple->count) {
        return NULL_VAL;
    }
    return tuple->items[index];
}

int tuple_len(ObjTuple* tuple) {
    return tuple->count;
}

bool tuple_contains(ObjTuple* tuple, Value value) {
    for (int i = 0; i < tuple->count; i++) {
        if (values_equal(tuple->items[i], value)) {
            return true;
        }
    }
    return false;
}

/* Native Tuple Methods */

static Value tuple_method_len(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    if (argc != 1) return NULL_VAL;
    ObjTuple* tuple = AS_TUPLE(args[0]);
    return INT_VAL(tuple->count);
}

static Value tuple_method_first(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    if (argc != 1) return NULL_VAL;
    ObjTuple* tuple = AS_TUPLE(args[0]);
    if (tuple->count == 0) return NULL_VAL;
    return tuple->items[0];
}

static Value tuple_method_last(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    if (argc != 1) return NULL_VAL;
    ObjTuple* tuple = AS_TUPLE(args[0]);
    if (tuple->count == 0) return NULL_VAL;
    return tuple->items[tuple->count - 1];
}

static Value tuple_method_contains(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    if (argc != 2) return NULL_VAL;
    ObjTuple* tuple = AS_TUPLE(args[0]);
    return BOOL_VAL(tuple_contains(tuple, args[1]));
}

static Value tuple_method_empty(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    if (argc != 1) return NULL_VAL;
    ObjTuple* tuple = AS_TUPLE(args[0]);
    return BOOL_VAL(tuple->count == 0);
}

typedef struct {
    const char* name;
    NativeFn function;
    int arity;
} TupleMethodDef;

static TupleMethodDef tuple_methods[] = {
    {"len",      tuple_method_len,      1},
    {"first",    tuple_method_first,    1},
    {"last",     tuple_method_last,     1},
    {"contains", tuple_method_contains, 2},
    {"empty",    tuple_method_empty,    1},
    {NULL, NULL, 0}
};

ObjClass* get_tuple_class(void) {
    return tuple_class;
}

void init_tuple_class(void) {
    tuple_class = new_class(new_string_cstr("tuple"));
    
    for (TupleMethodDef* def = tuple_methods; def->name != NULL; def++) {
        ObjNative* native = new_native(def->function, def->arity, false, def->name);
        table_set(&tuple_class->methods, new_string_cstr(def->name), OBJ_VAL(native));
    }
}
