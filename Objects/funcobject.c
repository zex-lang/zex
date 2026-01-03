/*
 * Zex Programming Language
 * Objects/funcobject.c - Function object implementation
 */

#include "funcobject.h"
#include "classobject.h"
#include "stringobject.h"

ObjFunction* new_function(void) {
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION, NULL);
    function->arity = 0;
    function->has_rest = false;
    function->upvalue_count = 0;
    function->name = NULL;
    function->chunk = NULL;
    return function;
}

ObjNative* new_native(NativeFn function, int arity, bool has_rest, const char* name) {
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE, NULL);
    native->function = function;
    native->arity = arity;
    native->has_rest = has_rest;
    native->name = new_string_cstr(name);
    return native;
}

ObjBoundMethod* new_bound_method(Value receiver, Value method) {
    ObjBoundMethod* bound = ALLOCATE_OBJ(ObjBoundMethod, OBJ_BOUND_METHOD, NULL);
    bound->receiver = receiver;
    bound->method = method;
    return bound;
}
