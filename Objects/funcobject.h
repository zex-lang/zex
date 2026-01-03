/*
 * Zex Programming Language
 * Objects/funcobject.h - Function object definitions
 */

#ifndef ZEX_FUNCOBJECT_H
#define ZEX_FUNCOBJECT_H

#include "object.h"

/* Forward declarations */
struct Chunk;

/* Function object */
struct ObjFunction {
    Obj obj;
    int arity;                  /* Number of required parameters */
    bool has_rest;              /* Has rest parameter (..param) */
    int upvalue_count;          /* Number of captured variables */
    struct Chunk* chunk;        /* Bytecode */
    ObjString* name;            /* Function name */
};

/* Create a new function */
ObjFunction* new_function(void);

/* Get as function */
#define AS_FUNCTION(value)  ((ObjFunction*)AS_OBJ(value))

/* Native function signature */
typedef Value (*NativeFn)(VM* vm, int argc, Value* args);

/* Native function object */
struct ObjNative {
    Obj obj;
    NativeFn function;
    int arity;                  /* Min required args */
    bool has_rest;              /* Accepts extra args */
    ObjString* name;
};

/* Create a native function */
ObjNative* new_native(NativeFn function, int arity, bool has_rest, const char* name);

/* Get as native */
#define AS_NATIVE(value)    ((ObjNative*)AS_OBJ(value))

/* Bound method - method bound to instance */
struct ObjBoundMethod {
    Obj obj;
    Value receiver;             /* The instance (self) */
    Value method;               /* The method function (closure or native) */
};

/* Create a bound method */
ObjBoundMethod* new_bound_method(Value receiver, Value method);

/* Get as bound method */
#define AS_BOUND_METHOD(value) ((ObjBoundMethod*)AS_OBJ(value))

#endif /* ZEX_FUNCOBJECT_H */
