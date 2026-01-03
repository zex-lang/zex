/*
 * Zex Programming Language
 * Objects/object.h - Base object definitions
 *
 * Everything in Zex is an object. This file defines the base object
 * structure that all types inherit from.
 */

#ifndef ZEX_OBJECT_H
#define ZEX_OBJECT_H

#include "common.h"
#include "memory.h"

/* Object types */
typedef enum {
    OBJ_INT,
    OBJ_FLOAT,
    OBJ_BOOL,
    OBJ_NULL,
    OBJ_STRING,
    OBJ_FUNCTION,
    OBJ_CLASS,
    OBJ_INSTANCE,
    OBJ_NATIVE,
    OBJ_BOUND_METHOD,
    OBJ_ARRAY,
    OBJ_EXCEPTION,
    OBJ_TUPLE,
} ObjType;

/*
 * Base object header.
 * All Zex values have this structure at memory offset 0.
 * This enables polymorphic access via casting.
 */
struct Obj {
    ObjType type;
    bool is_marked;         /* GC mark bit */
    struct Obj* next;       /* Intrusive list for GC tracking */
    ObjClass* klass;        /* The class of this object */
};

/*
 * Value representation.
 * Using tagged union for efficiency - primitives store value inline,
 * heap objects store pointer.
 */
typedef struct {
    enum {
        VAL_OBJ,            /* Heap-allocated object */
    } type;
    Obj* obj;
} Value;

/* Value macros */
#define OBJ_VAL(object)     ((Value){VAL_OBJ, (Obj*)(object)})
#define AS_OBJ(value)       ((value).obj)

#define IS_INT(value)       (is_obj_type(value, OBJ_INT))
#define IS_FLOAT(value)     (is_obj_type(value, OBJ_FLOAT))
#define IS_BOOL(value)      (is_obj_type(value, OBJ_BOOL))
#define IS_NULL(value)      (is_obj_type(value, OBJ_NULL))
#define IS_STRING(value)    (is_obj_type(value, OBJ_STRING))
#define IS_FUNCTION(value)  (is_obj_type(value, OBJ_FUNCTION))
#define IS_CLASS(value)     (is_obj_type(value, OBJ_CLASS))
#define IS_INSTANCE(value)  (is_obj_type(value, OBJ_INSTANCE))
#define IS_NATIVE(value)    (is_obj_type(value, OBJ_NATIVE))
#define IS_BOUND_METHOD(value) (is_obj_type(value, OBJ_BOUND_METHOD))
#define IS_ARRAY(value)     (is_obj_type(value, OBJ_ARRAY))
#define IS_EXCEPTION(value) (is_obj_type(value, OBJ_EXCEPTION))

/* Type checking helper */
static inline bool is_obj_type(Value value, ObjType type) {
    return value.obj != NULL && value.obj->type == type;
}

/* Object type name for error messages */
const char* obj_type_name(ObjType type);

/* Get type name of a value */
const char* value_type_name(Value value);

/* Print a value */
void print_value(Value value);

/* Convert value to string representation */
ObjString* value_to_string(Value value);

/* Check if two values are equal */
bool values_equal(Value a, Value b);

/* Check if value is truthy */
bool is_truthy(Value value);

/* Allocate an object of given type */
Obj* allocate_object(size_t size, ObjType type, ObjClass* klass);

#define ALLOCATE_OBJ(type, obj_type, klass) \
    (type*)allocate_object(sizeof(type), obj_type, klass)

/* Free all objects (for cleanup) */
void free_objects(void);

/* GC roots - linked list of all objects */
extern Obj* vm_objects;

#endif /* ZEX_OBJECT_H */
