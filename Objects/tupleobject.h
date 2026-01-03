/*
 * Zex Programming Language
 * Objects/tupleobject.h - Tuple object definitions
 */

#ifndef ZEX_TUPLEOBJECT_H
#define ZEX_TUPLEOBJECT_H

#include "object.h"

typedef struct ObjTuple ObjTuple;

struct ObjTuple {
    Obj obj;
    Value* items;
    int count;
};

/* Create a new tuple from array of values */
ObjTuple* new_tuple(Value* items, int count);

/* Tuple operations */
Value tuple_get(ObjTuple* tuple, int index);
int tuple_len(ObjTuple* tuple);
bool tuple_contains(ObjTuple* tuple, Value value);

/* Macros */
#define AS_TUPLE(value)     ((ObjTuple*)AS_OBJ(value))
#define IS_TUPLE(value)     (is_obj_type(value, OBJ_TUPLE))

/* Get the tuple class */
ObjClass* get_tuple_class(void);

/* Initialize tuple class */
void init_tuple_class(void);

#endif /* ZEX_TUPLEOBJECT_H */
