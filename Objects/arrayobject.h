/*
 * Zex Programming Language
 * Objects/arrayobject.h - Array object definitions
 */

#ifndef ZEX_ARRAYOBJECT_H
#define ZEX_ARRAYOBJECT_H

#include "object.h"

/* Forward declare */
typedef struct ObjArray ObjArray;

/* Array object */
struct ObjArray {
    Obj obj;
    Value* items;
    int count;
    int capacity;
};

/* Create a new empty array */
ObjArray* new_array(void);

/* Create array with initial capacity */
ObjArray* new_array_with_capacity(int capacity);

/* Array operations */
void array_push(ObjArray* arr, Value value);
Value array_pop(ObjArray* arr);
Value array_get(ObjArray* arr, int index);
void array_set(ObjArray* arr, int index, Value value);
int array_len(ObjArray* arr);
bool array_contains(ObjArray* arr, Value value);
void array_clear(ObjArray* arr);
void array_reverse(ObjArray* arr);

/* Macros */
#define AS_ARRAY(value)     ((ObjArray*)AS_OBJ(value))
#define IS_ARRAY(value)     (is_obj_type(value, OBJ_ARRAY))

/* Get the array class */
ObjClass* get_array_class(void);

/* Initialize array class with methods */
void init_array_class(void);

#endif /* ZEX_ARRAYOBJECT_H */
