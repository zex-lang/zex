/*
 * Zex Programming Language
 * Objects/intobject.h - Integer object definition
 */

#ifndef ZEX_INTOBJECT_H
#define ZEX_INTOBJECT_H

#include "object.h"

/* Integer object */
typedef struct {
    Obj obj;
    int64_t value;
} ObjInt;

/* Create a new integer object */
ObjInt* new_int(int64_t value);

/* Get integer value from Value */
#define AS_INT(v)   (((ObjInt*)AS_OBJ(v))->value)

/* Create Value from int64_t */
#define INT_VAL(val)    OBJ_VAL(new_int(val))

/* Get the int class */
ObjClass* get_int_class(void);

/* Initialize int class */
void init_int_class(void);

#endif /* ZEX_INTOBJECT_H */
