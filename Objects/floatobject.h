/*
 * Zex Programming Language
 * Objects/floatobject.h - Float object definition
 */

#ifndef ZEX_FLOATOBJECT_H
#define ZEX_FLOATOBJECT_H

#include "object.h"

/* Float object */
typedef struct {
    Obj obj;
    double value;
} ObjFloat;

/* Create a new float object */
ObjFloat* new_float(double value);

/* Get float value from Value */
#define AS_FLOAT(value) (((ObjFloat*)AS_OBJ(value))->value)

/* Create Value from double */
#define FLOAT_VAL(val)  OBJ_VAL(new_float(val))

/* Get the float class */
ObjClass* get_float_class(void);

/* Initialize float class */
void init_float_class(void);

#endif /* ZEX_FLOATOBJECT_H */
