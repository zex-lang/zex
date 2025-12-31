/*
 * Zex Programming Language
 * Objects/boolobject.h - Boolean object definition
 */

#ifndef ZEX_BOOLOBJECT_H
#define ZEX_BOOLOBJECT_H

#include "object.h"

/* Boolean object - singleton true/false */
typedef struct {
    Obj obj;
    bool value;
} ObjBool;

/* Singleton instances */
extern ObjBool* bool_true;
extern ObjBool* bool_false;

/* Get boolean value */
#define AS_BOOL(value)  (((ObjBool*)AS_OBJ(value))->value)

/* Create Value from bool */
#define BOOL_VAL(val)   OBJ_VAL((val) ? bool_true : bool_false)

/* Get the bool class */
ObjClass* get_bool_class(void);

/* Initialize bool class and singletons */
void init_bool_class(void);

#endif /* ZEX_BOOLOBJECT_H */
