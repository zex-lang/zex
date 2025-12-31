/*
 * Zex Programming Language
 * Objects/nullobject.h - Null object definition
 */

#ifndef ZEX_NULLOBJECT_H
#define ZEX_NULLOBJECT_H

#include "object.h"

/* Null object - singleton */
typedef struct {
    Obj obj;
} ObjNull;

/* Singleton instance */
extern ObjNull* null_instance;

/* Create null Value */
#define NULL_VAL        OBJ_VAL(null_instance)

/* Get the null class */
ObjClass* get_null_class(void);

/* Initialize null class and singleton */
void init_null_class(void);

#endif /* ZEX_NULLOBJECT_H */
