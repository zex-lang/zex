/*
 * Zex Programming Language
 * Objects/classobject.h - Class and instance object definitions
 */

#ifndef ZEX_CLASSOBJECT_H
#define ZEX_CLASSOBJECT_H

#include "object.h"
#include "tableobject.h"

/* Class object */
struct ObjClass {
    Obj obj;
    ObjString* name;            /* Class name */
    struct ObjClass* superclass; /* Parent class (NULL if none) */
    Table methods;              /* Instance method table */
    Table static_members;       /* Static fields and methods */
    Table default_properties;   /* Default property values */
    Table getters;              /* Computed property getters */
    Table setters;              /* Computed property setters */
    Table member_visibility;    /* Visibility: 0=public, 1=protected, 2=private */
    ObjFunction* init;          /* Constructor (init method) */
};

/* Create a new class */
ObjClass* new_class(ObjString* name);

/* Get as class */
#define AS_CLASS(value)     ((ObjClass*)AS_OBJ(value))

/* Instance object */
struct ObjInstance {
    Obj obj;                    /* Base object (includes klass pointer) */
    Table properties;           /* Instance properties */
};

/* Create a new instance of a class */
ObjInstance* new_instance(ObjClass* klass);

/* Get as instance */
#define AS_INSTANCE(value)  ((ObjInstance*)AS_OBJ(value))

/* Get string representation of value for string() constructor */
#define AS_STRING(value)    ((ObjString*)AS_OBJ(value))

/* Create Value from string object */
#define STRING_VAL(obj)     OBJ_VAL(obj)

/* Get the string class */
ObjClass* get_string_class(void);

/* Initialize string class */
void init_string_class(void);

#endif /* ZEX_CLASSOBJECT_H */
