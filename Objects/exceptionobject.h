/*
 * Zex Programming Language
 * Objects/exceptionobject.h - Exception object definitions
 *
 * Exception objects represent runtime errors that can be caught
 * and handled using try/except/finally blocks.
 */

#ifndef ZEX_EXCEPTIONOBJECT_H
#define ZEX_EXCEPTIONOBJECT_H

#include "object.h"
#include "stringobject.h"

/* Forward declaration */
typedef struct ObjException ObjException;

/*
 * Exception object - represents a catchable error.
 * Stores error message, source position, and traceback info.
 */
struct ObjException {
    Obj obj;
    ObjString* message;         /* Error message */
    ObjClass* exception_class;  /* Exception type (ValueError, TypeError, etc.) */
    int line;                   /* Source line where exception occurred */
    int column;                 /* Source column */
    int span;                   /* Error span length */
    ObjString* traceback;       /* Formatted traceback string */
};

/* Create a new exception object */
ObjException* new_exception(ObjClass* exception_class, ObjString* message,
                             int line, int column, int span);

/* Create exception with C string message */
ObjException* new_exception_cstr(ObjClass* exception_class, const char* message,
                                  int line, int column, int span);

/* Type checking macros */
#define IS_EXCEPTION(value)  (is_obj_type(value, OBJ_EXCEPTION))
#define AS_EXCEPTION(value)  ((ObjException*)AS_OBJ(value))

/* Exception Value macro */
#define EXCEPTION_VAL(obj)   OBJ_VAL(obj)

/*
 * Built-in exception class hierarchy.
 * All exceptions inherit from Exception base class.
 */

/* Initialize all exception classes */
void init_exception_classes(void);

/* Get exception classes */
ObjClass* get_exception_class(void);           /* Base Exception class */
ObjClass* get_value_error_class(void);         /* Invalid value */
ObjClass* get_type_error_class(void);          /* Type mismatch */
ObjClass* get_index_error_class(void);         /* Index out of bounds */
ObjClass* get_zero_division_error_class(void); /* Division by zero */
ObjClass* get_attribute_error_class(void);     /* Undefined attribute/property */
ObjClass* get_name_error_class(void);          /* Undefined variable */
ObjClass* get_runtime_error_class(void);       /* General runtime error */

/* Check if exception is instance of a class (including inheritance) */
bool exception_is_type(Value exception, ObjClass* exception_class);

#endif /* ZEX_EXCEPTIONOBJECT_H */
