/*
 * Zex Programming Language
 * Objects/exceptionobject.c - Exception object implementation
 */

#include "exceptionobject.h"
#include "classobject.h"
#include "tableobject.h"
#include "memory.h"

/* Built-in exception classes */
static ObjClass* exception_class = NULL;
static ObjClass* value_error_class = NULL;
static ObjClass* type_error_class = NULL;
static ObjClass* index_error_class = NULL;
static ObjClass* zero_division_error_class = NULL;
static ObjClass* attribute_error_class = NULL;
static ObjClass* name_error_class = NULL;
static ObjClass* runtime_error_class = NULL;

/* Helper to create an exception class with optional parent */
static ObjClass* make_exception_class(const char* name, ObjClass* parent) {
    ObjClass* klass = new_class(new_string_cstr(name));
    klass->superclass = parent;
    
    /* Copy parent methods if inheriting */
    if (parent != NULL) {
        table_add_all(&parent->methods, &klass->methods);
    }
    
    return klass;
}

void init_exception_classes(void) {
    /* Base Exception class */
    exception_class = make_exception_class("Exception", NULL);
    
    /* Child exception classes - all inherit from Exception */
    value_error_class = make_exception_class("ValueError", exception_class);
    type_error_class = make_exception_class("TypeError", exception_class);
    index_error_class = make_exception_class("IndexError", exception_class);
    zero_division_error_class = make_exception_class("ZeroDivisionError", exception_class);
    attribute_error_class = make_exception_class("AttributeError", exception_class);
    name_error_class = make_exception_class("NameError", exception_class);
    runtime_error_class = make_exception_class("RuntimeError", exception_class);
}

ObjClass* get_exception_class(void) {
    return exception_class;
}

ObjClass* get_value_error_class(void) {
    return value_error_class;
}

ObjClass* get_type_error_class(void) {
    return type_error_class;
}

ObjClass* get_index_error_class(void) {
    return index_error_class;
}

ObjClass* get_zero_division_error_class(void) {
    return zero_division_error_class;
}

ObjClass* get_attribute_error_class(void) {
    return attribute_error_class;
}

ObjClass* get_name_error_class(void) {
    return name_error_class;
}

ObjClass* get_runtime_error_class(void) {
    return runtime_error_class;
}

ObjException* new_exception(ObjClass* exc_class, ObjString* message,
                             int line, int column, int span) {
    ObjException* exception = ALLOCATE_OBJ(ObjException, OBJ_EXCEPTION, exc_class);
    exception->message = message;
    exception->exception_class = exc_class;
    exception->line = line;
    exception->column = column;
    exception->span = span;
    exception->traceback = NULL;
    return exception;
}

ObjException* new_exception_cstr(ObjClass* exc_class, const char* message,
                                  int line, int column, int span) {
    return new_exception(exc_class, new_string_cstr(message), line, column, span);
}

bool exception_is_type(Value exception, ObjClass* target_class) {
    if (!IS_EXCEPTION(exception)) return false;
    
    ObjException* exc = AS_EXCEPTION(exception);
    ObjClass* exc_class = exc->exception_class;
    
    /* Walk up the inheritance chain */
    while (exc_class != NULL) {
        if (exc_class == target_class) {
            return true;
        }
        exc_class = exc_class->superclass;
    }
    
    return false;
}
