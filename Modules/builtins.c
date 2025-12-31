/*
 * Zex Programming Language
 * Modules/builtins.c - Built-in functions implementation
 *
 * Python-style elegant registration of built-in functions.
 */

#include "builtins.h"
#include "object.h"
#include "stringobject.h"
#include "intobject.h"
#include "floatobject.h"
#include "boolobject.h"
#include "nullobject.h"
#include "classobject.h"

/*
 * println(args...)
 * Print values followed by a newline
 */
static Value builtin_println(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    
    for (int i = 0; i < argc; i++) {
        if (i > 0) printf(" ");
        print_value(args[i]);
    }
    printf("\n");
    
    return NULL_VAL;
}

/*
 * type(value)
 * Return the class of a value
 */
static Value builtin_type(VM* vm, int argc, Value* args) {
    if (argc != 1) {
        vm_runtime_error(vm, "type() takes exactly 1 argument (%d given)", argc);
        return NULL_VAL;
    }
    
    Value v = args[0];
    if (v.obj == NULL) {
        return OBJ_VAL(vm->null_class);
    }
    
    if (v.obj->klass != NULL) {
        return OBJ_VAL(v.obj->klass);
    }
    
    /* For objects without explicit class, return based on type */
    switch (v.obj->type) {
        case OBJ_INT:    return OBJ_VAL(vm->int_class);
        case OBJ_FLOAT:  return OBJ_VAL(vm->float_class);
        case OBJ_BOOL:   return OBJ_VAL(vm->bool_class);
        case OBJ_NULL:   return OBJ_VAL(vm->null_class);
        case OBJ_STRING: return OBJ_VAL(vm->string_class);
        default:         return NULL_VAL;
    }
}

/*
 * Built-in function definition table
 */
typedef struct {
    const char* name;
    NativeFn function;
    int arity;          /* -1 = variadic */
    const char* doc;    /* Documentation string */
} BuiltinDef;

static BuiltinDef builtins[] = {
    /* I/O */
    {"println", builtin_println, -1, "Print values followed by newline"},
    
    /* Introspection */
    {"type",    builtin_type,    1,  "Return the type/class of a value"},
    
    /* Sentinel */
    {NULL, NULL, 0, NULL}
};

void register_builtins(VM* vm) {
    /* Register native functions */
    for (BuiltinDef* def = builtins; def->name != NULL; def++) {
        vm_define_native(vm, def->name, def->function, def->arity);
    }
    
    /* Register built-in classes as globals for Python-style type conversion
     * This allows: int(3.14) -> 3, string(42) -> "42", etc.
     * Also allows: type(5) == int -> true
     */
    table_set(&vm->globals, new_string_cstr("int"), OBJ_VAL(vm->int_class));
    table_set(&vm->globals, new_string_cstr("float"), OBJ_VAL(vm->float_class));
    table_set(&vm->globals, new_string_cstr("string"), OBJ_VAL(vm->string_class));
    table_set(&vm->globals, new_string_cstr("bool"), OBJ_VAL(vm->bool_class));
    table_set(&vm->globals, new_string_cstr("null"), OBJ_VAL(vm->null_class));
}
