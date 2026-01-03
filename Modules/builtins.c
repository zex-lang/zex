/*
 * Zex Programming Language
 * Modules/builtins.c - Built-in functions
 */

#include "builtins.h"
#include "error.h"
#include "object.h"
#include "stringobject.h"
#include "nullobject.h"
#include "exceptionobject.h"
#include "tupleobject.h"

static Value builtin_println(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    for (int i = 0; i < argc; i++) {
        if (i > 0) printf(" ");
        print_value(args[i]);
    }
    printf("\n");
    return NULL_VAL;
}

static Value builtin_type(VM* vm, int argc, Value* args) {
    if (argc != 1) {
        zex_error(ERROR_RUNTIME, 0, 0, 0, "type() takes exactly 1 argument (%d given)", argc);
        return NULL_VAL;
    }
    
    Value v = args[0];
    if (v.obj == NULL) return OBJ_VAL(vm->null_class);
    if (v.obj->klass != NULL) return OBJ_VAL(v.obj->klass);
    
    switch (v.obj->type) {
        case OBJ_INT:    return OBJ_VAL(vm->int_class);
        case OBJ_FLOAT:  return OBJ_VAL(vm->float_class);
        case OBJ_BOOL:   return OBJ_VAL(vm->bool_class);
        case OBJ_NULL:   return OBJ_VAL(vm->null_class);
        case OBJ_STRING: return OBJ_VAL(vm->string_class);
        default:         return NULL_VAL;
    }
}

typedef struct {
    const char* name;
    NativeFn function;
    int arity;
    bool has_rest;
} BuiltinDef;

static BuiltinDef builtins[] = {
    {"println", builtin_println, 0, true},
    {"type",    builtin_type,    1, false},
    {NULL, NULL, 0, false}
};

void register_builtins(VM* vm) {
    for (BuiltinDef* def = builtins; def->name != NULL; def++) {
        vm_define_native(vm, def->name, def->function, def->arity, def->has_rest);
    }
    
    /* Built-in type classes */
    table_set(&vm->globals, new_string_cstr("int"), OBJ_VAL(vm->int_class));
    table_set(&vm->globals, new_string_cstr("float"), OBJ_VAL(vm->float_class));
    table_set(&vm->globals, new_string_cstr("string"), OBJ_VAL(vm->string_class));
    table_set(&vm->globals, new_string_cstr("bool"), OBJ_VAL(vm->bool_class));
    table_set(&vm->globals, new_string_cstr("null"), OBJ_VAL(vm->null_class));
    table_set(&vm->globals, new_string_cstr("tuple"), OBJ_VAL(get_tuple_class()));
    
    /* Exception classes */
    table_set(&vm->globals, new_string_cstr("Exception"), OBJ_VAL(get_exception_class()));
    table_set(&vm->globals, new_string_cstr("ValueError"), OBJ_VAL(get_value_error_class()));
    table_set(&vm->globals, new_string_cstr("TypeError"), OBJ_VAL(get_type_error_class()));
    table_set(&vm->globals, new_string_cstr("IndexError"), OBJ_VAL(get_index_error_class()));
    table_set(&vm->globals, new_string_cstr("ZeroDivisionError"), OBJ_VAL(get_zero_division_error_class()));
    table_set(&vm->globals, new_string_cstr("AttributeError"), OBJ_VAL(get_attribute_error_class()));
    table_set(&vm->globals, new_string_cstr("NameError"), OBJ_VAL(get_name_error_class()));
    table_set(&vm->globals, new_string_cstr("RuntimeError"), OBJ_VAL(get_runtime_error_class()));
}
