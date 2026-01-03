/*
 * Zex Programming Language
 * Objects/arrayobject.c - Array object implementation
 */

#include "arrayobject.h"
#include "stringobject.h"
#include "intobject.h"
#include "boolobject.h"
#include "nullobject.h"
#include "classobject.h"
#include "funcobject.h"
#include "memory.h"
#include "vm.h"

/* Array class singleton */
static ObjClass* array_class = NULL;

/* Initial capacity for arrays */
#define ARRAY_INITIAL_CAPACITY 8

ObjArray* new_array(void) {
    return new_array_with_capacity(0);
}

ObjArray* new_array_with_capacity(int capacity) {
    ObjArray* arr = ALLOCATE_OBJ(ObjArray, OBJ_ARRAY, array_class);
    arr->count = 0;
    arr->capacity = capacity;
    if (capacity > 0) {
        arr->items = ALLOCATE(Value, capacity);
    } else {
        arr->items = NULL;
    }
    return arr;
}

static void ensure_capacity(ObjArray* arr, int needed) {
    if (arr->capacity >= needed) return;
    
    int new_capacity = arr->capacity < ARRAY_INITIAL_CAPACITY 
                       ? ARRAY_INITIAL_CAPACITY 
                       : arr->capacity * 2;
    while (new_capacity < needed) {
        new_capacity *= 2;
    }
    
    arr->items = GROW_ARRAY(Value, arr->items, arr->capacity, new_capacity);
    arr->capacity = new_capacity;
}

void array_push(ObjArray* arr, Value value) {
    ensure_capacity(arr, arr->count + 1);
    arr->items[arr->count++] = value;
}

Value array_pop(ObjArray* arr) {
    if (arr->count == 0) {
        return NULL_VAL;
    }
    return arr->items[--arr->count];
}

Value array_get(ObjArray* arr, int index) {
    if (index < 0 || index >= arr->count) {
        return NULL_VAL;
    }
    return arr->items[index];
}

void array_set(ObjArray* arr, int index, Value value) {
    if (index >= 0 && index < arr->count) {
        arr->items[index] = value;
    }
}

int array_len(ObjArray* arr) {
    return arr->count;
}

bool array_contains(ObjArray* arr, Value value) {
    for (int i = 0; i < arr->count; i++) {
        if (values_equal(arr->items[i], value)) {
            return true;
        }
    }
    return false;
}

void array_clear(ObjArray* arr) {
    arr->count = 0;
}

void array_reverse(ObjArray* arr) {
    int left = 0;
    int right = arr->count - 1;
    while (left < right) {
        Value temp = arr->items[left];
        arr->items[left] = arr->items[right];
        arr->items[right] = temp;
        left++;
        right--;
    }
}

/* Native Array Methods */

/* arr.len() -> int */
static Value array_method_len(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    if (argc != 1) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    return INT_VAL(arr->count);
}

/* arr.push(value) -> null */
static Value array_method_push(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    if (argc != 2) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    array_push(arr, args[1]);
    return NULL_VAL;
}

/* arr.pop() -> value */
static Value array_method_pop(VM* vm, int argc, Value* args) {
    UNUSED(vm);  
    if (argc != 1) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    return array_pop(arr);
}

/* arr.first() -> value */
static Value array_method_first(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    if (argc != 1) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    if (arr->count == 0) return NULL_VAL;
    return arr->items[0];
}

/* arr.last() -> value */
static Value array_method_last(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    if (argc != 1) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    if (arr->count == 0) return NULL_VAL;
    return arr->items[arr->count - 1];
}

/* arr.contains(value) -> bool */
static Value array_method_contains(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    if (argc != 2) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    return BOOL_VAL(array_contains(arr, args[1]));
}

/* arr.empty() -> bool */
static Value array_method_empty(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    if (argc != 1) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    return BOOL_VAL(arr->count == 0);
}

/* arr.clear() -> null */
static Value array_method_clear(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    if (argc != 1) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    array_clear(arr);
    return NULL_VAL;
}

/* arr.reverse() -> null */
static Value array_method_reverse(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    if (argc != 1) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    array_reverse(arr);
    return NULL_VAL;
}

/* arr.join(separator) -> string */
static Value array_method_join(VM* vm, int argc, Value* args) {
    UNUSED(vm);
    if (argc != 2 || !IS_STRING(args[1])) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    ObjString* sep = AS_STRING(args[1]);
    
    if (arr->count == 0) {
        return OBJ_VAL(new_string_cstr(""));
    }
    
    /* Calculate total length */
    int total_len = 0;
    ObjString** strs = ALLOCATE(ObjString*, arr->count);
    
    for (int i = 0; i < arr->count; i++) {
        strs[i] = value_to_string(arr->items[i]);
        total_len += strs[i]->length;
        if (i > 0) total_len += sep->length;
    }
    
    /* Build result string */
    char* buffer = ALLOCATE(char, total_len + 1);
    char* p = buffer;
    
    for (int i = 0; i < arr->count; i++) {
        if (i > 0) {
            memcpy(p, sep->chars, sep->length);
            p += sep->length;
        }
        memcpy(p, strs[i]->chars, strs[i]->length);
        p += strs[i]->length;
    }
    *p = '\0';
    
    ObjString* result = new_string(buffer, total_len);
    
    FREE_ARRAY(char, buffer, total_len + 1);
    FREE_ARRAY(ObjString*, strs, arr->count);
    
    return OBJ_VAL(result);
}

/* arr.map(fn) -> new array with fn(item) for each item */
static Value array_method_map(VM* vm, int argc, Value* args) {
    if (argc != 2) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    Value fn = args[1];
    
    ObjArray* result = new_array_with_capacity(arr->count);
    
    for (int i = 0; i < arr->count; i++) {
        Value call_args[1] = { arr->items[i] };
        Value mapped;
        if (!vm_call_value(vm, fn, 1, call_args, &mapped)) {
            return NULL_VAL;
        }
        array_push(result, mapped);
    }
    
    return OBJ_VAL(result);
}

/* arr.filter(fn) -> new array with items where fn(item) is truthy */
static Value array_method_filter(VM* vm, int argc, Value* args) {
    if (argc != 2) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    Value fn = args[1];
    
    ObjArray* result = new_array();
    
    for (int i = 0; i < arr->count; i++) {
        Value call_args[1] = { arr->items[i] };
        Value keep;
        if (!vm_call_value(vm, fn, 1, call_args, &keep)) {
            return NULL_VAL;
        }
        if (is_truthy(keep)) {
            array_push(result, arr->items[i]);
        }
    }
    
    return OBJ_VAL(result);
}

/* arr.reduce(fn, initial) -> fn(acc, item) for each item */
static Value array_method_reduce(VM* vm, int argc, Value* args) {
    if (argc != 3) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    Value fn = args[1];
    Value acc = args[2];
    
    for (int i = 0; i < arr->count; i++) {
        Value call_args[2] = { acc, arr->items[i] };
        if (!vm_call_value(vm, fn, 2, call_args, &acc)) {
            return NULL_VAL;
        }
    }
    
    return acc;
}

/* arr.forEach(fn) -> null, calls fn(item) for each item */
static Value array_method_foreach(VM* vm, int argc, Value* args) {
    if (argc != 2) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    Value fn = args[1];
    
    for (int i = 0; i < arr->count; i++) {
        Value call_args[1] = { arr->items[i] };
        Value result;
        if (!vm_call_value(vm, fn, 1, call_args, &result)) {
            return NULL_VAL;
        }
    }
    
    return NULL_VAL;
}

/* arr.find(fn) -> first item where fn(item) is truthy, or null */
static Value array_method_find(VM* vm, int argc, Value* args) {
    if (argc != 2) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    Value fn = args[1];
    
    for (int i = 0; i < arr->count; i++) {
        Value call_args[1] = { arr->items[i] };
        Value found;
        if (!vm_call_value(vm, fn, 1, call_args, &found)) {
            return NULL_VAL;
        }
        if (is_truthy(found)) {
            return arr->items[i];
        }
    }
    
    return NULL_VAL;
}

/* arr.some(fn) -> true if fn(item) is truthy for any item */
static Value array_method_some(VM* vm, int argc, Value* args) {
    if (argc != 2) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    Value fn = args[1];
    
    for (int i = 0; i < arr->count; i++) {
        Value call_args[1] = { arr->items[i] };
        Value result;
        if (!vm_call_value(vm, fn, 1, call_args, &result)) {
            return NULL_VAL;
        }
        if (is_truthy(result)) {
            return BOOL_VAL(true);
        }
    }
    
    return BOOL_VAL(false);
}

/* arr.every(fn) -> true if fn(item) is truthy for all items */
static Value array_method_every(VM* vm, int argc, Value* args) {
    if (argc != 2) return NULL_VAL;
    
    ObjArray* arr = AS_ARRAY(args[0]);
    Value fn = args[1];
    
    for (int i = 0; i < arr->count; i++) {
        Value call_args[1] = { arr->items[i] };
        Value result;
        if (!vm_call_value(vm, fn, 1, call_args, &result)) {
            return NULL_VAL;
        }
        if (!is_truthy(result)) {
            return BOOL_VAL(false);
        }
    }
    
    return BOOL_VAL(true);
}

/* Method registration table */
typedef struct {
    const char* name;
    NativeFn function;
    int arity;  /* Including self; -1 = variadic */
} ArrayMethodDef;

static ArrayMethodDef array_methods[] = {
    {"len",      array_method_len,      1},
    {"push",     array_method_push,     2},
    {"pop",      array_method_pop,      1},
    {"first",    array_method_first,    1},
    {"last",     array_method_last,     1},
    {"contains", array_method_contains, 2},
    {"empty",    array_method_empty,    1},
    {"clear",    array_method_clear,    1},
    {"reverse",  array_method_reverse,  1},
    {"join",     array_method_join,     2},
    {"map",      array_method_map,      2},
    {"filter",   array_method_filter,   2},
    {"reduce",   array_method_reduce,   3},
    {"forEach",  array_method_foreach,  2},
    {"find",     array_method_find,     2},
    {"some",     array_method_some,     2},
    {"every",    array_method_every,    2},
    {NULL, NULL, 0}
};

ObjClass* get_array_class(void) {
    return array_class;
}

void init_array_class(void) {
    array_class = new_class(new_string_cstr("array"));
    
    /* Register all array methods as native functions in the class methods table */
    for (ArrayMethodDef* def = array_methods; def->name != NULL; def++) {
        ObjNative* native = new_native(def->function, def->arity, false, def->name);
        table_set(&array_class->methods, new_string_cstr(def->name), OBJ_VAL(native));
    }
}
