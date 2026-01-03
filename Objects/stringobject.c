/*
 * Zex Programming Language
 * Objects/stringobject.c - String object implementation
 */

#include "stringobject.h"
#include "tableobject.h"
#include "classobject.h"
#include "funcobject.h"
#include "intobject.h"
#include "boolobject.h"
#include "nullobject.h"
#include "arrayobject.h"
#include "memory.h"
#include "utf8.h"
#include "vm.h"

/* String interning table */
Table string_intern_table;

static ObjClass* string_class = NULL;

/* A simple null value for interning - just uses null pointer */
static Value INTERN_NULL = {VAL_OBJ, NULL};

void init_string_intern(void) {
    table_init(&string_intern_table);
}

void free_string_intern(void) {
    table_free(&string_intern_table);
}

static uint32_t hash_string(const char* key, int length) {
    /* FNV-1a hash */
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

ObjString* new_string(const char* chars, int length) {
    uint32_t hash = hash_string(chars, length);
    
    /* Check for interned string */
    ObjString* interned = table_find_string(&string_intern_table, chars, length, hash);
    if (interned != NULL) return interned;
    
    /* Allocate new string */
    ObjString* string = (ObjString*)allocate_object(
        sizeof(ObjString) + length + 1,
        OBJ_STRING,
        string_class
    );
    
    string->length = length;
    string->char_count = utf8_char_count(chars, length);
    string->hash = hash;
    memcpy(string->chars, chars, length);
    string->chars[length] = '\0';
    
    /* Intern the string - use simple null value to avoid circular dependency */
    table_set(&string_intern_table, string, INTERN_NULL);
    
    return string;
}

ObjString* new_string_cstr(const char* cstr) {
    return new_string(cstr, (int)strlen(cstr));
}

ObjString* string_concat(ObjString* a, ObjString* b) {
    int length = a->length + b->length;
    char* chars = ALLOCATE(char, length + 1);
    
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';
    
    ObjString* result = new_string(chars, length);
    FREE_ARRAY(char, chars, length + 1);
    
    return result;
}

ObjString* string_char_at(ObjString* s, int index) {
    /* Handle negative index */
    if (index < 0) {
        index = s->char_count + index;
    }
    
    if (index < 0 || index >= s->char_count) {
        return NULL;
    }
    
    /* Find byte offset for character index */
    int offset = utf8_byte_offset(s->chars, s->length, index);
    if (offset < 0) return NULL;
    
    /* Get character length */
    int char_len = utf8_char_len(s->chars + offset);
    
    /* Create new single-character string */
    return new_string(s->chars + offset, char_len);
}

/* Native string methods */

static Value string_len(VM* vm, int argc, Value* args) {
    (void)vm; (void)argc;
    ObjString* s = AS_STRING(args[0]);
    return INT_VAL(s->char_count);
}

static Value string_bytes(VM* vm, int argc, Value* args) {
    (void)vm; (void)argc;
    ObjString* s = AS_STRING(args[0]);
    return INT_VAL(s->length);
}

static Value string_starts_with(VM* vm, int argc, Value* args) {
    (void)vm; (void)argc;
    ObjString* s = AS_STRING(args[0]);
    if (!IS_STRING(args[1])) return BOOL_VAL(false);
    ObjString* prefix = AS_STRING(args[1]);
    
    if (prefix->length > s->length) return BOOL_VAL(false);
    return BOOL_VAL(memcmp(s->chars, prefix->chars, prefix->length) == 0);
}

static Value string_ends_with(VM* vm, int argc, Value* args) {
    (void)vm; (void)argc;
    ObjString* s = AS_STRING(args[0]);
    if (!IS_STRING(args[1])) return BOOL_VAL(false);
    ObjString* suffix = AS_STRING(args[1]);
    
    if (suffix->length > s->length) return BOOL_VAL(false);
    return BOOL_VAL(memcmp(s->chars + s->length - suffix->length, 
                           suffix->chars, suffix->length) == 0);
}

static Value string_contains(VM* vm, int argc, Value* args) {
    (void)vm; (void)argc;
    ObjString* s = AS_STRING(args[0]);
    if (!IS_STRING(args[1])) return BOOL_VAL(false);
    ObjString* needle = AS_STRING(args[1]);
    
    if (needle->length == 0) return BOOL_VAL(true);
    if (needle->length > s->length) return BOOL_VAL(false);
    
    for (int i = 0; i <= s->length - needle->length; i++) {
        if (memcmp(s->chars + i, needle->chars, needle->length) == 0) {
            return BOOL_VAL(true);
        }
    }
    return BOOL_VAL(false);
}

static Value string_upper(VM* vm, int argc, Value* args) {
    (void)vm; (void)argc;
    ObjString* s = AS_STRING(args[0]);
    int len = s->length;
    char* result = ALLOCATE(char, len + 1);
    
    for (int i = 0; i < len; i++) {
        char c = s->chars[i];
        if (c >= 'a' && c <= 'z') {
            result[i] = c - 32;
        } else {
            result[i] = c;
        }
    }
    result[len] = '\0';
    
    ObjString* str = new_string(result, len);
    FREE_ARRAY(char, result, len + 1);
    return OBJ_VAL(str);
}

static Value string_lower(VM* vm, int argc, Value* args) {
    (void)vm; (void)argc;
    ObjString* s = AS_STRING(args[0]);
    int len = s->length;
    char* result = ALLOCATE(char, len + 1);
    
    for (int i = 0; i < len; i++) {
        char c = s->chars[i];
        if (c >= 'A' && c <= 'Z') {
            result[i] = c + 32;
        } else {
            result[i] = c;
        }
    }
    result[len] = '\0';
    
    ObjString* str = new_string(result, len);
    FREE_ARRAY(char, result, len + 1);
    return OBJ_VAL(str);
}

static Value string_trim(VM* vm, int argc, Value* args) {
    (void)vm; (void)argc;
    ObjString* s = AS_STRING(args[0]);
    
    int start = 0;
    int end = s->length;
    
    /* Find first non-whitespace */
    while (start < end && (s->chars[start] == ' ' || s->chars[start] == '\t' ||
                           s->chars[start] == '\n' || s->chars[start] == '\r')) {
        start++;
    }
    
    /* Find last non-whitespace */
    while (end > start && (s->chars[end - 1] == ' ' || s->chars[end - 1] == '\t' ||
                           s->chars[end - 1] == '\n' || s->chars[end - 1] == '\r')) {
        end--;
    }
    
    return OBJ_VAL(new_string(s->chars + start, end - start));
}

static Value string_split(VM* vm, int argc, Value* args) {
    (void)vm; (void)argc;
    ObjString* s = AS_STRING(args[0]);
    if (!IS_STRING(args[1])) return NULL_VAL;
    ObjString* sep = AS_STRING(args[1]);
    
    ObjArray* arr = new_array();
    
    if (sep->length == 0) {
        /* Split into characters */
        for (int i = 0; i < s->char_count; i++) {
            ObjString* ch = string_char_at(s, i);
            if (ch) array_push(arr, OBJ_VAL(ch));
        }
        return OBJ_VAL(arr);
    }
    
    int start = 0;
    for (int i = 0; i <= s->length - sep->length; i++) {
        if (memcmp(s->chars + i, sep->chars, sep->length) == 0) {
            array_push(arr, OBJ_VAL(new_string(s->chars + start, i - start)));
            i += sep->length - 1;
            start = i + 1;
        }
    }
    
    /* Add remaining part */
    array_push(arr, OBJ_VAL(new_string(s->chars + start, s->length - start)));
    
    return OBJ_VAL(arr);
}

static Value string_slice(VM* vm, int argc, Value* argv) {
    (void)vm;
    ObjString* s = AS_STRING(argv[0]);
    
    if (argc < 2 || !IS_INT(argv[1])) return NULL_VAL;
    int start = (int)AS_INT(argv[1]);
    int end = s->char_count;
    
    if (argc >= 3 && IS_INT(argv[2])) {
        end = (int)AS_INT(argv[2]);
    }
    
    /* Handle negative indices */
    if (start < 0) start = s->char_count + start;
    if (end < 0) end = s->char_count + end;
    
    /* Clamp */
    if (start < 0) start = 0;
    if (end > s->char_count) end = s->char_count;
    if (start >= end) return OBJ_VAL(new_string("", 0));
    
    /* Convert char indices to byte offsets */
    int byte_start = utf8_byte_offset(s->chars, s->length, start);
    int byte_end = utf8_byte_offset(s->chars, s->length, end);
    
    if (byte_start < 0) byte_start = 0;
    if (byte_end < 0) byte_end = s->length;
    
    return OBJ_VAL(new_string(s->chars + byte_start, byte_end - byte_start));
}

static Value string_replace(VM* vm, int argc, Value* args) {
    (void)vm; (void)argc;
    ObjString* s = AS_STRING(args[0]);
    if (!IS_STRING(args[1]) || !IS_STRING(args[2])) return OBJ_VAL(s);
    ObjString* old = AS_STRING(args[1]);
    ObjString* newstr = AS_STRING(args[2]);
    
    if (old->length == 0) return OBJ_VAL(s);
    
    /* Count occurrences */
    int count = 0;
    for (int i = 0; i <= s->length - old->length; i++) {
        if (memcmp(s->chars + i, old->chars, old->length) == 0) {
            count++;
            i += old->length - 1;
        }
    }
    
    if (count == 0) return OBJ_VAL(s);
    
    /* Calculate new length */
    int new_len = s->length + (newstr->length - old->length) * count;
    char* result = ALLOCATE(char, new_len + 1);
    
    int dst = 0;
    for (int i = 0; i < s->length; ) {
        if (i <= s->length - old->length && 
            memcmp(s->chars + i, old->chars, old->length) == 0) {
            memcpy(result + dst, newstr->chars, newstr->length);
            dst += newstr->length;
            i += old->length;
        } else {
            result[dst++] = s->chars[i++];
        }
    }
    result[new_len] = '\0';
    
    ObjString* str = new_string(result, new_len);
    FREE_ARRAY(char, result, new_len + 1);
    return OBJ_VAL(str);
}

/* str.forEach(fn) -> iterates over each character */
static Value string_foreach(VM* vm, int argc, Value* args) {
    if (argc != 2) return NULL_VAL;
    ObjString* s = AS_STRING(args[0]);
    Value fn = args[1];
    
    int offset = 0;
    while (offset < s->length) {
        int char_len = utf8_char_len(s->chars + offset);
        ObjString* ch = new_string(s->chars + offset, char_len);
        
        Value call_args[1] = { OBJ_VAL(ch) };
        Value result;
        if (!vm_call_value(vm, fn, 1, call_args, &result)) {
            return NULL_VAL;
        }
        
        offset += char_len;
    }
    
    return NULL_VAL;
}

/* str.map(fn) -> new string with mapped characters */
static Value string_map(VM* vm, int argc, Value* args) {
    if (argc != 2) return NULL_VAL;
    ObjString* s = AS_STRING(args[0]);
    Value fn = args[1];
    
    int capacity = s->length; /* Initial guess */
    int count = 0;
    char* result = ALLOCATE(char, capacity + 1);
    
    int offset = 0;
    while (offset < s->length) {
        int char_len = utf8_char_len(s->chars + offset);
        ObjString* ch = new_string(s->chars + offset, char_len);
        
        Value call_args[1] = { OBJ_VAL(ch) };
        Value mapped;
        if (!vm_call_value(vm, fn, 1, call_args, &mapped)) {
            FREE_ARRAY(char, result, capacity + 1);
            return NULL_VAL;
        }
        
        if (IS_STRING(mapped)) {
            ObjString* map_str = AS_STRING(mapped);
            if (count + map_str->length > capacity) {
                int old_capacity = capacity;
                capacity = GROW_CAPACITY(capacity);
                if (capacity < count + map_str->length) capacity = count + map_str->length + 16;
                result = GROW_ARRAY(char, result, old_capacity + 1, capacity + 1);
            }
            memcpy(result + count, map_str->chars, map_str->length);
            count += map_str->length;
        }
        
        offset += char_len;
    }
    
    result[count] = '\0';
    ObjString* new_str = new_string(result, count);
    FREE_ARRAY(char, result, capacity + 1);
    
    return OBJ_VAL(new_str);
}

/* str.filter(fn) -> new string with kept characters */
static Value string_filter(VM* vm, int argc, Value* args) {
    if (argc != 2) return NULL_VAL;
    ObjString* s = AS_STRING(args[0]);
    Value fn = args[1];
    
    int capacity = s->length;
    int count = 0;
    char* result = ALLOCATE(char, capacity + 1);
    
    int offset = 0;
    while (offset < s->length) {
        int char_len = utf8_char_len(s->chars + offset);
        ObjString* ch = new_string(s->chars + offset, char_len);
        
        Value call_args[1] = { OBJ_VAL(ch) };
        Value keep;
        if (!vm_call_value(vm, fn, 1, call_args, &keep)) {
            FREE_ARRAY(char, result, capacity + 1);
            return NULL_VAL;
        }
        
        if (is_truthy(keep)) {
            memcpy(result + count, s->chars + offset, char_len);
            count += char_len;
        }
        
        offset += char_len;
    }
    
    result[count] = '\0';
    ObjString* new_str = new_string(result, count);
    FREE_ARRAY(char, result, capacity + 1);
    
    return OBJ_VAL(new_str);
}

/* str.reduce(fn, init) */
static Value string_reduce(VM* vm, int argc, Value* args) {
    if (argc != 3) return NULL_VAL;
    ObjString* s = AS_STRING(args[0]);
    Value fn = args[1];
    Value acc = args[2];
    
    int offset = 0;
    while (offset < s->length) {
        int char_len = utf8_char_len(s->chars + offset);
        ObjString* ch = new_string(s->chars + offset, char_len);
        
        Value call_args[2] = { acc, OBJ_VAL(ch) };
        if (!vm_call_value(vm, fn, 2, call_args, &acc)) {
            return NULL_VAL;
        }
        
        offset += char_len;
    }
    
    return acc;
}

/* str.find(fn) -> char string or null */
static Value string_find(VM* vm, int argc, Value* args) {
    if (argc != 2) return NULL_VAL;
    ObjString* s = AS_STRING(args[0]);
    Value fn = args[1];
    
    int offset = 0;
    while (offset < s->length) {
        int char_len = utf8_char_len(s->chars + offset);
        ObjString* ch = new_string(s->chars + offset, char_len);
        
        Value call_args[1] = { OBJ_VAL(ch) };
        Value found;
        if (!vm_call_value(vm, fn, 1, call_args, &found)) {
            return NULL_VAL;
        }
        
        if (is_truthy(found)) {
            return OBJ_VAL(ch);
        }
        
        offset += char_len;
    }
    
    return NULL_VAL;
}

/* str.some(fn) -> bool */
static Value string_some(VM* vm, int argc, Value* args) {
    if (argc != 2) return NULL_VAL;
    ObjString* s = AS_STRING(args[0]);
    Value fn = args[1];
    
    int offset = 0;
    while (offset < s->length) {
        int char_len = utf8_char_len(s->chars + offset);
        ObjString* ch = new_string(s->chars + offset, char_len);
        
        Value call_args[1] = { OBJ_VAL(ch) };
        Value result;
        if (!vm_call_value(vm, fn, 1, call_args, &result)) {
            return NULL_VAL;
        }
        
        if (is_truthy(result)) {
            return BOOL_VAL(true);
        }
        
        offset += char_len;
    }
    
    return BOOL_VAL(false);
}

/* str.every(fn) -> bool */
static Value string_every(VM* vm, int argc, Value* args) {
    if (argc != 2) return NULL_VAL;
    ObjString* s = AS_STRING(args[0]);
    Value fn = args[1];
    
    int offset = 0;
    while (offset < s->length) {
        int char_len = utf8_char_len(s->chars + offset);
        ObjString* ch = new_string(s->chars + offset, char_len);
        
        Value call_args[1] = { OBJ_VAL(ch) };
        Value result;
        if (!vm_call_value(vm, fn, 1, call_args, &result)) {
            return NULL_VAL;
        }
        
        if (!is_truthy(result)) {
            return BOOL_VAL(false);
        }
        
        offset += char_len;
    }
    
    return BOOL_VAL(true);
}

/* Method registration table */
typedef struct {
    const char* name;
    NativeFn function;
    int arity;
    bool has_rest;
} StringMethodDef;

static StringMethodDef string_methods[] = {
    {"len",        string_len,         1, false},
    {"bytes",      string_bytes,       1, false},
    {"startsWith", string_starts_with, 2, false},
    {"endsWith",   string_ends_with,   2, false},
    {"contains",   string_contains,    2, false},
    {"upper",      string_upper,       1, false},
    {"lower",      string_lower,       1, false},
    {"trim",       string_trim,        1, false},
    {"split",      string_split,       2, false},
    {"slice",      string_slice,       2, true},  /* arity=2 (self, start), has_rest for optional end */
    {"replace",    string_replace,     3, false},
    {"forEach",    string_foreach,     2, false},
    {"map",        string_map,         2, false},
    {"filter",     string_filter,      2, false},
    {"reduce",     string_reduce,      3, false},
    {"find",       string_find,        2, false},
    {"some",       string_some,        2, false},
    {"every",      string_every,       2, false},
    {NULL, NULL, 0, false}
};

ObjClass* get_string_class(void) {
    return string_class;
}

void init_string_class(void) {
    string_class = new_class(new_string_cstr("string"));
    
    /* Register all string methods */
    for (StringMethodDef* def = string_methods; def->name != NULL; def++) {
        ObjNative* native = new_native(def->function, def->arity, def->has_rest, def->name);
        table_set(&string_class->methods, new_string_cstr(def->name), OBJ_VAL(native));
    }
}
