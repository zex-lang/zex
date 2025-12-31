/*
 * Zex Programming Language
 * Objects/stringobject.c - String object implementation
 */

#include "stringobject.h"
#include "tableobject.h"
#include "classobject.h"
#include "memory.h"

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

ObjClass* get_string_class(void) {
    return string_class;
}

void init_string_class(void) {
    string_class = new_class(new_string_cstr("string"));
}
