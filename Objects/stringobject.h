/*
 * Zex Programming Language
 * Objects/stringobject.h - String object definition
 */

#ifndef ZEX_STRINGOBJECT_H
#define ZEX_STRINGOBJECT_H

#include "object.h"

/* Forward declare Table */
struct Table;

/* String object - immutable, UTF-8 aware */
struct ObjString {
    Obj obj;
    int length;         /* Byte length */
    int char_count;     /* UTF-8 character count (cached) */
    uint32_t hash;
    char chars[];       /* Flexible array member for string data */
};

/* Create a new string (copies the characters) */
ObjString* new_string(const char* chars, int length);

/* Create string from C string */
ObjString* new_string_cstr(const char* cstr);

/* Concatenate two strings */
ObjString* string_concat(ObjString* a, ObjString* b);

/* Get character at index (returns new single-char string) */
ObjString* string_char_at(ObjString* s, int index);

/* Initialize string interning */
void init_string_intern(void);

/* Free string intern table */
void free_string_intern(void);

/* Get/init string class */
ObjClass* get_string_class(void);
void init_string_class(void);

#endif /* ZEX_STRINGOBJECT_H */
