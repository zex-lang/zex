/*
 * Zex Programming Language
 * Objects/stringobject.h - String object definition
 */

#ifndef ZEX_STRINGOBJECT_H
#define ZEX_STRINGOBJECT_H

#include "object.h"

/* Forward declare Table */
struct Table;

/* String object - immutable */
struct ObjString {
    Obj obj;
    int length;
    uint32_t hash;
    char chars[];       /* Flexible array member for string data */
};

/* Create a new string (copies the characters) */
ObjString* new_string(const char* chars, int length);

/* Create string from C string */
ObjString* new_string_cstr(const char* cstr);

/* Concatenate two strings */
ObjString* string_concat(ObjString* a, ObjString* b);

/* Initialize string interning */
void init_string_intern(void);

/* Free string intern table */
void free_string_intern(void);

#endif /* ZEX_STRINGOBJECT_H */
