/*
 * Zex Programming Language
 * Objects/tableobject.h - Hash table for properties and globals
 */

#ifndef ZEX_TABLEOBJECT_H
#define ZEX_TABLEOBJECT_H

#include "object.h"

/* Hash table entry */
typedef struct {
    ObjString* key;
    Value value;
} TableEntry;

/* Hash table */
typedef struct {
    int count;
    int capacity;
    TableEntry* entries;
} Table;

/* Initialize an empty table */
void table_init(Table* table);

/* Free table memory */
void table_free(Table* table);

/* Get value by key. Returns true if found, false otherwise */
bool table_get(Table* table, ObjString* key, Value* value);

/* Set value by key. Returns true if new key, false if update */
bool table_set(Table* table, ObjString* key, Value value);

/* Delete entry by key. Returns true if deleted, false if not found */
bool table_delete(Table* table, ObjString* key);

/* Copy all entries from one table to another */
void table_add_all(Table* from, Table* to);

/* Find string in table (for string interning) */
ObjString* table_find_string(Table* table, const char* chars, int length, uint32_t hash);

/* Mark all objects in table for GC */
void table_mark(Table* table);

/* Remove unmarked entries (weak references) */
void table_remove_white(Table* table);

#endif /* ZEX_TABLEOBJECT_H */
