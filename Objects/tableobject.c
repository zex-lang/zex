/*
 * Zex Programming Language
 * Objects/tableobject.c - Hash table implementation
 */

#include "tableobject.h"
#include "stringobject.h"
#include "memory.h"

#define TABLE_MAX_LOAD 0.75

/* Simple helper to check if value is a "nil" value (obj pointer is NULL) */
static inline bool is_nil_value(Value v) {
    return v.obj == NULL;
}

/* Create a simple nil value (obj pointer is NULL) */
static inline Value nil_value(void) {
    return (Value){VAL_OBJ, NULL};
}

void table_init(Table* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void table_free(Table* table) {
    FREE_ARRAY(TableEntry, table->entries, table->capacity);
    table_init(table);
}

static TableEntry* find_entry(TableEntry* entries, int capacity, ObjString* key) {
    uint32_t index = key->hash & (capacity - 1);
    TableEntry* tombstone = NULL;
    
    for (;;) {
        TableEntry* entry = &entries[index];
        
        if (entry->key == NULL) {
            if (is_nil_value(entry->value)) {
                /* Empty entry */
                return tombstone != NULL ? tombstone : entry;
            } else {
                /* Tombstone */
                if (tombstone == NULL) tombstone = entry;
            }
        } else if (entry->key == key) {
            /* Found the key */
            return entry;
        }
        
        index = (index + 1) & (capacity - 1);
    }
}

static void adjust_capacity(Table* table, int capacity) {
    TableEntry* entries = ALLOCATE(TableEntry, capacity);
    
    for (int i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = nil_value();
    }
    
    /* Rehash existing entries */
    table->count = 0;
    for (int i = 0; i < table->capacity; i++) {
        TableEntry* entry = &table->entries[i];
        if (entry->key == NULL) continue;
        
        TableEntry* dest = find_entry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }
    
    FREE_ARRAY(TableEntry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

bool table_get(Table* table, ObjString* key, Value* value) {
    if (table->count == 0) return false;
    
    TableEntry* entry = find_entry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;
    
    *value = entry->value;
    return true;
}

bool table_set(Table* table, ObjString* key, Value value) {
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjust_capacity(table, capacity);
    }
    
    TableEntry* entry = find_entry(table->entries, table->capacity, key);
    bool is_new_key = entry->key == NULL;
    
    /* Only increment count for truly new entries (not tombstones) */
    if (is_new_key && is_nil_value(entry->value)) {
        table->count++;
    }
    
    entry->key = key;
    entry->value = value;
    return is_new_key;
}

bool table_delete(Table* table, ObjString* key) {
    if (table->count == 0) return false;
    
    TableEntry* entry = find_entry(table->entries, table->capacity, key);
    if (entry->key == NULL) return false;
    
    /* Place a tombstone - use a non-nil marker */
    entry->key = NULL;
    /* For tombstone, we set obj to a non-null value (we use key as marker) */
    entry->value = (Value){VAL_OBJ, (Obj*)0x1};  /* Sentinel tombstone */
    return true;
}

void table_add_all(Table* from, Table* to) {
    for (int i = 0; i < from->capacity; i++) {
        TableEntry* entry = &from->entries[i];
        if (entry->key != NULL) {
            table_set(to, entry->key, entry->value);
        }
    }
}

ObjString* table_find_string(Table* table, const char* chars, int length, uint32_t hash) {
    if (table->count == 0) return NULL;
    
    uint32_t index = hash & (table->capacity - 1);
    
    for (;;) {
        TableEntry* entry = &table->entries[index];
        
        if (entry->key == NULL) {
            /* Stop if we find an empty non-tombstone entry */
            if (is_nil_value(entry->value)) return NULL;
        } else if (entry->key->length == length &&
                   entry->key->hash == hash &&
                   memcmp(entry->key->chars, chars, length) == 0) {
            /* Found it */
            return entry->key;
        }
        
        index = (index + 1) & (table->capacity - 1);
    }
}

void table_mark(Table* table) {
    /* TODO: Implement when GC is ready */
    UNUSED(table);
}

void table_remove_white(Table* table) {
    /* TODO: Implement when GC is ready */
    UNUSED(table);
}
