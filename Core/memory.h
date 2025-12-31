/*
 * Zex Programming Language
 * Core/memory.h - Memory management interface
 */

#ifndef ZEX_MEMORY_H
#define ZEX_MEMORY_H

#include "common.h"

/* Allocation functions */
void* zex_alloc(size_t size);
void* zex_realloc(void* ptr, size_t old_size, size_t new_size);
void  zex_free(void* ptr, size_t size);

/* Convenience macros */
#define ALLOCATE(type, count) \
    (type*)zex_alloc(sizeof(type) * (count))

#define FREE(type, ptr) \
    zex_free(ptr, sizeof(type))

#define GROW_ARRAY(type, ptr, old_count, new_count) \
    (type*)zex_realloc(ptr, sizeof(type) * (old_count), sizeof(type) * (new_count))

#define FREE_ARRAY(type, ptr, count) \
    zex_free(ptr, sizeof(type) * (count))

/* String duplication */
char* zex_strdup(const char* str);
char* zex_strndup(const char* str, size_t n);

#endif /* ZEX_MEMORY_H */
