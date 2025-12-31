/*
 * Zex Programming Language
 * Core/memory.c - Memory management implementation
 */

#include "memory.h"

/* Track total bytes allocated (for debugging/profiling) */
static size_t bytes_allocated = 0;

void* zex_alloc(size_t size) {
    if (size == 0) return NULL;
    
    bytes_allocated += size;
    
    void* ptr = malloc(size);
    if (ptr == NULL) {
        fprintf(stderr, "Fatal: Out of memory (requested %zu bytes)\n", size);
        exit(1);
    }
    
    return ptr;
}

void* zex_realloc(void* ptr, size_t old_size, size_t new_size) {
    bytes_allocated += new_size - old_size;
    
    if (new_size == 0) {
        free(ptr);
        return NULL;
    }
    
    void* new_ptr = realloc(ptr, new_size);
    if (new_ptr == NULL) {
        fprintf(stderr, "Fatal: Out of memory (requested %zu bytes)\n", new_size);
        exit(1);
    }
    
    return new_ptr;
}

void zex_free(void* ptr, size_t size) {
    if (ptr == NULL) return;
    bytes_allocated -= size;
    free(ptr);
}

char* zex_strdup(const char* str) {
    if (str == NULL) return NULL;
    
    size_t len = strlen(str);
    char* copy = zex_alloc(len + 1);
    memcpy(copy, str, len + 1);
    return copy;
}

char* zex_strndup(const char* str, size_t n) {
    if (str == NULL) return NULL;
    
    size_t len = strlen(str);
    if (n < len) len = n;
    
    char* copy = zex_alloc(len + 1);
    memcpy(copy, str, len);
    copy[len] = '\0';
    return copy;
}
