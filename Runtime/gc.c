/*
 * Zex Programming Language
 * Runtime/gc.c - Garbage collector
 */

#include "vm.h"
#include "object.h"

/* Simple mark-sweep GC - placeholder for now */
/* Full implementation would track roots and mark reachable objects */

void gc_collect(VM* vm) {
    UNUSED(vm);
    /* TODO: Implement when needed */
}
