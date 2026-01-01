/*
 * Zex Programming Language
 * Core/common.h - Common types and macros
 */

#ifndef ZEX_COMMON_H
#define ZEX_COMMON_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/* Version info */
#define ZEX_VERSION_STRING "0.1.0"

/* Utility macros */
#define UNUSED(x) ((void)(x))

#define GROW_CAPACITY(cap) ((cap) < 8 ? 8 : (cap) * 2)

#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))

/* Forward declarations of main types */
typedef struct Obj Obj;
typedef struct ObjString ObjString;
typedef struct ObjFunction ObjFunction;
typedef struct ObjClass ObjClass;
typedef struct ObjInstance ObjInstance;
typedef struct ObjNative ObjNative;
typedef struct ObjBoundMethod ObjBoundMethod;
typedef struct VM VM;
typedef struct Compiler Compiler;

/* Result type for interpret */
typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;

#endif /* ZEX_COMMON_H */
