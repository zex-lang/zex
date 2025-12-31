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
#define ZEX_VERSION_MAJOR 0
#define ZEX_VERSION_MINOR 1
#define ZEX_VERSION_PATCH 0
#define ZEX_VERSION_STRING "0.1.0"

/* Bytecode magic number - stored as bytes to avoid endian issues */
#define ZEX_MAGIC_0 'P'
#define ZEX_MAGIC_1 'L'
#define ZEX_MAGIC_2 'A'
#define ZEX_MAGIC_3 'Y'
#define ZEX_MAGIC_4 'W'
#define ZEX_MAGIC_5 'I'
#define ZEX_MAGIC_6 'T'
#define ZEX_MAGIC_7 'H'
#define ZEX_MAGIC_8 'Z'
#define ZEX_MAGIC_9 'E'
#define ZEX_MAGIC_10 'X'
#define ZEX_MAGIC_11 '\0'
#define ZEX_MAGIC_SIZE 12

/* Utility macros */
#define UNUSED(x) ((void)(x))

#define GROW_CAPACITY(cap) ((cap) < 8 ? 8 : (cap) * 2)

#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))

/* Debug macros */
#ifdef DEBUG
    #define DEBUG_PRINT(fmt, ...) \
        fprintf(stderr, "[DEBUG %s:%d] " fmt "\n", __FILE__, __LINE__, ##__VA_ARGS__)
    #define DEBUG_TRACE_EXECUTION
#else
    #define DEBUG_PRINT(fmt, ...) ((void)0)
#endif

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
