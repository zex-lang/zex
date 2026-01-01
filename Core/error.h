/*
 * Zex Programming Language
 * Core/error.h - Elm-style error handling interface
 */

#ifndef ZEX_ERROR_H
#define ZEX_ERROR_H

#include "common.h"

/* Source location for error reporting */
typedef struct {
    const char* filename;
    const char* source;        /* Full source code */
    int line;
    int column;
    int length;                /* Length of error span */
} SourceLoc;

/* Error types */
typedef enum {
    ERROR_SYNTAX,
    ERROR_TYPE,
    ERROR_RUNTIME,
    ERROR_NAME,
} ErrorType;

/* Initialize error system with source */
void error_init(const char* filename, const char* source);

/* Report an error with Elm-style formatting */
void error_report(ErrorType type, SourceLoc loc, const char* message, const char* hint);

/* Convenience functions */
void error_at(int line, int column, const char* message, const char* hint);
void error_at_token(const char* token_start, int token_length, int line, int column,
                    const char* message, const char* hint);

/* Check if any errors occurred */
bool error_had_error(void);

/* Get current filename */
const char* error_get_filename(void);

/* Reset error state */
void error_reset(void);

/* Print formatted error message */
void error_reportf(ErrorType type, SourceLoc loc, const char* hint, 
                   const char* format, ...) __attribute__((format(printf, 4, 5)));

/* Compile-time error with optional line number */
void error_compile(int line, const char* format, ...) __attribute__((format(printf, 2, 3)));

/* Runtime error with formatted message */
void error_runtime(const char* format, ...) __attribute__((format(printf, 1, 2)));

#endif /* ZEX_ERROR_H */
