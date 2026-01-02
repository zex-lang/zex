/*
 * Zex Programming Language
 * Core/error.h - Unified error handling interface
 */

#ifndef ZEX_ERROR_H
#define ZEX_ERROR_H

#include "common.h"

/* Error types */
typedef enum {
    ERROR_SYNTAX,
    ERROR_COMPILE,
    ERROR_RUNTIME,
} ErrorType;

/* Maximum stack trace depth */
#define ZEX_MAX_ERROR_FRAMES 64

/* Initialize error system with source */
void error_init(const char* filename, const char* source);

/* Check if any errors occurred */
bool error_had_error(void);

/* Get current filename */
const char* error_get_filename(void);

/* Get current source */
const char* error_get_source(void);

/* Reset error state */
void error_reset(void);

/* Push a frame onto the error stack trace */
void error_push_frame(const char* name, int line, int column);

/* Clear the error stack trace */
void error_clear_frames(void);

/*
 * Format:
 *   at Line N, <Error Type>: <message>
 *     <source line>
 *     ^^^^^^^^^^^^
 *   
 *   Stack trace:
 *     in func() (/path/to/file.zex:N:M)
 */
void zex_error(ErrorType type, int line, int column, int span, 
               const char* format, ...) __attribute__((format(printf, 5, 6)));

/* Report an exception with custom type name (for exception classes) */
void zex_exception(const char* type_name, int line, int column, int span, 
                   const char* format, ...) __attribute__((format(printf, 5, 6)));

#endif /* ZEX_ERROR_H */
