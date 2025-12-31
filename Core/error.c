/*
 * Zex Programming Language
 * Core/error.c - Elm-style error handling implementation
 */

#include "error.h"

/* ANSI color codes */
#define COLOR_RESET   "\033[0m"
#define COLOR_RED     "\033[31m"
#define COLOR_YELLOW  "\033[33m"
#define COLOR_CYAN    "\033[36m"
#define COLOR_BOLD    "\033[1m"
#define COLOR_DIM     "\033[2m"

/* Global error state */
static const char* g_filename = NULL;
static const char* g_source = NULL;
static bool g_had_error = false;

void error_init(const char* filename, const char* source) {
    g_filename = filename;
    g_source = source;
    g_had_error = false;
}

bool error_had_error(void) {
    return g_had_error;
}

void error_reset(void) {
    g_had_error = false;
}

/* Get a line from source code */
static void get_line(const char* source, int line_num, const char** start, int* length) {
    const char* p = source;
    int current_line = 1;
    
    /* Find start of requested line */
    while (*p && current_line < line_num) {
        if (*p == '\n') current_line++;
        p++;
    }
    *start = p;
    
    /* Find end of line */
    const char* end = p;
    while (*end && *end != '\n') end++;
    *length = (int)(end - p);
}

/* Get error type name */
static const char* error_type_name(ErrorType type) {
    switch (type) {
        case ERROR_SYNTAX:  return "Syntax Error";
        case ERROR_TYPE:    return "Type Error";
        case ERROR_RUNTIME: return "Runtime Error";
        case ERROR_NAME:    return "Name Error";
        default:            return "Error";
    }
}

void error_report(ErrorType type, SourceLoc loc, const char* message, const char* hint) {
    g_had_error = true;
    
    const char* line_start;
    int line_length;
    get_line(g_source ? g_source : "", loc.line, &line_start, &line_length);
    
    /* Calculate line number width */
    int line_width = 1;
    int temp = loc.line;
    while (temp >= 10) { temp /= 10; line_width++; }
    
    fprintf(stderr, "\n");
    
    /* Header */
    fprintf(stderr, "%s%s── %s %s", COLOR_BOLD, COLOR_RED, error_type_name(type), COLOR_RESET);
    fprintf(stderr, "%s", COLOR_DIM);
    for (int i = 0; i < 50 - (int)strlen(error_type_name(type)); i++) {
        fprintf(stderr, "─");
    }
    fprintf(stderr, "%s\n", COLOR_RESET);
    fprintf(stderr, "\n");
    
    /* File location */
    if (g_filename) {
        fprintf(stderr, "%s%s --> %s:%d:%d%s\n", 
                COLOR_CYAN, COLOR_BOLD, g_filename, loc.line, loc.column, COLOR_RESET);
        fprintf(stderr, "%s%*s │%s\n", COLOR_CYAN, line_width, "", COLOR_RESET);
    }
    
    /* Source line */
    fprintf(stderr, "%s%*d │%s ", COLOR_CYAN, line_width, loc.line, COLOR_RESET);
    fprintf(stderr, "%.*s\n", line_length, line_start);
    
    /* Error pointer */
    fprintf(stderr, "%s%*s │%s ", COLOR_CYAN, line_width, "", COLOR_RESET);
    for (int i = 0; i < loc.column - 1; i++) {
        fprintf(stderr, " ");
    }
    fprintf(stderr, "%s%s", COLOR_RED, COLOR_BOLD);
    int underline_len = loc.length > 0 ? loc.length : 1;
    for (int i = 0; i < underline_len; i++) {
        fprintf(stderr, "^");
    }
    fprintf(stderr, "%s\n", COLOR_RESET);
    
    fprintf(stderr, "\n");
    
    /* Error message */
    fprintf(stderr, "%s%s\n", message, COLOR_RESET);
    
    /* Hint */
    if (hint) {
        fprintf(stderr, "\n");
        fprintf(stderr, "%s%sHint:%s %s\n", COLOR_YELLOW, COLOR_BOLD, COLOR_RESET, hint);
    }
    
    fprintf(stderr, "\n");
}

void error_at(int line, int column, const char* message, const char* hint) {
    SourceLoc loc = {
        .filename = g_filename,
        .source = g_source,
        .line = line,
        .column = column,
        .length = 1,
    };
    error_report(ERROR_SYNTAX, loc, message, hint);
}

void error_at_token(const char* token_start, int token_length, int line, int column,
                    const char* message, const char* hint) {
    UNUSED(token_start);
    SourceLoc loc = {
        .filename = g_filename,
        .source = g_source,
        .line = line,
        .column = column,
        .length = token_length,
    };
    error_report(ERROR_SYNTAX, loc, message, hint);
}

void error_reportf(ErrorType type, SourceLoc loc, const char* hint, 
                   const char* format, ...) {
    char buffer[1024];
    va_list args;
    va_start(args, format);
    vsnprintf(buffer, sizeof(buffer), format, args);
    va_end(args);
    
    error_report(type, loc, buffer, hint);
}
