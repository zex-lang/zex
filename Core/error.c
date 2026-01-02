/*
 * Zex Programming Language
 * Core/error.c - Unified error handling implementation
 */

#include "error.h"

/* ANSI color codes */
#define COLOR_RESET   "\033[0m"
#define COLOR_RED     "\033[31m"
#define COLOR_CYAN    "\033[36m"
#define COLOR_BOLD    "\033[1m"

/* Global error state */
static const char* g_filename = NULL;
static const char* g_source = NULL;
static bool g_had_error = false;

/* Stack trace frame */
typedef struct {
    const char* name;
    int line;
    int column;
} ErrorFrame;

/* Stack trace storage */
static ErrorFrame g_frames[ZEX_MAX_ERROR_FRAMES];
static int g_frame_count = 0;

void error_init(const char* filename, const char* source) {
    g_filename = filename;
    g_source = source;
    g_had_error = false;
    g_frame_count = 0;
}

bool error_had_error(void) {
    return g_had_error;
}

const char* error_get_filename(void) {
    return g_filename;
}

const char* error_get_source(void) {
    return g_source;
}

void error_reset(void) {
    g_had_error = false;
    g_frame_count = 0;
}

void error_push_frame(const char* name, int line, int column) {
    if (g_frame_count < ZEX_MAX_ERROR_FRAMES) {
        g_frames[g_frame_count].name = name;
        g_frames[g_frame_count].line = line;
        g_frames[g_frame_count].column = column;
        g_frame_count++;
    }
}

void error_clear_frames(void) {
    g_frame_count = 0;
}

/* Get error type name */
static const char* error_type_name(ErrorType type) {
    switch (type) {
        case ERROR_SYNTAX:  return "Syntax error";
        case ERROR_COMPILE: return "Compile error";
        case ERROR_RUNTIME: return "Runtime error";
        default:            return "Error";
    }
}

/* Get a line from source code */
static void get_source_line(const char* source, int line_num, 
                            const char** out_start, int* out_length) {
    if (source == NULL) {
        *out_start = "";
        *out_length = 0;
        return;
    }
    
    const char* p = source;
    int current_line = 1;
    
    /* Find start of requested line */
    while (*p && current_line < line_num) {
        if (*p == '\n') current_line++;
        p++;
    }
    *out_start = p;
    
    /* Find end of line */
    const char* end = p;
    while (*end && *end != '\n') end++;
    *out_length = (int)(end - p);
}

void zex_error(ErrorType type, int line, int column, int span, 
               const char* format, ...) {
    g_had_error = true;
    
    /* Get source line */
    const char* line_start;
    int line_length;
    get_source_line(g_source, line, &line_start, &line_length);
    
    fprintf(stderr, "\n");
    
    /* Header: at Line N, Error Type: message */
    fprintf(stderr, "%s%sat Line %d, %s:%s ", 
            COLOR_BOLD, COLOR_RED, line, error_type_name(type), COLOR_RESET);
    
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    
    fprintf(stderr, "\n");
    
    /* Source line (always show if available) */
    if (line_length > 0) {
        /* Find leading whitespace count */
        int leading_spaces = 0;
        while (leading_spaces < line_length && 
               (line_start[leading_spaces] == ' ' || line_start[leading_spaces] == '\t')) {
            leading_spaces++;
        }
        
        fprintf(stderr, "  %.*s\n", line_length - leading_spaces, line_start + leading_spaces);
        
        /* Pointer under error (only when we have accurate column info) */
        if (column > 0) {
            fprintf(stderr, "  ");
            int adjusted_col = column - 1 - leading_spaces;
            if (adjusted_col < 0) adjusted_col = 0;
            
            for (int i = 0; i < adjusted_col; i++) {
                fprintf(stderr, " ");
            }
            fprintf(stderr, "%s%s", COLOR_RED, COLOR_BOLD);
            int underline_len = span > 0 ? span : 1;
            for (int i = 0; i < underline_len; i++) {
                fprintf(stderr, "^");
            }
            fprintf(stderr, "%s\n", COLOR_RESET);
        }
    }
    
    /* Stack trace (only if we have frames) */
    if (g_frame_count > 0) {
        fprintf(stderr, "\nStack trace:\n");
        for (int i = g_frame_count - 1; i >= 0; i--) {
            ErrorFrame* frame = &g_frames[i];
            fprintf(stderr, "  in %s%s%s (%s%s:%d:%d%s)\n",
                    COLOR_CYAN, 
                    frame->name ? frame->name : "<script>",
                    COLOR_RESET,
                    COLOR_BOLD,
                    g_filename ? g_filename : "<unknown>",
                    frame->line,
                    frame->column,
                    COLOR_RESET);
        }
    }
    
    fprintf(stderr, "\n");
}

void zex_exception(const char* type_name, int line, int column, int span, 
                   const char* format, ...) {
    g_had_error = true;
    
    /* Get source line */
    const char* line_start;
    int line_length;
    get_source_line(g_source, line, &line_start, &line_length);
    
    fprintf(stderr, "\n");
    
    /* Header: at Line N, ExceptionType: message */
    fprintf(stderr, "%s%sat Line %d, %s:%s ", 
            COLOR_BOLD, COLOR_RED, line, type_name, COLOR_RESET);
    
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    
    fprintf(stderr, "\n");
    
    /* Source line (always show if available) */
    if (line_length > 0) {
        /* Find leading whitespace count */
        int leading_spaces = 0;
        while (leading_spaces < line_length && 
               (line_start[leading_spaces] == ' ' || line_start[leading_spaces] == '\t')) {
            leading_spaces++;
        }
        
        fprintf(stderr, "  %.*s\n", line_length - leading_spaces, line_start + leading_spaces);
        
        /* Pointer under error (only when we have accurate column info) */
        if (column > 0) {
            fprintf(stderr, "  ");
            int adjusted_col = column - 1 - leading_spaces;
            if (adjusted_col < 0) adjusted_col = 0;
            
            for (int i = 0; i < adjusted_col; i++) {
                fprintf(stderr, " ");
            }
            fprintf(stderr, "%s%s", COLOR_RED, COLOR_BOLD);
            int underline_len = span > 0 ? span : 1;
            for (int i = 0; i < underline_len; i++) {
                fprintf(stderr, "^");
            }
            fprintf(stderr, "%s\n", COLOR_RESET);
        }
    }
    
    /* Stack trace (only if we have frames) */
    if (g_frame_count > 0) {
        fprintf(stderr, "\nStack trace:\n");
        for (int i = g_frame_count - 1; i >= 0; i--) {
            ErrorFrame* frame = &g_frames[i];
            fprintf(stderr, "  in %s%s%s (%s%s:%d:%d%s)\n",
                    COLOR_CYAN, 
                    frame->name ? frame->name : "<script>",
                    COLOR_RESET,
                    COLOR_BOLD,
                    g_filename ? g_filename : "<unknown>",
                    frame->line,
                    frame->column,
                    COLOR_RESET);
        }
    }
    
    fprintf(stderr, "\n");
}
