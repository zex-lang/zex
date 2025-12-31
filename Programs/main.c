/*
 * Zex Programming Language
 * Programs/main.c - Entry point
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "vm.h"
#include "builtins.h"
#include "error.h"

static void print_usage(const char* program) {
    fprintf(stderr, "Zex Programming Language v%s\n", ZEX_VERSION_STRING);
    fprintf(stderr, "Usage: %s <script.zex>\n", program);
}

static char* read_file(const char* path) {
    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        fprintf(stderr, "Error: Could not open file '%s'\n", path);
        return NULL;
    }
    
    fseek(file, 0L, SEEK_END);
    size_t file_size = ftell(file);
    rewind(file);
    
    char* buffer = (char*)malloc(file_size + 1);
    if (buffer == NULL) {
        fprintf(stderr, "Error: Not enough memory to read file '%s'\n", path);
        fclose(file);
        return NULL;
    }
    
    size_t bytes_read = fread(buffer, sizeof(char), file_size, file);
    if (bytes_read < file_size) {
        fprintf(stderr, "Error: Could not read file '%s'\n", path);
        free(buffer);
        fclose(file);
        return NULL;
    }
    
    buffer[bytes_read] = '\0';
    fclose(file);
    
    return buffer;
}

static int run_file(const char* path) {
    char* source = read_file(path);
    if (source == NULL) {
        return 74;  /* EX_IOERR */
    }
    
    VM* vm = vm_get();
    vm_init(vm);
    register_builtins(vm);
    
    error_init(path, source);
    
    InterpretResult result = vm_interpret(vm, source);
    
    free(source);
    vm_free(vm);
    
    if (result == INTERPRET_COMPILE_ERROR) return 65;  /* EX_DATAERR */
    if (result == INTERPRET_RUNTIME_ERROR) return 70;  /* EX_SOFTWARE */
    
    return 0;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        print_usage(argv[0]);
        return 64;  /* EX_USAGE */
    }
    
    return run_file(argv[1]);
}
