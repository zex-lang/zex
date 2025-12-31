/*
 * Zex Programming Language
 * Programs/main.c - Entry point
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>

#include "common.h"
#include "vm.h"
#include "builtins.h"
#include "error.h"
#include "parser.h"
#include "compiler.h"
#include "bytecode.h"
#include "ast.h"

/* Command line options */
static struct option long_options[] = {
    {"dump-ast",      no_argument, NULL, 'a'},
    {"dump-bytecode", no_argument, NULL, 'b'},
    {"help",          no_argument, NULL, 'h'},
    {"version",       no_argument, NULL, 'v'},
    {NULL, 0, NULL, 0}
};

typedef struct {
    bool dump_ast;
    bool dump_bytecode;
    const char* script_path;
} Options;

static void print_usage(const char* program) {
    printf("Zex Programming Language v%s\n\n", ZEX_VERSION_STRING);
    printf("Usage: %s [options] <script.zex>\n\n", program);
    printf("Options:\n");
    printf("  --dump-ast       Print the Abstract Syntax Tree\n");
    printf("  --dump-bytecode  Print the compiled bytecode\n");
    printf("  --help, -h       Show this help message\n");
    printf("  --version, -v    Show version information\n");
}

static void print_version(void) {
    printf("Zex %s\n", ZEX_VERSION_STRING);
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

static int run_file(const char* path, Options* opts) {
    char* source = read_file(path);
    if (source == NULL) {
        return 74;  /* EX_IOERR */
    }
    
    /* Initialize VM and builtins first (needed for compilation) */
    VM* vm = vm_get();
    vm_init(vm);
    register_builtins(vm);
    
    error_init(path, source);
    
    /* Parse to AST */
    ASTNode* ast = parse(source);
    if (ast == NULL) {
        free(source);
        vm_free(vm);
        return 65;  /* EX_DATAERR */
    }
    
    /* Dump AST if requested */
    if (opts->dump_ast) {
        ast_print(ast, 0);
        printf("\n");
    }
    
    /* Compile to bytecode */
    CompileResult compiled = compile(ast);
    ast_free(ast);
    
    if (compiled.had_error || compiled.function == NULL) {
        free(source);
        vm_free(vm);
        return 65;  /* EX_DATAERR */
    }
    
    /* Dump bytecode if requested */
    if (opts->dump_bytecode) {
        chunk_disassemble(compiled.function->chunk);
        printf("\n");
    }
    
    /* If only dumping, don't run */
    if (opts->dump_ast || opts->dump_bytecode) {
        free(source);
        vm_free(vm);
        return 0;
    }
    
    /* Run the program */
    InterpretResult result = vm_run(vm, compiled.function);
    
    free(source);
    vm_free(vm);
    
    if (result == INTERPRET_COMPILE_ERROR) return 65;  /* EX_DATAERR */
    if (result == INTERPRET_RUNTIME_ERROR) return 70;  /* EX_SOFTWARE */
    
    return 0;
}

int main(int argc, char* argv[]) {
    Options opts = {false, false, NULL};
    
    int opt;
    while ((opt = getopt_long(argc, argv, "abhv", long_options, NULL)) != -1) {
        switch (opt) {
            case 'a':
                opts.dump_ast = true;
                break;
            case 'b':
                opts.dump_bytecode = true;
                break;
            case 'h':
                print_usage(argv[0]);
                return 0;
            case 'v':
                print_version();
                return 0;
            default:
                print_usage(argv[0]);
                return 64;  /* EX_USAGE */
        }
    }
    
    /* Get the script path (remaining argument) */
    if (optind >= argc) {
        print_usage(argv[0]);
        return 64;  /* EX_USAGE */
    }
    
    opts.script_path = argv[optind];
    
    return run_file(opts.script_path, &opts);
}
