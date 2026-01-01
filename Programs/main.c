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
    {"compile",       no_argument,       NULL, 'c'},
    {"output",        required_argument, NULL, 'o'},
    {"dump-ast",      no_argument,       NULL, 'a'},
    {"dump-bytecode", no_argument,       NULL, 'b'},
    {"help",          no_argument,       NULL, 'h'},
    {"version",       no_argument,       NULL, 'v'},
    {NULL, 0, NULL, 0}
};

typedef struct {
    bool compile;
    const char* output;
    bool dump_ast;
    bool dump_bytecode;
    const char* script_path;
} Options;

static void print_usage(const char* program) {
    printf("Zex Programming Language v%s\n\n", ZEX_VERSION_STRING);
    printf("Usage: %s [options] <script>\n\n", program);
    printf("Options:\n");
    printf("  -c, --compile        Compile to bytecode file\n");
    printf("  -o, --output <file>  Output file for --compile\n");
    printf("  --dump-ast           Print the Abstract Syntax Tree\n");
    printf("  --dump-bytecode      Print the compiled bytecode\n");
    printf("  -h, --help           Show this help message\n");
    printf("  -v, --version        Show version information\n");
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

static char* get_default_output(const char* input_path) {
    size_t len = strlen(input_path);
    char* out = malloc(len + 2);
    strcpy(out, input_path);
    
    /* Replace .zex with .zexc or append .zexc */
    if (len > 4 && strcmp(input_path + len - 4, ".zex") == 0) {
        out[len - 4] = '\0';
        strcat(out, ".zexc");
    } else {
        strcat(out, ".zexc");
    }
    return out;
}

static int compile_file(const char* path, Options* opts) {
    char* source = read_file(path);
    if (source == NULL) {
        return 74;
    }
    
    VM* vm = vm_get();
    vm_init(vm);
    register_builtins(vm);
    
    error_init(path, source);
    
    ASTNode* ast = parse(source);
    if (ast == NULL) {
        free(source);
        vm_free(vm);
        return 65;
    }
    
    CompileResult compiled = compile(ast);
    ast_free(ast);
    
    if (compiled.had_error || compiled.function == NULL) {
        free(source);
        vm_free(vm);
        return 65;
    }
    
    char* output = opts->output ? (char*)opts->output : get_default_output(path);
    
    if (!bytecode_save(compiled.function, output)) {
        fprintf(stderr, "Error: Could not write bytecode to '%s'\n", output);
        if (!opts->output) free(output);
        free(source);
        vm_free(vm);
        return 74;
    }
    
    if (!opts->output) free(output);
    free(source);
    vm_free(vm);
    return 0;
}

static int run_bytecode(const char* path, Options* opts) {
    VM* vm = vm_get();
    vm_init(vm);
    register_builtins(vm);
    
    ObjFunction* fn = bytecode_load(path);
    if (fn == NULL) {
        fprintf(stderr, "Error: Could not load bytecode from '%s'\n", path);
        vm_free(vm);
        return 65;
    }
    
    if (opts->dump_bytecode) {
        const char* basename = path;
        const char* p = path;
        while (*p) {
            if (*p == '/') basename = p + 1;
            p++;
        }
        chunk_disassemble(fn->chunk, basename);
        printf("\n");
        vm_free(vm);
        return 0;
    }
    
    InterpretResult result = vm_run(vm, fn);
    vm_free(vm);
    
    if (result == INTERPRET_COMPILE_ERROR) return 65;
    if (result == INTERPRET_RUNTIME_ERROR) return 70;
    return 0;
}

static int run_source(const char* path, Options* opts) {
    char* source = read_file(path);
    if (source == NULL) {
        return 74;
    }
    
    VM* vm = vm_get();
    vm_init(vm);
    register_builtins(vm);
    
    error_init(path, source);
    
    ASTNode* ast = parse(source);
    if (ast == NULL) {
        free(source);
        vm_free(vm);
        return 65;
    }
    
    if (opts->dump_ast) {
        ast_print(ast, 0);
        printf("\n");
    }
    
    CompileResult compiled = compile(ast);
    ast_free(ast);
    
    if (compiled.had_error || compiled.function == NULL) {
        free(source);
        vm_free(vm);
        return 65;
    }
    
    if (opts->dump_bytecode) {
        const char* basename = path;
        const char* p = path;
        while (*p) {
            if (*p == '/') basename = p + 1;
            p++;
        }
        chunk_disassemble(compiled.function->chunk, basename);
        printf("\n");
    }
    
    if (opts->dump_ast || opts->dump_bytecode) {
        free(source);
        vm_free(vm);
        return 0;
    }
    
    InterpretResult result = vm_run(vm, compiled.function);
    
    free(source);
    vm_free(vm);
    
    if (result == INTERPRET_COMPILE_ERROR) return 65;
    if (result == INTERPRET_RUNTIME_ERROR) return 70;
    
    return 0;
}

static int run_file(const char* path, Options* opts) {
    if (opts->compile) {
        return compile_file(path, opts);
    }
    
    if (bytecode_is_compiled(path)) {
        return run_bytecode(path, opts);
    }
    
    return run_source(path, opts);
}

int main(int argc, char* argv[]) {
    Options opts = {false, NULL, false, false, NULL};
    
    int opt;
    while ((opt = getopt_long(argc, argv, "co:abhv", long_options, NULL)) != -1) {
        switch (opt) {
            case 'c':
                opts.compile = true;
                break;
            case 'o':
                opts.output = optarg;
                break;
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
                return 64;
        }
    }
    
    if (optind >= argc) {
        print_usage(argv[0]);
        return 64;
    }
    
    opts.script_path = argv[optind];
    
    return run_file(opts.script_path, &opts);
}

