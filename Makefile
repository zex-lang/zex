# Zex Programming Language - Makefile

CC := gcc
CFLAGS := -std=c99 -Wall -Wextra -Wpedantic
CFLAGS += -I. -ICore -IObjects -IParser -ICompiler -IRuntime -IModules

# Release flags
RELEASE_FLAGS := -Ofast -flto -fomit-frame-pointer -ffast-math
RELEASE_FLAGS += -funroll-loops -DNDEBUG
RELEASE_FLAGS += -fdata-sections -ffunction-sections

# Debug flags
DEBUG_FLAGS := -O0 -g3 -DDEBUG -DDEBUG_PRINT_CODE -DDEBUG_TRACE_EXECUTION

# Linker flags
LDFLAGS_RELEASE := -flto -Wl,--gc-sections -lm
LDFLAGS_DEBUG := -lm

# Source files
CORE_SRC := Core/memory.c Core/error.c

OBJECTS_SRC := Objects/object.c Objects/tableobject.c \
               Objects/stringobject.c Objects/intobject.c \
               Objects/floatobject.c Objects/boolobject.c \
               Objects/nullobject.c Objects/funcobject.c \
               Objects/classobject.c

PARSER_SRC := Parser/token.c Parser/lexer.c Parser/ast.c Parser/parser.c

COMPILER_SRC := Compiler/bytecode.c Compiler/symtable.c Compiler/compiler.c

RUNTIME_SRC := Runtime/vm.c Runtime/frame.c Runtime/gc.c

MODULES_SRC := Modules/builtins.c

PROGRAMS_SRC := Programs/main.c

ALL_SRC := $(CORE_SRC) $(OBJECTS_SRC) $(PARSER_SRC) $(COMPILER_SRC) \
           $(RUNTIME_SRC) $(MODULES_SRC) $(PROGRAMS_SRC)

OBJ_RELEASE := $(ALL_SRC:.c=.release.o)
OBJ_DEBUG := $(ALL_SRC:.c=.debug.o)

TARGET := zex

# Default: Release build
all: release

release: CFLAGS += $(RELEASE_FLAGS)
release: LDFLAGS := $(LDFLAGS_RELEASE)
release: $(TARGET)
	@strip $(TARGET) 2>/dev/null || true

debug: CFLAGS += $(DEBUG_FLAGS)  
debug: LDFLAGS := $(LDFLAGS_DEBUG)
debug: $(TARGET)-debug

$(TARGET): $(OBJ_RELEASE)
	$(CC) $(OBJ_RELEASE) -o $@ $(LDFLAGS)

$(TARGET)-debug: $(OBJ_DEBUG)
	$(CC) $(OBJ_DEBUG) -o $@ $(LDFLAGS)

%.release.o: %.c
	$(CC) $(CFLAGS) $(RELEASE_FLAGS) -c $< -o $@

%.debug.o: %.c
	$(CC) $(CFLAGS) $(DEBUG_FLAGS) -c $< -o $@

clean:
	find . -name "*.o" -delete
	rm -f $(TARGET) $(TARGET)-debug

test: release
	./$(TARGET) examples/test.zex

.PHONY: all release debug clean test
