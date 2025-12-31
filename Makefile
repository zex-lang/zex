# Zex Programming Language - Makefile

CC := gcc
CFLAGS := -std=c99 -Wall -Wextra -Wpedantic
CFLAGS += -I. -ICore -IObjects -IParser -ICompiler -IRuntime -IModules

# Build directory
BUILD_DIR := build

# Optimization flags
CFLAGS += -Ofast -flto -DNDEBUG -fdata-sections -ffunction-sections
LDFLAGS := -flto=auto -Wl,--gc-sections -lm

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

OBJS := $(ALL_SRC:.c=.o)

TARGET := $(BUILD_DIR)/zex

all: $(TARGET)
	@strip $(TARGET) 2>/dev/null || true

$(TARGET): $(BUILD_DIR) $(OBJS)
	$(CC) $(OBJS) -o $@ $(LDFLAGS)

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	find . -name "*.o" -delete
	rm -rf $(BUILD_DIR)

.PHONY: all clean
