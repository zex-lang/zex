/*
 * Zex Programming Language
 * Compiler/bytecode.h - Bytecode definitions
 */

#ifndef ZEX_BYTECODE_H
#define ZEX_BYTECODE_H

#include "common.h"
#include "object.h"

/*
 * Magic number: "PLAYWITHZEX\0" (12 bytes)
 * Stored as individual bytes to avoid endianness issues
 */
extern const uint8_t ZEX_MAGIC[12];

/* Bytecode version */
#define ZEX_BYTECODE_VERSION 1

/*
 * Register-based opcodes
 * Format: OP_NAME [operands...]
 */
typedef enum {
    /* Load constants */
    OP_LOAD_CONST,      /* R, idx16 -- R = constants[idx] */
    OP_LOAD_NULL,       /* R -- R = null */
    OP_LOAD_TRUE,       /* R -- R = true */
    OP_LOAD_FALSE,      /* R -- R = false */
    
    /* Register operations */
    OP_MOVE,            /* Rdst, Rsrc -- Rdst = Rsrc */
    
    /* Global variables */
    OP_GET_GLOBAL,      /* R, idx16 -- R = globals[constants[idx]] */
    OP_SET_GLOBAL,      /* idx16, R -- globals[constants[idx]] = R */
    OP_DEF_GLOBAL,      /* idx16, R -- define globals[constants[idx]] = R */
    
    /* Local variables */
    OP_GET_LOCAL,       /* R, slot -- R = locals[slot] */
    OP_SET_LOCAL,       /* slot, R -- locals[slot] = R */
    
    /* Arithmetic (all: Rdst, Ra, Rb) */
    OP_ADD,             /* Rdst = Ra + Rb */
    OP_SUB,             /* Rdst = Ra - Rb */
    OP_MUL,             /* Rdst = Ra * Rb */
    OP_DIV,             /* Rdst = Ra / Rb */
    OP_MOD,             /* Rdst = Ra % Rb */
    OP_NEG,             /* Rdst, R -- Rdst = -R */
    
    /* Comparison (all: Rdst, Ra, Rb) */
    OP_EQ,              /* Rdst = Ra == Rb */
    OP_NE,              /* Rdst = Ra != Rb */
    OP_LT,              /* Rdst = Ra < Rb */
    OP_LE,              /* Rdst = Ra <= Rb */
    OP_GT,              /* Rdst = Ra > Rb */
    OP_GE,              /* Rdst = Ra >= Rb */
    
    /* Logical */
    OP_NOT,             /* Rdst, R -- Rdst = !R */
    
    /* Control flow */
    OP_JUMP,            /* offset16 -- ip += offset */
    OP_JUMP_BACK,       /* offset16 -- ip -= offset */
    OP_JUMP_IF_FALSE,   /* R, offset16 -- if (!R) ip += offset */
    OP_JUMP_IF_TRUE,    /* R, offset16 -- if (R) ip += offset */
    
    /* Functions */
    OP_CALL,            /* Rfunc, argc, Rbase -- call Rfunc with args at Rbase */
    OP_RETURN,          /* R -- return R */
    OP_CLOSURE,         /* R, idx16 -- R = closure(constants[idx]) */
    
    /* Classes & Objects */
    OP_CLASS,           /* R, idx16 -- R = new class(constants[idx]) */
    OP_GET_PROPERTY,    /* Rdst, Robj, idx16 -- Rdst = Robj.constants[idx] */
    OP_SET_PROPERTY,    /* Robj, idx16, Rval -- Robj.constants[idx] = Rval */
    OP_METHOD,          /* Rclass, idx16 -- add method to class */
    OP_INVOKE,          /* Rdst, Robj, idx16, argc -- invoke method */
    OP_INHERIT,         /* Rsubclass, Rsuperclass -- inherit methods */
    
    /* Arrays */
    OP_ARRAY,           /* Rdst, count -- create array from count values on stack */
    OP_INDEX_GET,       /* Rdst, Rarr, Ridx -- Rdst = Rarr[Ridx] */
    OP_INDEX_SET,       /* Rarr, Ridx, Rval -- Rarr[Ridx] = Rval */
    
    /* Iterators (for-in loops) */
    OP_ITER_NEXT,       /* Rval, Ridx, Rarr, offset16 -- get arr[idx], idx++, jump if done */
    
    /* Exception handling */
    OP_TRY_BEGIN,       /* offset16 -- push exception handler, jump to offset on exception */
    OP_TRY_END,         /* -- pop exception handler (successful try completion) */
    OP_RAISE,           /* R -- raise exception in R (0xFF = re-raise current exception) */
    OP_CHECK_EXC_TYPE,  /* Rdst, idx16 -- check if current exception matches type, bool to Rdst */
    OP_GET_EXCEPTION,   /* R -- get current exception into R */
    OP_CLEAR_EXCEPTION, /* -- clear current exception (after handling) */
} OpCode;

/* Get opcode name for debugging */
const char* opcode_name(OpCode op);

/* Get number of operand bytes for an opcode */
int opcode_operand_count(OpCode op);

/*
 * Bytecode chunk - holds compiled bytecode
 */
typedef struct Chunk {
    uint8_t* code;
    int count;
    int capacity;
    
    /* Constant pool */
    Value* constants;
    int const_count;
    int const_capacity;
    
    /* Line information for errors */
    int* lines;
} Chunk;

/* Initialize a chunk */
void chunk_init(Chunk* chunk);

/* Free a chunk */
void chunk_free(Chunk* chunk);

/* Write a byte to the chunk */
void chunk_write(Chunk* chunk, uint8_t byte, int line);

/* Write a 16-bit value (little endian) */
void chunk_write16(Chunk* chunk, uint16_t value, int line);

/* Add a constant and return its index */
int chunk_add_constant(Chunk* chunk, Value value);

/* Disassemble chunk for debugging */
void chunk_disassemble(Chunk* chunk, const char* name);

/* Disassemble single instruction, returns next offset */
int chunk_disassemble_instruction(Chunk* chunk, int offset);

/* Bytecode file serialization */
bool bytecode_save(ObjFunction* fn, const char* path);
ObjFunction* bytecode_load(const char* path);
bool bytecode_is_compiled(const char* path);

#endif /* ZEX_BYTECODE_H */
