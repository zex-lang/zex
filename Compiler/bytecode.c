/*
 * Zex Programming Language
 * Compiler/bytecode.c - Bytecode utilities
 */

#include "bytecode.h"
#include "memory.h"
#include "stringobject.h"
#include "classobject.h"
#include "funcobject.h"

/* Magic number bytes */
const uint8_t ZEX_MAGIC[12] = {
    'P', 'L', 'A', 'Y', 'W', 'I', 'T', 'H', 'Z', 'E', 'X', '\0'
};

const char* opcode_name(OpCode op) {
    switch (op) {
        case OP_LOAD_CONST:     return "LOAD_CONST";
        case OP_LOAD_NULL:      return "LOAD_NULL";
        case OP_LOAD_TRUE:      return "LOAD_TRUE";
        case OP_LOAD_FALSE:     return "LOAD_FALSE";
        case OP_MOVE:           return "MOVE";
        case OP_GET_GLOBAL:     return "GET_GLOBAL";
        case OP_SET_GLOBAL:     return "SET_GLOBAL";
        case OP_DEF_GLOBAL:     return "DEF_GLOBAL";
        case OP_GET_LOCAL:      return "GET_LOCAL";
        case OP_SET_LOCAL:      return "SET_LOCAL";
        case OP_ADD:            return "ADD";
        case OP_SUB:            return "SUB";
        case OP_MUL:            return "MUL";
        case OP_DIV:            return "DIV";
        case OP_NEG:            return "NEG";
        case OP_EQ:             return "EQ";
        case OP_NE:             return "NE";
        case OP_LT:             return "LT";
        case OP_LE:             return "LE";
        case OP_GT:             return "GT";
        case OP_GE:             return "GE";
        case OP_NOT:            return "NOT";
        case OP_JUMP:           return "JUMP";
        case OP_JUMP_BACK:      return "JUMP_BACK";
        case OP_JUMP_IF_FALSE:  return "JUMP_IF_FALSE";
        case OP_JUMP_IF_TRUE:   return "JUMP_IF_TRUE";
        case OP_CALL:           return "CALL";
        case OP_RETURN:         return "RETURN";
        case OP_CLOSURE:        return "CLOSURE";
        case OP_CLASS:          return "CLASS";
        case OP_GET_PROPERTY:   return "GET_PROPERTY";
        case OP_SET_PROPERTY:   return "SET_PROPERTY";
        case OP_METHOD:         return "METHOD";
        case OP_INVOKE:         return "INVOKE";
        case OP_INHERIT:        return "INHERIT";
        case OP_ARRAY:          return "ARRAY";
        case OP_INDEX_GET:      return "INDEX_GET";
        case OP_INDEX_SET:      return "INDEX_SET";
        case OP_ITER_NEXT:      return "ITER_NEXT";
        default:                return "UNKNOWN";;
    }
}

int opcode_operand_count(OpCode op) {
    switch (op) {
        case OP_LOAD_NULL:
        case OP_LOAD_TRUE:
        case OP_LOAD_FALSE:
            return 1;  /* Just register */
            
        case OP_MOVE:
        case OP_NEG:
        case OP_NOT:
        case OP_GET_LOCAL:
        case OP_SET_LOCAL:
        case OP_RETURN:
        case OP_INHERIT:
            return 2;
            
        case OP_LOAD_CONST:
        case OP_GET_GLOBAL:
        case OP_SET_GLOBAL:
        case OP_DEF_GLOBAL:
        case OP_CLOSURE:
        case OP_CLASS:
        case OP_JUMP:
        case OP_JUMP_BACK:
        case OP_JUMP_IF_FALSE:
        case OP_JUMP_IF_TRUE:
            return 3;
            
        case OP_ADD:
        case OP_SUB:
        case OP_MUL:
        case OP_DIV:
        case OP_EQ:
        case OP_NE:
        case OP_LT:
        case OP_LE:
        case OP_GT:
        case OP_GE:
        case OP_CALL:
        case OP_GET_PROPERTY:
        case OP_SET_PROPERTY:
            return 4;
            
        case OP_METHOD:
            return 3;
            
        case OP_INVOKE:
            return 5;
        
        case OP_ARRAY:
            return 3;  /* Rdst, count, start_reg */
            
        case OP_INDEX_GET:
        case OP_INDEX_SET:
            return 3;  /* 3 registers */
        
        case OP_ITER_NEXT:
            return 5;  /* Rval, Ridx, Rarr, offset16 */
            
        default:
            return 1;
    }
}

void chunk_init(Chunk* chunk) {
    chunk->code = NULL;
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->constants = NULL;
    chunk->const_count = 0;
    chunk->const_capacity = 0;
    chunk->lines = NULL;
}

void chunk_free(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(Value, chunk->constants, chunk->const_capacity);
    FREE_ARRAY(int, chunk->lines, chunk->capacity);
    chunk_init(chunk);
}

void chunk_write(Chunk* chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        int old_capacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(old_capacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, old_capacity, chunk->capacity);
        chunk->lines = GROW_ARRAY(int, chunk->lines, old_capacity, chunk->capacity);
    }
    
    chunk->code[chunk->count] = byte;
    chunk->lines[chunk->count] = line;
    chunk->count++;
}

void chunk_write16(Chunk* chunk, uint16_t value, int line) {
    /* Little endian */
    chunk_write(chunk, (uint8_t)(value & 0xFF), line);
    chunk_write(chunk, (uint8_t)((value >> 8) & 0xFF), line);
}

int chunk_add_constant(Chunk* chunk, Value value) {
    if (chunk->const_capacity < chunk->const_count + 1) {
        int old_capacity = chunk->const_capacity;
        chunk->const_capacity = GROW_CAPACITY(old_capacity);
        chunk->constants = GROW_ARRAY(Value, chunk->constants, old_capacity, chunk->const_capacity);
    }
    
    chunk->constants[chunk->const_count] = value;
    return chunk->const_count++;
}

static int register_instruction(const char* name, Chunk* chunk, int offset) {
    uint8_t reg = chunk->code[offset + 1];
    printf("%-16s R%d\n", name, reg);
    return offset + 2;
}

static int two_register_instruction(const char* name, Chunk* chunk, int offset) {
    uint8_t r1 = chunk->code[offset + 1];
    uint8_t r2 = chunk->code[offset + 2];
    printf("%-16s R%d, R%d\n", name, r1, r2);
    return offset + 3;
}

static int three_register_instruction(const char* name, Chunk* chunk, int offset) {
    uint8_t r1 = chunk->code[offset + 1];
    uint8_t r2 = chunk->code[offset + 2];
    uint8_t r3 = chunk->code[offset + 3];
    printf("%-16s R%d, R%d, R%d\n", name, r1, r2, r3);
    return offset + 4;
}

static int constant_instruction(const char* name, Chunk* chunk, int offset) {
    uint8_t reg = chunk->code[offset + 1];
    uint16_t idx = chunk->code[offset + 2] | (chunk->code[offset + 3] << 8);
    printf("%-16s R%d, [%d] = ", name, reg, idx);
    print_value(chunk->constants[idx]);
    printf("\n");
    return offset + 4;
}

static int global_instruction(const char* name, Chunk* chunk, int offset) {
    uint8_t reg = chunk->code[offset + 1];
    uint16_t idx = chunk->code[offset + 2] | (chunk->code[offset + 3] << 8);
    printf("%-16s R%d, '%s'\n", name, reg, 
           AS_STRING(chunk->constants[idx])->chars);
    return offset + 4;
}

static int jump_instruction(const char* name, Chunk* chunk, int offset, int sign) {
    uint8_t reg = chunk->code[offset + 1];
    uint16_t jump = chunk->code[offset + 2] | (chunk->code[offset + 3] << 8);
    printf("%-16s R%d, %04d -> %04d\n", name, reg, offset, offset + 4 + sign * jump);
    return offset + 4;
}

int chunk_disassemble_instruction(Chunk* chunk, int offset) {
    printf("%04d ", offset);
    
    if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1]) {
        printf("   | ");
    } else {
        printf("%4d ", chunk->lines[offset]);
    }
    
    uint8_t instruction = chunk->code[offset];
    
    switch (instruction) {
        case OP_LOAD_CONST:
            return constant_instruction("LOAD_CONST", chunk, offset);
        case OP_LOAD_NULL:
            return register_instruction("LOAD_NULL", chunk, offset);
        case OP_LOAD_TRUE:
            return register_instruction("LOAD_TRUE", chunk, offset);
        case OP_LOAD_FALSE:
            return register_instruction("LOAD_FALSE", chunk, offset);
        case OP_MOVE:
            return two_register_instruction("MOVE", chunk, offset);
        case OP_GET_GLOBAL:
            return global_instruction("GET_GLOBAL", chunk, offset);
        case OP_SET_GLOBAL:
            return global_instruction("SET_GLOBAL", chunk, offset);
        case OP_DEF_GLOBAL:
            return global_instruction("DEF_GLOBAL", chunk, offset);
        case OP_GET_LOCAL:
            return two_register_instruction("GET_LOCAL", chunk, offset);
        case OP_SET_LOCAL:
            return two_register_instruction("SET_LOCAL", chunk, offset);
        case OP_ADD:
            return three_register_instruction("ADD", chunk, offset);
        case OP_SUB:
            return three_register_instruction("SUB", chunk, offset);
        case OP_MUL:
            return three_register_instruction("MUL", chunk, offset);
        case OP_DIV:
            return three_register_instruction("DIV", chunk, offset);
        case OP_NEG:
            return two_register_instruction("NEG", chunk, offset);
        case OP_EQ:
            return three_register_instruction("EQ", chunk, offset);
        case OP_NE:
            return three_register_instruction("NE", chunk, offset);
        case OP_LT:
            return three_register_instruction("LT", chunk, offset);
        case OP_LE:
            return three_register_instruction("LE", chunk, offset);
        case OP_GT:
            return three_register_instruction("GT", chunk, offset);
        case OP_GE:
            return three_register_instruction("GE", chunk, offset);
        case OP_NOT:
            return two_register_instruction("NOT", chunk, offset);
        case OP_JUMP: {
            uint16_t jump = chunk->code[offset + 1] | (chunk->code[offset + 2] << 8);
            printf("%-16s %04d -> %04d\n", "JUMP", offset, offset + 3 + jump);
            return offset + 3;
        }
        case OP_JUMP_BACK: {
            uint16_t jump = chunk->code[offset + 1] | (chunk->code[offset + 2] << 8);
            printf("%-16s %04d -> %04d\n", "JUMP_BACK", offset, offset + 3 - jump);
            return offset + 3;
        }
        case OP_JUMP_IF_FALSE:
            return jump_instruction("JUMP_IF_FALSE", chunk, offset, 1);
        case OP_JUMP_IF_TRUE:
            return jump_instruction("JUMP_IF_TRUE", chunk, offset, 1);
        case OP_CALL: {
            uint8_t r = chunk->code[offset + 1];
            uint8_t argc = chunk->code[offset + 2];
            uint8_t base = chunk->code[offset + 3];
            printf("%-16s R%d, argc=%d, base=R%d\n", "CALL", r, argc, base);
            return offset + 4;
        }
        case OP_RETURN:
            return register_instruction("RETURN", chunk, offset);
        case OP_CLOSURE:
            return constant_instruction("CLOSURE", chunk, offset);
        case OP_CLASS:
            return constant_instruction("CLASS", chunk, offset);
        case OP_GET_PROPERTY:
            return three_register_instruction("GET_PROPERTY", chunk, offset);
        case OP_SET_PROPERTY:
            return three_register_instruction("SET_PROPERTY", chunk, offset);
        case OP_METHOD:
            return two_register_instruction("METHOD", chunk, offset);
        case OP_INVOKE: {
            uint8_t rdst = chunk->code[offset + 1];
            uint8_t robj = chunk->code[offset + 2];
            uint16_t idx = chunk->code[offset + 3] | (chunk->code[offset + 4] << 8);
            uint8_t argc = chunk->code[offset + 5];
            printf("%-16s R%d, R%d.%s, argc=%d\n", "INVOKE", rdst, robj,
                   AS_STRING(chunk->constants[idx])->chars, argc);
            return offset + 6;
        }
        case OP_INHERIT:
            return two_register_instruction("INHERIT", chunk, offset);
        case OP_ARRAY: {
            uint8_t rdst = chunk->code[offset + 1];
            uint8_t count = chunk->code[offset + 2];
            uint8_t start = chunk->code[offset + 3];
            printf("%-16s R%d, count=%d, start=R%d\n", "ARRAY", rdst, count, start);
            return offset + 4;
        }
        case OP_INDEX_GET:
            return three_register_instruction("INDEX_GET", chunk, offset);
        case OP_INDEX_SET:
            return three_register_instruction("INDEX_SET", chunk, offset);
        case OP_ITER_NEXT: {
            uint8_t rval = chunk->code[offset + 1];
            uint8_t ridx = chunk->code[offset + 2];
            uint8_t rarr = chunk->code[offset + 3];
            uint16_t jump = chunk->code[offset + 4] | (chunk->code[offset + 5] << 8);
            printf("%-16s R%d, R%d, R%d, -> %04d\n", "ITER_NEXT", rval, ridx, rarr, offset + 6 + jump);
            return offset + 6;
        }
        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}

void chunk_disassemble(Chunk* chunk, const char* name) {
    /* Print header for this chunk */
    if (name != NULL) {
        printf("-- %s --\n", name);
    }
    
    /* Disassemble the chunk */
    for (int offset = 0; offset < chunk->count;) {
        offset = chunk_disassemble_instruction(chunk, offset);
    }
    
    /* Then disassemble any functions in the constant pool */
    for (int i = 0; i < chunk->const_count; i++) {
        Value constant = chunk->constants[i];
        if (constant.obj != NULL && constant.obj->type == OBJ_FUNCTION) {
            ObjFunction* fn = (ObjFunction*)constant.obj;
            if (fn->chunk != NULL && fn->chunk->count > 0) {
                printf("\n");
                chunk_disassemble(fn->chunk, fn->name ? fn->name->chars : "<anonymous>");
            }
        }
    }
}
