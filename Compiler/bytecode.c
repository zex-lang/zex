/*
 * Zex Programming Language
 * Compiler/bytecode.c - Bytecode utilities
 */

#include "bytecode.h"
#include "memory.h"
#include "stringobject.h"
#include "classobject.h"
#include "funcobject.h"
#include "intobject.h"
#include "floatobject.h"
#include "boolobject.h"
#include "nullobject.h"
#include <string.h>

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
            return 4;
            
        case OP_GET_PROPERTY:
        case OP_SET_PROPERTY:
        case OP_METHOD:
            return 4;
            
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
        case OP_GET_PROPERTY: {
            uint8_t dst = chunk->code[offset + 1];
            uint8_t obj = chunk->code[offset + 2];
            uint16_t idx = chunk->code[offset + 3] | (chunk->code[offset + 4] << 8);
            printf("%-16s R%d, R%d, '%s'\n", "GET_PROPERTY", dst, obj,
                   AS_STRING(chunk->constants[idx])->chars);
            return offset + 5;
        }
        case OP_SET_PROPERTY: {
            uint8_t obj = chunk->code[offset + 1];
            uint16_t idx = chunk->code[offset + 2] | (chunk->code[offset + 3] << 8);
            uint8_t val = chunk->code[offset + 4];
            printf("%-16s R%d, '%s', R%d\n", "SET_PROPERTY", obj,
                   AS_STRING(chunk->constants[idx])->chars, val);
            return offset + 5;
        }
        case OP_METHOD: {
            uint8_t class_reg = chunk->code[offset + 1];
            uint16_t name_idx = chunk->code[offset + 2] | (chunk->code[offset + 3] << 8);
            uint8_t method_reg = chunk->code[offset + 4];
            printf("%-16s R%d, '%s', R%d\n", "METHOD", class_reg,
                   AS_STRING(chunk->constants[name_idx])->chars, method_reg);
            return offset + 5;
        }
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

#define TAG_NULL    0
#define TAG_BOOL    1
#define TAG_INT     2
#define TAG_FLOAT   3
#define TAG_STRING  4
#define TAG_FUNC    5

static bool write_u8(FILE* f, uint8_t val) {
    return fwrite(&val, 1, 1, f) == 1;
}

static bool write_u16(FILE* f, uint16_t val) {
    uint8_t bytes[2] = {val & 0xFF, (val >> 8) & 0xFF};
    return fwrite(bytes, 1, 2, f) == 2;
}

static bool write_u32(FILE* f, uint32_t val) {
    uint8_t bytes[4] = {val & 0xFF, (val >> 8) & 0xFF, (val >> 16) & 0xFF, (val >> 24) & 0xFF};
    return fwrite(bytes, 1, 4, f) == 4;
}

static bool write_i64(FILE* f, int64_t val) {
    return fwrite(&val, sizeof(int64_t), 1, f) == 1;
}

static bool write_f64(FILE* f, double val) {
    return fwrite(&val, sizeof(double), 1, f) == 1;
}

static bool write_string(FILE* f, ObjString* str) {
    if (!write_u32(f, str->length)) return false;
    return fwrite(str->chars, 1, str->length, f) == (size_t)str->length;
}

static bool write_value(FILE* f, Value val);
static bool write_chunk(FILE* f, Chunk* chunk);

static bool write_function(FILE* f, ObjFunction* fn) {
    /* Function name */
    bool has_name = fn->name != NULL;
    if (!write_u8(f, has_name ? 1 : 0)) return false;
    if (has_name && !write_string(f, fn->name)) return false;
    
    /* Arity */
    if (!write_u8(f, fn->arity)) return false;
    
    /* Chunk */
    return write_chunk(f, fn->chunk);
}

static bool write_value(FILE* f, Value v) {
    if (v.obj == NULL || IS_NULL(v)) {
        return write_u8(f, TAG_NULL);
    }
    
    switch (v.obj->type) {
        case OBJ_BOOL: {
            if (!write_u8(f, TAG_BOOL)) return false;
            ObjBool* b = (ObjBool*)v.obj;
            return write_u8(f, b->value ? 1 : 0);
        }
        case OBJ_INT: {
            if (!write_u8(f, TAG_INT)) return false;
            ObjInt* i = (ObjInt*)v.obj;
            return write_i64(f, i->value);
        }
        case OBJ_FLOAT: {
            if (!write_u8(f, TAG_FLOAT)) return false;
            ObjFloat* fl = (ObjFloat*)v.obj;
            return write_f64(f, fl->value);
        }
        case OBJ_STRING: {
            if (!write_u8(f, TAG_STRING)) return false;
            return write_string(f, AS_STRING(v));
        }
        case OBJ_FUNCTION: {
            if (!write_u8(f, TAG_FUNC)) return false;
            return write_function(f, (ObjFunction*)v.obj);
        }
        default:
            return write_u8(f, TAG_NULL);
    }
}

static bool write_chunk(FILE* f, Chunk* chunk) {
    /* Constants */
    if (!write_u16(f, chunk->const_count)) return false;
    for (int i = 0; i < chunk->const_count; i++) {
        if (!write_value(f, chunk->constants[i])) return false;
    }
    
    /* Code */
    if (!write_u32(f, chunk->count)) return false;
    if (chunk->count > 0 && fwrite(chunk->code, 1, chunk->count, f) != (size_t)chunk->count) {
        return false;
    }
    
    /* Lines */
    for (int i = 0; i < chunk->count; i++) {
        if (!write_u32(f, chunk->lines[i])) return false;
    }
    
    return true;
}

bool bytecode_save(ObjFunction* fn, const char* path) {
    FILE* f = fopen(path, "wb");
    if (!f) return false;
    
    /* Magic number */
    if (fwrite(ZEX_MAGIC, 1, 12, f) != 12) {
        fclose(f);
        return false;
    }
    
    /* Version */
    if (!write_u16(f, ZEX_BYTECODE_VERSION)) {
        fclose(f);
        return false;
    }
    
    /* Function */
    bool ok = write_function(f, fn);
    fclose(f);
    return ok;
}

/* Reading functions */

static bool read_u8(FILE* f, uint8_t* val) {
    return fread(val, 1, 1, f) == 1;
}

static bool read_u16(FILE* f, uint16_t* val) {
    uint8_t bytes[2];
    if (fread(bytes, 1, 2, f) != 2) return false;
    *val = bytes[0] | (bytes[1] << 8);
    return true;
}

static bool read_u32(FILE* f, uint32_t* val) {
    uint8_t bytes[4];
    if (fread(bytes, 1, 4, f) != 4) return false;
    *val = bytes[0] | (bytes[1] << 8) | (bytes[2] << 16) | (bytes[3] << 24);
    return true;
}

static bool read_i64(FILE* f, int64_t* val) {
    return fread(val, sizeof(int64_t), 1, f) == 1;
}

static bool read_f64(FILE* f, double* val) {
    return fread(val, sizeof(double), 1, f) == 1;
}

static ObjString* read_string(FILE* f) {
    uint32_t len;
    if (!read_u32(f, &len)) return NULL;
    
    char* buf = ALLOCATE(char, len + 1);
    if (fread(buf, 1, len, f) != len) {
        FREE_ARRAY(char, buf, len + 1);
        return NULL;
    }
    buf[len] = '\0';
    
    ObjString* str = new_string(buf, len);
    FREE_ARRAY(char, buf, len + 1);
    return str;
}

static Value read_value(FILE* f);
static Chunk* read_chunk(FILE* f);

static ObjFunction* read_function(FILE* f) {
    /* Name */
    uint8_t has_name;
    if (!read_u8(f, &has_name)) return NULL;
    
    ObjString* name = NULL;
    if (has_name) {
        name = read_string(f);
        if (!name) return NULL;
    }
    
    /* Arity */
    uint8_t arity;
    if (!read_u8(f, &arity)) return NULL;
    
    /* Chunk */
    Chunk* chunk = read_chunk(f);
    if (!chunk) return NULL;
    
    ObjFunction* fn = new_function();
    fn->name = name;
    fn->arity = arity;
    fn->chunk = chunk;
    
    return fn;
}

static Value read_value(FILE* f) {
    uint8_t tag;
    if (!read_u8(f, &tag)) return NULL_VAL;
    
    switch (tag) {
        case TAG_NULL:
            return NULL_VAL;
        case TAG_BOOL: {
            uint8_t val;
            if (!read_u8(f, &val)) return NULL_VAL;
            return BOOL_VAL(val != 0);
        }
        case TAG_INT: {
            int64_t val;
            if (!read_i64(f, &val)) return NULL_VAL;
            return INT_VAL(val);
        }
        case TAG_FLOAT: {
            double val;
            if (!read_f64(f, &val)) return NULL_VAL;
            return FLOAT_VAL(val);
        }
        case TAG_STRING: {
            ObjString* str = read_string(f);
            if (!str) return NULL_VAL;
            return OBJ_VAL(str);
        }
        case TAG_FUNC: {
            ObjFunction* fn = read_function(f);
            if (!fn) return NULL_VAL;
            return OBJ_VAL(fn);
        }
        default:
            return NULL_VAL;
    }
}

static Chunk* read_chunk(FILE* f) {
    Chunk* chunk = ALLOCATE(Chunk, 1);
    chunk_init(chunk);
    
    /* Constants */
    uint16_t const_count;
    if (!read_u16(f, &const_count)) {
        chunk_free(chunk);
        FREE(Chunk, chunk);
        return NULL;
    }
    
    for (int i = 0; i < const_count; i++) {
        Value val = read_value(f);
        chunk_add_constant(chunk, val);
    }
    
    /* Code */
    uint32_t code_len;
    if (!read_u32(f, &code_len)) {
        chunk_free(chunk);
        FREE(Chunk, chunk);
        return NULL;
    }
    
    if (code_len > 0) {
        chunk->code = ALLOCATE(uint8_t, code_len);
        chunk->capacity = code_len;
        chunk->count = code_len;
        if (fread(chunk->code, 1, code_len, f) != code_len) {
            chunk_free(chunk);
            FREE(Chunk, chunk);
            return NULL;
        }
        
        /* Lines */
        chunk->lines = ALLOCATE(int, code_len);
        for (uint32_t i = 0; i < code_len; i++) {
            uint32_t line;
            if (!read_u32(f, &line)) {
                chunk_free(chunk);
                FREE(Chunk, chunk);
                return NULL;
            }
            chunk->lines[i] = line;
        }
    }
    
    return chunk;
}

ObjFunction* bytecode_load(const char* path) {
    FILE* f = fopen(path, "rb");
    if (!f) return NULL;
    
    /* Magic number */
    uint8_t magic[12];
    if (fread(magic, 1, 12, f) != 12 || memcmp(magic, ZEX_MAGIC, 12) != 0) {
        fclose(f);
        return NULL;
    }
    
    /* Version */
    uint16_t version;
    if (!read_u16(f, &version) || version != ZEX_BYTECODE_VERSION) {
        fclose(f);
        return NULL;
    }
    
    /* Function */
    ObjFunction* fn = read_function(f);
    fclose(f);
    return fn;
}

bool bytecode_is_compiled(const char* path) {
    FILE* f = fopen(path, "rb");
    if (!f) return false;
    
    uint8_t magic[12];
    bool is_compiled = false;
    
    if (fread(magic, 1, 12, f) == 12) {
        is_compiled = (memcmp(magic, ZEX_MAGIC, 12) == 0);
    }
    
    fclose(f);
    return is_compiled;
}
