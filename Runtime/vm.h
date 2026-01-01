/*
 * Zex Programming Language
 * Runtime/vm.h - Virtual machine interface
 *
 * Uses a large register pool with proper bounds checking.
 */

#ifndef ZEX_VM_H
#define ZEX_VM_H

#include "common.h"
#include "bytecode.h"
#include "object.h"
#include "tableobject.h"
#include "funcobject.h"

#define ZEX_MAX_FRAMES 256
#define ZEX_MAX_REGISTERS 8192

/* Call frame */
typedef struct {
    ObjFunction* function;
    uint8_t* ip;            /* Instruction pointer */
    Value* registers;       /* Base of register window */
    int reg_offset;         /* Offset into VM's register array */
} CallFrame;

/* Virtual machine state */
struct VM {
    CallFrame frames[ZEX_MAX_FRAMES];
    int frame_count;
    
    Value registers[ZEX_MAX_REGISTERS];
    int reg_top;            /* Track highest used register */
    
    Table globals;
    Table strings;          /* String interning table */
    
    /* Built-in classes */
    ObjClass* int_class;
    ObjClass* float_class;
    ObjClass* bool_class;
    ObjClass* null_class;
    ObjClass* string_class;
    
    /* GC tracking */
    Obj* objects;
    size_t bytes_allocated;
    size_t next_gc;
};

/* Initialize the VM */
void vm_init(VM* vm);

/* Free VM resources */
void vm_free(VM* vm);

/* Interpret source code  */
InterpretResult vm_interpret(VM* vm, const char* source);

/* Interpret compiled function */
InterpretResult vm_run(VM* vm, ObjFunction* function);

/* Define a native function */
void vm_define_native(VM* vm, const char* name, NativeFn function, int arity);

/* Get the global VM instance */
VM* vm_get(void);

/* Report runtime error with stack trace */
void vm_error(VM* vm, const char* format, ...);

#endif /* ZEX_VM_H */
