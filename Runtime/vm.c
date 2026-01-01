/*
 * Zex Programming Language
 * Runtime/vm.c - Virtual machine implementation
 */

#include "vm.h"
#include "compiler.h"
#include "error.h"
#include "memory.h"
#include "stringobject.h"
#include "intobject.h"
#include "floatobject.h"
#include "boolobject.h"
#include "nullobject.h"
#include "classobject.h"

/* Forward declare vm_run_frame */
static Value vm_run_frame(VM* vm);

/* Global VM instance */
static VM global_vm;

VM* vm_get(void) {
    return &global_vm;
}

void vm_runtime_error(VM* vm, const char* format, ...) {
    va_list args;
    va_start(args, format);
    
    fprintf(stderr, "\n");
    fprintf(stderr, "\033[31m\033[1m── Runtime Error ─────────────────────────────────\033[0m\n");
    fprintf(stderr, "\n");
    
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
    
    va_end(args);
    
    /* Print stack trace */
    fprintf(stderr, "\nStack trace:\n");
    const char* filename = error_get_filename();
    
    for (int i = vm->frame_count - 1; i >= 0; i--) {
        CallFrame* frame = &vm->frames[i];
        ObjFunction* function = frame->function;
        size_t instruction = frame->ip - function->chunk->code - 1;
        int line = function->chunk->lines[instruction];
        
        fprintf(stderr, "  [line %d] in ", line);
        if (function->name == NULL) {
            fprintf(stderr, "<script>");
        } else {
            fprintf(stderr, "%s()", function->name->chars);
        }
        
        if (filename != NULL) {
            fprintf(stderr, " (%s)", filename);
        }
        fprintf(stderr, "\n");
    }
    fprintf(stderr, "\n");
}

void vm_init(VM* vm) {
    vm->frame_count = 0;
    vm->reg_top = 0;
    vm->objects = NULL;
    vm->bytes_allocated = 0;
    vm->next_gc = 1024 * 1024;
    
    table_init(&vm->globals);
    table_init(&vm->strings);
    
    /* Initialize string interning */
    init_string_intern();
    
    /* Initialize built-in classes */
    init_null_class();
    init_bool_class();
    init_string_class();
    init_int_class();
    init_float_class();
    
    vm->null_class = get_null_class();
    vm->bool_class = get_bool_class();
    vm->string_class = get_string_class();
    vm->int_class = get_int_class();
    vm->float_class = get_float_class();
    
    /* Initialize registers to null */
    for (int i = 0; i < ZEX_MAX_REGISTERS; i++) {
        vm->registers[i] = NULL_VAL;
    }
}

void vm_free(VM* vm) {
    table_free(&vm->globals);
    table_free(&vm->strings);
    free_string_intern();
    free_objects();
    vm->frame_count = 0;
    vm->reg_top = 0;
}

void vm_define_native(VM* vm, const char* name, NativeFn function, int arity) {
    ObjString* name_str = new_string_cstr(name);
    ObjNative* native = new_native(function, arity, name);
    table_set(&vm->globals, name_str, OBJ_VAL(native));
}

/* Slots per frame - must be enough for any single function's locals */
#define FRAME_SLOTS 32

static bool call_function(VM* vm, ObjFunction* function, int argc, Value* args) {
    if (argc != function->arity) {
        vm_runtime_error(vm, "Expected %d arguments but got %d", function->arity, argc);
        return false;
    }
    
    if (vm->frame_count >= ZEX_MAX_FRAMES) {
        vm_runtime_error(vm, "Stack overflow: exceeded %d frames", ZEX_MAX_FRAMES);
        return false;
    }
    
    /* Calculate register window */
    int reg_offset = vm->reg_top;
    
    if (reg_offset + FRAME_SLOTS > ZEX_MAX_REGISTERS) {
        vm_runtime_error(vm, "Stack overflow: exceeded %d registers", ZEX_MAX_REGISTERS);
        return false;
    }
    
    CallFrame* frame = &vm->frames[vm->frame_count++];
    frame->function = function;
    frame->ip = function->chunk->code;
    frame->reg_offset = reg_offset;
    frame->registers = &vm->registers[reg_offset];
    
    vm->reg_top = reg_offset + FRAME_SLOTS;
    
    /* Copy arguments to new frame's registers */
    for (int i = 0; i < argc; i++) {
        frame->registers[i] = args[i];
    }
    
    return true;
}

static bool call_value(VM* vm, Value callee, int argc, Value* args, Value* result) {
    if (callee.obj == NULL) {
        vm_runtime_error(vm, "Cannot call null");
        return false;
    }
    
    switch (callee.obj->type) {
        case OBJ_FUNCTION: {
            ObjFunction* func = (ObjFunction*)callee.obj;
            if (!call_function(vm, func, argc, args)) {
                return false;
            }
            *result = vm_run_frame(vm);
            return true;
        }
        
        case OBJ_NATIVE: {
            ObjNative* native = (ObjNative*)callee.obj;
            if (native->arity >= 0 && argc != native->arity) {
                vm_runtime_error(vm, "Expected %d arguments but got %d", 
                                native->arity, argc);
                return false;
            }
            *result = native->function(vm, argc, args);
            return true;
        }
        
        case OBJ_CLASS: {
            ObjClass* klass = (ObjClass*)callee.obj;
            
            /* Check if this is a built-in type for conversion */
            if (argc == 1) {
                Value arg = args[0];
                
                if (klass == vm->int_class) {
                    if (IS_INT(arg)) {
                        *result = arg;
                        return true;
                    }
                    if (IS_FLOAT(arg)) {
                        ObjFloat* f = (ObjFloat*)arg.obj;
                        *result = INT_VAL((int64_t)f->value);
                        return true;
                    }
                    if (IS_STRING(arg)) {
                        ObjString* s = (ObjString*)arg.obj;
                        int64_t val = strtoll(s->chars, NULL, 10);
                        *result = INT_VAL(val);
                        return true;
                    }
                    if (IS_BOOL(arg)) {
                        ObjBool* b = (ObjBool*)arg.obj;
                        *result = INT_VAL(b->value ? 1 : 0);
                        return true;
                    }
                    vm_runtime_error(vm, "Cannot convert '%s' to int", value_type_name(arg));
                    return false;
                }
                
                if (klass == vm->float_class) {
                    if (IS_FLOAT(arg)) {
                        *result = arg;
                        return true;
                    }
                    if (IS_INT(arg)) {
                        ObjInt* i = (ObjInt*)arg.obj;
                        *result = FLOAT_VAL((double)i->value);
                        return true;
                    }
                    if (IS_STRING(arg)) {
                        ObjString* s = (ObjString*)arg.obj;
                        double val = strtod(s->chars, NULL);
                        *result = FLOAT_VAL(val);
                        return true;
                    }
                    vm_runtime_error(vm, "Cannot convert '%s' to float", value_type_name(arg));
                    return false;
                }
                
                if (klass == vm->string_class) {
                    ObjString* str = value_to_string(arg);
                    *result = STRING_VAL(str);
                    return true;
                }
                
                if (klass == vm->bool_class) {
                    *result = BOOL_VAL(is_truthy(arg));
                    return true;
                }
            }
            
            /* Regular class instantiation */
            ObjInstance* instance = new_instance(klass);
            *result = OBJ_VAL(instance);
            
            Value init_val;
            ObjString* init_name = new_string_cstr("init");
            if (table_get(&klass->methods, init_name, &init_val)) {
                Value new_args[256];
                new_args[0] = OBJ_VAL(instance);
                for (int i = 0; i < argc; i++) {
                    new_args[i + 1] = args[i];
                }
                ObjFunction* init_func = (ObjFunction*)init_val.obj;
                if (!call_function(vm, init_func, argc + 1, new_args)) {
                    return false;
                }
                vm_run_frame(vm);
            }
            return true;
        }
        
        case OBJ_BOUND_METHOD: {
            ObjBoundMethod* bound = (ObjBoundMethod*)callee.obj;
            Value new_args[256];
            new_args[0] = bound->receiver;
            for (int i = 0; i < argc; i++) {
                new_args[i + 1] = args[i];
            }
            if (!call_function(vm, bound->method, argc + 1, new_args)) {
                return false;
            }
            *result = vm_run_frame(vm);
            return true;
        }
        
        default:
            vm_runtime_error(vm, "Cannot call value of type '%s'", 
                            value_type_name(callee));
            return false;
    }
}

static bool binary_op(VM* vm, OpCode op, Value a, Value b, Value* result) {
    if (IS_INT(a) && IS_INT(b)) {
        ObjInt* ia = (ObjInt*)a.obj;
        ObjInt* ib = (ObjInt*)b.obj;
        int64_t va = ia->value;
        int64_t vb = ib->value;
        
        switch (op) {
            case OP_ADD: *result = INT_VAL(va + vb); return true;
            case OP_SUB: *result = INT_VAL(va - vb); return true;
            case OP_MUL: *result = INT_VAL(va * vb); return true;
            case OP_DIV: 
                if (vb == 0) {
                    vm_runtime_error(vm, "Division by zero");
                    return false;
                }
                *result = INT_VAL(va / vb); 
                return true;
            case OP_EQ:  *result = BOOL_VAL(va == vb); return true;
            case OP_NE:  *result = BOOL_VAL(va != vb); return true;
            case OP_LT:  *result = BOOL_VAL(va < vb); return true;
            case OP_LE:  *result = BOOL_VAL(va <= vb); return true;
            case OP_GT:  *result = BOOL_VAL(va > vb); return true;
            case OP_GE:  *result = BOOL_VAL(va >= vb); return true;
            default: break;
        }
    }
    
    if ((IS_INT(a) || IS_FLOAT(a)) && (IS_INT(b) || IS_FLOAT(b))) {
        double va, vb;
        if (IS_INT(a)) {
            va = (double)((ObjInt*)a.obj)->value;
        } else {
            va = ((ObjFloat*)a.obj)->value;
        }
        if (IS_INT(b)) {
            vb = (double)((ObjInt*)b.obj)->value;
        } else {
            vb = ((ObjFloat*)b.obj)->value;
        }
        
        switch (op) {
            case OP_ADD: *result = FLOAT_VAL(va + vb); return true;
            case OP_SUB: *result = FLOAT_VAL(va - vb); return true;
            case OP_MUL: *result = FLOAT_VAL(va * vb); return true;
            case OP_DIV: 
                if (vb == 0.0) {
                    vm_runtime_error(vm, "Division by zero");
                    return false;
                }
                *result = FLOAT_VAL(va / vb); 
                return true;
            case OP_EQ:  *result = BOOL_VAL(va == vb); return true;
            case OP_NE:  *result = BOOL_VAL(va != vb); return true;
            case OP_LT:  *result = BOOL_VAL(va < vb); return true;
            case OP_LE:  *result = BOOL_VAL(va <= vb); return true;
            case OP_GT:  *result = BOOL_VAL(va > vb); return true;
            case OP_GE:  *result = BOOL_VAL(va >= vb); return true;
            default: break;
        }
    }
    
    if (IS_STRING(a) && IS_STRING(b) && op == OP_ADD) {
        ObjString* sa = (ObjString*)a.obj;
        ObjString* sb = (ObjString*)b.obj;
        *result = STRING_VAL(string_concat(sa, sb));
        return true;
    }
    
    if (IS_STRING(a) && op == OP_ADD) {
        ObjString* sa = (ObjString*)a.obj;
        ObjString* sb = value_to_string(b);
        *result = STRING_VAL(string_concat(sa, sb));
        return true;
    }
    
    if (IS_STRING(b) && op == OP_ADD) {
        ObjString* sa = value_to_string(a);
        ObjString* sb = (ObjString*)b.obj;
        *result = STRING_VAL(string_concat(sa, sb));
        return true;
    }
    
    if (op == OP_EQ) {
        *result = BOOL_VAL(values_equal(a, b));
        return true;
    }
    if (op == OP_NE) {
        *result = BOOL_VAL(!values_equal(a, b));
        return true;
    }
    
    vm_runtime_error(vm, "Cannot perform operation on '%s' and '%s'",
                    value_type_name(a), value_type_name(b));
    return false;
}

static Value vm_run_frame(VM* vm) {
    CallFrame* frame = &vm->frames[vm->frame_count - 1];
    
#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() (frame->ip += 2, (uint16_t)((frame->ip[-2]) | (frame->ip[-1] << 8)))
#define READ_CONSTANT() (frame->function->chunk->constants[READ_SHORT()])
#define REG(r) (frame->registers[r])
    
    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");
        for (int i = 0; i < 8; i++) {
            printf("R%d=", i);
            print_value(REG(i));
            printf(" ");
        }
        printf("\n");
        chunk_disassemble_instruction(frame->function->chunk, 
                                      (int)(frame->ip - frame->function->chunk->code));
#endif
        
        uint8_t instruction = READ_BYTE();
        
        switch (instruction) {
            case OP_LOAD_CONST: {
                uint8_t reg = READ_BYTE();
                Value constant = READ_CONSTANT();
                REG(reg) = constant;
                break;
            }
            
            case OP_LOAD_NULL: {
                uint8_t reg = READ_BYTE();
                REG(reg) = NULL_VAL;
                break;
            }
            
            case OP_LOAD_TRUE: {
                uint8_t reg = READ_BYTE();
                REG(reg) = BOOL_VAL(true);
                break;
            }
            
            case OP_LOAD_FALSE: {
                uint8_t reg = READ_BYTE();
                REG(reg) = BOOL_VAL(false);
                break;
            }
            
            case OP_MOVE: {
                uint8_t dst = READ_BYTE();
                uint8_t src = READ_BYTE();
                REG(dst) = REG(src);
                break;
            }
            
            case OP_GET_GLOBAL: {
                uint8_t reg = READ_BYTE();
                ObjString* name = (ObjString*)READ_CONSTANT().obj;
                Value value;
                if (!table_get(&vm->globals, name, &value)) {
                    vm_runtime_error(vm, "Undefined variable '%s'", name->chars);
                    return NULL_VAL;
                }
                REG(reg) = value;
                break;
            }
            
            case OP_SET_GLOBAL: {
                ObjString* name = (ObjString*)READ_CONSTANT().obj;
                uint8_t reg = READ_BYTE();
                table_set(&vm->globals, name, REG(reg));
                break;
            }
            
            case OP_DEF_GLOBAL: {
                ObjString* name = (ObjString*)READ_CONSTANT().obj;
                uint8_t reg = READ_BYTE();
                table_set(&vm->globals, name, REG(reg));
                break;
            }
            
            case OP_GET_LOCAL: {
                uint8_t dst = READ_BYTE();
                uint8_t slot = READ_BYTE();
                REG(dst) = REG(slot);
                break;
            }
            
            case OP_SET_LOCAL: {
                uint8_t slot = READ_BYTE();
                uint8_t src = READ_BYTE();
                REG(slot) = REG(src);
                break;
            }
            
            case OP_ADD:
            case OP_SUB:
            case OP_MUL:
            case OP_DIV:
            case OP_EQ:
            case OP_NE:
            case OP_LT:
            case OP_LE:
            case OP_GT:
            case OP_GE: {
                uint8_t dst = READ_BYTE();
                uint8_t ra = READ_BYTE();
                uint8_t rb = READ_BYTE();
                Value result;
                if (!binary_op(vm, instruction, REG(ra), REG(rb), &result)) {
                    return NULL_VAL;
                }
                REG(dst) = result;
                break;
            }
            
            case OP_NEG: {
                uint8_t dst = READ_BYTE();
                uint8_t src = READ_BYTE();
                Value v = REG(src);
                if (IS_INT(v)) {
                    ObjInt* i = (ObjInt*)v.obj;
                    REG(dst) = INT_VAL(-i->value);
                } else if (IS_FLOAT(v)) {
                    ObjFloat* f = (ObjFloat*)v.obj;
                    REG(dst) = FLOAT_VAL(-f->value);
                } else {
                    vm_runtime_error(vm, "Cannot negate '%s'", value_type_name(v));
                    return NULL_VAL;
                }
                break;
            }
            
            case OP_NOT: {
                uint8_t dst = READ_BYTE();
                uint8_t src = READ_BYTE();
                REG(dst) = BOOL_VAL(!is_truthy(REG(src)));
                break;
            }
            
            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                break;
            }
            
            case OP_JUMP_BACK: {
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                break;
            }
            
            case OP_JUMP_IF_FALSE: {
                uint8_t reg = READ_BYTE();
                uint16_t offset = READ_SHORT();
                if (!is_truthy(REG(reg))) {
                    frame->ip += offset;
                }
                break;
            }
            
            case OP_JUMP_IF_TRUE: {
                uint8_t reg = READ_BYTE();
                uint16_t offset = READ_SHORT();
                if (is_truthy(REG(reg))) {
                    frame->ip += offset;
                }
                break;
            }
            
            case OP_CALL: {
                uint8_t callee_reg = READ_BYTE();
                uint8_t argc = READ_BYTE();
                uint8_t arg_base = READ_BYTE();
                
                Value callee = REG(callee_reg);
                Value args[256];
                for (int i = 0; i < argc; i++) {
                    args[i] = REG(arg_base + i);
                }
                
                Value result;
                if (!call_value(vm, callee, argc, args, &result)) {
                    return NULL_VAL;
                }
                
                frame = &vm->frames[vm->frame_count - 1];
                REG(callee_reg) = result;
                break;
            }
            
            case OP_RETURN: {
                uint8_t reg = READ_BYTE();
                Value result = REG(reg);
                
                /* Pop frame and free its registers */
                vm->reg_top = frame->reg_offset;
                vm->frame_count--;
                
                return result;
            }
            
            case OP_CLOSURE: {
                uint8_t reg = READ_BYTE();
                Value func_val = READ_CONSTANT();
                REG(reg) = func_val;
                break;
            }
            
            case OP_CLASS: {
                uint8_t reg = READ_BYTE();
                ObjString* name = (ObjString*)READ_CONSTANT().obj;
                ObjClass* klass = new_class(name);
                REG(reg) = OBJ_VAL(klass);
                break;
            }
            
            case OP_GET_PROPERTY: {
                uint8_t dst = READ_BYTE();
                uint8_t obj_reg = READ_BYTE();
                ObjString* name = (ObjString*)READ_CONSTANT().obj;
                
                Value obj_val = REG(obj_reg);
                
                if (IS_INSTANCE(obj_val)) {
                    ObjInstance* instance = (ObjInstance*)obj_val.obj;
                    
                    Value value;
                    if (table_get(&instance->properties, name, &value)) {
                        REG(dst) = value;
                        break;
                    }
                    
                    ObjClass* klass = instance->obj.klass;
                    if (table_get(&klass->methods, name, &value)) {
                        ObjFunction* method = (ObjFunction*)value.obj;
                        ObjBoundMethod* bound = new_bound_method(obj_val, method);
                        REG(dst) = OBJ_VAL(bound);
                        break;
                    }
                    
                    vm_runtime_error(vm, "Undefined property '%s'", name->chars);
                    return NULL_VAL;
                    
                } else if (IS_CLASS(obj_val)) {
                    ObjClass* klass = (ObjClass*)obj_val.obj;
                    Value value;
                    if (table_get(&klass->methods, name, &value)) {
                        REG(dst) = value;
                        break;
                    }
                    vm_runtime_error(vm, "Undefined method '%s' on class", name->chars);
                    return NULL_VAL;
                }
                
                vm_runtime_error(vm, "Only instances have properties");
                return NULL_VAL;
            }
            
            case OP_SET_PROPERTY: {
                uint8_t obj_reg = READ_BYTE();
                ObjString* name = (ObjString*)READ_CONSTANT().obj;
                uint8_t val_reg = READ_BYTE();
                
                Value obj_val = REG(obj_reg);
                if (!IS_INSTANCE(obj_val)) {
                    vm_runtime_error(vm, "Only instances have properties");
                    return NULL_VAL;
                }
                
                ObjInstance* instance = (ObjInstance*)obj_val.obj;
                table_set(&instance->properties, name, REG(val_reg));
                break;
            }
            
            case OP_METHOD: {
                uint8_t class_reg = READ_BYTE();
                ObjString* name = (ObjString*)READ_CONSTANT().obj;
                uint8_t method_reg = READ_BYTE();
                
                ObjClass* klass = (ObjClass*)REG(class_reg).obj;
                table_set(&klass->methods, name, REG(method_reg));
                break;
            }
            
            case OP_INVOKE: {
                uint8_t dst = READ_BYTE();
                uint8_t obj_reg = READ_BYTE();
                ObjString* name = (ObjString*)READ_CONSTANT().obj;
                uint8_t argc = READ_BYTE();
                
                Value obj_val = REG(obj_reg);
                
                if (!IS_INSTANCE(obj_val)) {
                    vm_runtime_error(vm, "Only instances have methods");
                    return NULL_VAL;
                }
                
                ObjInstance* instance = (ObjInstance*)obj_val.obj;
                ObjClass* klass = instance->obj.klass;
                
                Value method;
                if (!table_get(&klass->methods, name, &method)) {
                    vm_runtime_error(vm, "Undefined method '%s'", name->chars);
                    return NULL_VAL;
                }
                
                Value args[256];
                args[0] = obj_val;
                for (int i = 0; i < argc; i++) {
                    args[i + 1] = REG(dst + 1 + i);
                }
                
                ObjFunction* func = (ObjFunction*)method.obj;
                if (!call_function(vm, func, argc + 1, args)) {
                    return NULL_VAL;
                }
                
                Value result = vm_run_frame(vm);
                frame = &vm->frames[vm->frame_count - 1];
                REG(dst) = result;
                break;
            }
            
            case OP_INHERIT: {
                uint8_t subclass_reg = READ_BYTE();
                uint8_t superclass_reg = READ_BYTE();
                
                Value superclass_val = REG(superclass_reg);
                if (superclass_val.obj == NULL || superclass_val.obj->type != OBJ_CLASS) {
                    vm_runtime_error(vm, "Superclass must be a class");
                    return NULL_VAL;
                }
                
                ObjClass* superclass = (ObjClass*)superclass_val.obj;
                ObjClass* subclass = (ObjClass*)REG(subclass_reg).obj;
                
                /* Copy all methods from superclass to subclass */
                table_add_all(&superclass->methods, &subclass->methods);
                
                /* Set superclass pointer */
                subclass->superclass = superclass;
                break;
            }
            
            default:
                vm_runtime_error(vm, "Unknown opcode %d", instruction);
                return NULL_VAL;
        }
    }
    
#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef REG
}

InterpretResult vm_run(VM* vm, ObjFunction* function) {
    if (function == NULL) {
        return INTERPRET_COMPILE_ERROR;
    }
    
    /* Set up initial frame */
    CallFrame* frame = &vm->frames[vm->frame_count++];
    frame->function = function;
    frame->ip = function->chunk->code;
    frame->registers = vm->registers;
    frame->reg_offset = 0;
    
    vm->reg_top = FRAME_SLOTS;
    
    Value result = vm_run_frame(vm);
    UNUSED(result);
    
    return INTERPRET_OK;
}

InterpretResult vm_interpret(VM* vm, const char* source) {
    error_init("<script>", source);
    
    CompileResult compiled = compile_source(source);
    
    if (compiled.had_error || compiled.function == NULL) {
        return INTERPRET_COMPILE_ERROR;
    }
    
#ifdef DEBUG_PRINT_CODE
    chunk_disassemble(compiled.function->chunk, "<script>");
#endif
    
    return vm_run(vm, compiled.function);
}
