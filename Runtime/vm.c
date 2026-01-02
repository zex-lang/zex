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
#include "arrayobject.h"
#include "exceptionobject.h"
#include <math.h>

/* Forward declare vm_run_frame */
static Value vm_run_frame(VM* vm);

/* Global VM instance */
static VM global_vm;

VM* vm_get(void) {
    return &global_vm;
}

void vm_error(VM* vm, const char* format, ...) {
    /* Push stack trace frames to error system */
    error_clear_frames();
    for (int i = vm->frame_count - 1; i >= 0; i--) {
        CallFrame* frame = &vm->frames[i];
        ObjFunction* function = frame->function;
        size_t instruction = frame->ip - function->chunk->code - 1;
        int line = function->chunk->lines[instruction];
        
        const char* name = function->name ? function->name->chars : NULL;
        error_push_frame(name, line, 1);
    }
    
    /* Get line info from current frame */
    int line = 0;
    if (vm->frame_count > 0) {
        CallFrame* frame = &vm->frames[vm->frame_count - 1];
        size_t instruction = frame->ip - frame->function->chunk->code - 1;
        line = frame->function->chunk->lines[instruction];
    }
    
    /* Format message and call unified error */
    char buffer[1024];
    va_list args;
    va_start(args, format);
    vsnprintf(buffer, sizeof(buffer), format, args);
    va_end(args);
    
    zex_error(ERROR_RUNTIME, line, 0, 0, "%s", buffer);
}

/* 
 * Create a runtime exception and store it in the VM.
 * Returns true if an exception handler exists, false otherwise.
 * The caller should check the return value and either continue (if true)
 * or return (if false).
 */
static bool vm_runtime_exception(VM* vm, ObjClass* exc_class, const char* format, ...) {
    /* Get line info from current frame */
    int line = 0;
    if (vm->frame_count > 0) {
        CallFrame* frame = &vm->frames[vm->frame_count - 1];
        size_t instruction = frame->ip - frame->function->chunk->code - 1;
        line = frame->function->chunk->lines[instruction];
    }
    
    /* Format message */
    char buffer[1024];
    va_list args;
    va_start(args, format);
    vsnprintf(buffer, sizeof(buffer), format, args);
    va_end(args);
    
    /* Create exception object */
    ObjException* exc = new_exception_cstr(exc_class, buffer, line, 0, 0);
    vm->current_exception = OBJ_VAL(exc);
    vm->has_exception = true;
    
    /* Check if there's a handler */
    if (vm->exception_handler_count > 0) {
        return true;  /* Handler exists, caller should jump to it */
    }
    
    /* No handler - print exception with class name */
    error_clear_frames();
    for (int i = vm->frame_count - 1; i >= 0; i--) {
        CallFrame* frame = &vm->frames[i];
        ObjFunction* function = frame->function;
        size_t instruction = frame->ip - function->chunk->code - 1;
        int frame_line = function->chunk->lines[instruction];
        const char* name = function->name ? function->name->chars : NULL;
        error_push_frame(name, frame_line, 1);
    }
    
    const char* class_name = exc_class && exc_class->name ? exc_class->name->chars : "Exception";
    zex_exception(class_name, line, 0, 0, "%s", buffer);
    return false;
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
    init_array_class();
    
    vm->null_class = get_null_class();
    vm->bool_class = get_bool_class();
    vm->string_class = get_string_class();
    vm->int_class = get_int_class();
    vm->float_class = get_float_class();
    
    /* Initialize exception handling */
    init_exception_classes();
    vm->exception_handler_count = 0;
    vm->current_exception = NULL_VAL;
    vm->has_exception = false;
    
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
        vm_error(vm, "Expected %d arguments but got %d", function->arity, argc);
        return false;
    }
    
    if (vm->frame_count >= ZEX_MAX_FRAMES) {
        vm_error(vm, "Stack overflow: exceeded %d frames", ZEX_MAX_FRAMES);
        return false;
    }
    
    /* Calculate register window */
    int reg_offset = vm->reg_top;
    
    if (reg_offset + FRAME_SLOTS > ZEX_MAX_REGISTERS) {
        vm_error(vm, "Stack overflow: exceeded %d registers", ZEX_MAX_REGISTERS);
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
        vm_error(vm, "Cannot call null");
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
                vm_error(vm, "Expected %d arguments but got %d", 
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
                    vm_error(vm, "Cannot convert '%s' to int", value_type_name(arg));
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
                    vm_error(vm, "Cannot convert '%s' to float", value_type_name(arg));
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
            
            if (IS_NATIVE(bound->method)) {
                ObjNative* native = AS_NATIVE(bound->method);
                if (native->arity >= 0 && (argc + 1) != native->arity) {
                    vm_error(vm, "Expected %d arguments but got %d", 
                                    native->arity, argc + 1);
                    return false;
                }
                *result = native->function(vm, argc + 1, new_args);
                return true;
            } else if (IS_FUNCTION(bound->method)) {
                ObjFunction* func = AS_FUNCTION(bound->method);
                if (!call_function(vm, func, argc + 1, new_args)) {
                    return false;
                }
                *result = vm_run_frame(vm);
                return true;
            } else {
                vm_error(vm, "Invalid bound method type");
                return false;
            }
        }
        
        default:
            vm_error(vm, "Cannot call value of type '%s'", 
                            value_type_name(callee));
            return false;
    }
}

/* Public API wrapper for call_value */
bool vm_call_value(VM* vm, Value callee, int argc, Value* args, Value* result) {
    return call_value(vm, callee, argc, args, result);
}

/* Get the class for any value (for method lookup) */
static ObjClass* get_value_class(Value val) {
    if (IS_INSTANCE(val)) return ((ObjInstance*)val.obj)->obj.klass;
    if (IS_ARRAY(val)) return get_array_class();
    if (IS_STRING(val)) return get_string_class();
    if (IS_INT(val)) return get_int_class();
    if (IS_FLOAT(val)) return get_float_class();
    if (IS_BOOL(val)) return get_bool_class();
    return NULL;
}

/* Look up method on any value */
static bool get_builtin_method(Value receiver, ObjString* name, Value* method) {
    ObjClass* klass = get_value_class(receiver);
    if (klass && table_get(&klass->methods, name, method)) {
        return true;
    }
    return false;
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
                    vm_runtime_exception(vm, get_zero_division_error_class(), "Division by zero");
                    return false;
                }
                *result = INT_VAL(va / vb); 
                return true;
            case OP_MOD:
                if (vb == 0) {
                    vm_runtime_exception(vm, get_zero_division_error_class(), "Modulo by zero");
                    return false;
                }
                *result = INT_VAL(va % vb);
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
                    vm_runtime_exception(vm, get_zero_division_error_class(), "Division by zero");
                    return false;
                }
                *result = FLOAT_VAL(va / vb); 
                return true;
            case OP_MOD:
                if (vb == 0.0) {
                    vm_runtime_exception(vm, get_zero_division_error_class(), "Modulo by zero");
                    return false;
                }
                *result = FLOAT_VAL(fmod(va, vb));
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
    
    vm_error(vm, "Cannot perform operation on '%s' and '%s'",
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
                    vm_error(vm, "Undefined variable '%s'", name->chars);
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
            case OP_MOD:
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
                    /* Check if exception was raised and we have a handler */
                    if (vm->has_exception && vm->exception_handler_count > 0) {
                        ExceptionHandler* handler = &vm->exception_handlers[--vm->exception_handler_count];
                        while (vm->frame_count > handler->frame_index + 1) {
                            vm->frame_count--;
                        }
                        vm->reg_top = handler->reg_top;
                        frame = &vm->frames[vm->frame_count - 1];
                        frame->ip = handler->handler_ip;
                        break;
                    }
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
                    vm_error(vm, "Cannot negate '%s'", value_type_name(v));
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
                        ObjBoundMethod* bound = new_bound_method(obj_val, value);
                        REG(dst) = OBJ_VAL(bound);
                        break;
                    }
                    
                    vm_error(vm, "Undefined property '%s'", name->chars);
                    return NULL_VAL;
                    
                } else if (IS_CLASS(obj_val)) {
                    ObjClass* klass = (ObjClass*)obj_val.obj;
                    Value value;
                    if (table_get(&klass->methods, name, &value)) {
                        REG(dst) = value;
                        break;
                    }
                    vm_error(vm, "Undefined method '%s' on class", name->chars);
                    return NULL_VAL;
                } else {
                    /* Builtin type method lookup */
                    Value method;
                    if (get_builtin_method(obj_val, name, &method)) {
                        ObjBoundMethod* bound = new_bound_method(obj_val, method);
                        REG(dst) = OBJ_VAL(bound);
                        break;
                    }
                }
                
                vm_error(vm, "Undefined property '%s' on '%s'", name->chars, value_type_name(obj_val));
                return NULL_VAL;
            }
            
            case OP_SET_PROPERTY: {
                uint8_t obj_reg = READ_BYTE();
                ObjString* name = (ObjString*)READ_CONSTANT().obj;
                uint8_t val_reg = READ_BYTE();
                
                Value obj_val = REG(obj_reg);
                if (!IS_INSTANCE(obj_val)) {
                    vm_error(vm, "Only instances have properties");
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
                
                if (IS_INSTANCE(obj_val)) {
                    ObjInstance* instance = (ObjInstance*)obj_val.obj;
                    ObjClass* klass = instance->obj.klass;
                    
                    Value method;
                    if (!table_get(&klass->methods, name, &method)) {
                        vm_error(vm, "Undefined method '%s'", name->chars);
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
                } else {
                    /* Builtin type method call */
                    Value method;
                    if (!get_builtin_method(obj_val, name, &method)) {
                        vm_error(vm, "Undefined method '%s' on '%s'", name->chars, value_type_name(obj_val));
                        return NULL_VAL;
                    }
                    
                    /* Builtin methods are native functions */
                    ObjNative* native = (ObjNative*)method.obj;
                    
                    Value args[256];
                    args[0] = obj_val;  /* self */
                    for (int i = 0; i < argc; i++) {
                        args[i + 1] = REG(dst + 1 + i);
                    }
                    
                    REG(dst) = native->function(vm, argc + 1, args);
                }
                break;
            }
            
            case OP_INHERIT: {
                uint8_t subclass_reg = READ_BYTE();
                uint8_t superclass_reg = READ_BYTE();
                
                Value superclass_val = REG(superclass_reg);
                if (superclass_val.obj == NULL || superclass_val.obj->type != OBJ_CLASS) {
                    vm_error(vm, "Superclass must be a class");
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
            
            case OP_ARRAY: {
                uint8_t dst = READ_BYTE();
                uint8_t count = READ_BYTE();
                uint8_t start_reg = READ_BYTE();
                
                ObjArray* arr = new_array_with_capacity(count);
                for (int i = 0; i < count; i++) {
                    array_push(arr, REG(start_reg + i));
                }
                
                REG(dst) = OBJ_VAL(arr);
                break;
            }
            
            case OP_INDEX_GET: {
                uint8_t dst = READ_BYTE();
                uint8_t arr_reg = READ_BYTE();
                uint8_t idx_reg = READ_BYTE();
                
                Value arr_val = REG(arr_reg);
                Value idx_val = REG(idx_reg);
                
                if (!IS_INT(idx_val)) {
                    if (vm_runtime_exception(vm, get_type_error_class(), "Index must be an integer")) {
                        ExceptionHandler* handler = &vm->exception_handlers[--vm->exception_handler_count];
                        while (vm->frame_count > handler->frame_index + 1) vm->frame_count--;
                        vm->reg_top = handler->reg_top;
                        frame = &vm->frames[vm->frame_count - 1];
                        frame->ip = handler->handler_ip;
                        break;
                    }
                    return NULL_VAL;
                }
                
                int idx = (int)AS_INT(idx_val);
                
                if (IS_ARRAY(arr_val)) {
                    ObjArray* arr = AS_ARRAY(arr_val);
                    
                    /* Handle negative index */
                    if (idx < 0) idx = arr->count + idx;
                    
                    if (idx < 0 || idx >= arr->count) {
                        if (vm_runtime_exception(vm, get_index_error_class(), 
                                "Array index out of bounds: %d (size: %d)", idx, arr->count)) {
                            ExceptionHandler* handler = &vm->exception_handlers[--vm->exception_handler_count];
                            while (vm->frame_count > handler->frame_index + 1) vm->frame_count--;
                            vm->reg_top = handler->reg_top;
                            frame = &vm->frames[vm->frame_count - 1];
                            frame->ip = handler->handler_ip;
                            break;
                        }
                        return NULL_VAL;
                    }
                    
                    REG(dst) = arr->items[idx];
                } else if (IS_STRING(arr_val)) {
                    ObjString* str = AS_STRING(arr_val);
                    
                    /* Handle negative index */
                    if (idx < 0) idx = str->char_count + idx;
                    
                    if (idx < 0 || idx >= str->char_count) {
                        if (vm_runtime_exception(vm, get_index_error_class(),
                                "String index out of bounds: %d (length: %d)", idx, str->char_count)) {
                            ExceptionHandler* handler = &vm->exception_handlers[--vm->exception_handler_count];
                            while (vm->frame_count > handler->frame_index + 1) vm->frame_count--;
                            vm->reg_top = handler->reg_top;
                            frame = &vm->frames[vm->frame_count - 1];
                            frame->ip = handler->handler_ip;
                            break;
                        }
                        return NULL_VAL;
                    }
                    
                    ObjString* ch = string_char_at(str, idx);
                    REG(dst) = OBJ_VAL(ch);
                } else {
                    if (vm_runtime_exception(vm, get_type_error_class(), "Can only index arrays and strings")) {
                        ExceptionHandler* handler = &vm->exception_handlers[--vm->exception_handler_count];
                        while (vm->frame_count > handler->frame_index + 1) vm->frame_count--;
                        vm->reg_top = handler->reg_top;
                        frame = &vm->frames[vm->frame_count - 1];
                        frame->ip = handler->handler_ip;
                        break;
                    }
                    return NULL_VAL;
                }
                break;
            }
            
            case OP_INDEX_SET: {
                uint8_t arr_reg = READ_BYTE();
                uint8_t idx_reg = READ_BYTE();
                uint8_t val_reg = READ_BYTE();
                
                Value arr_val = REG(arr_reg);
                Value idx_val = REG(idx_reg);
                
                if (!IS_ARRAY(arr_val)) {
                    vm_error(vm, "Can only index arrays");
                    return NULL_VAL;
                }
                if (!IS_INT(idx_val)) {
                    vm_error(vm, "Array index must be an integer");
                    return NULL_VAL;
                }
                
                ObjArray* arr = AS_ARRAY(arr_val);
                int idx = (int)((ObjInt*)idx_val.obj)->value;
                
                if (idx < 0 || idx >= arr->count) {
                    vm_error(vm, "Array index out of bounds: %d (size: %d)", idx, arr->count);
                    return NULL_VAL;
                }
                
                arr->items[idx] = REG(val_reg);
                break;
            }
            
            case OP_ITER_NEXT: {
                /* Get next item from array/string iterator */
                uint8_t val_reg = READ_BYTE();
                uint8_t idx_reg = READ_BYTE();
                uint8_t arr_reg = READ_BYTE();
                uint16_t jump_offset = READ_BYTE();
                jump_offset |= (READ_BYTE() << 8);
                
                Value arr_val = REG(arr_reg);
                Value idx_val = REG(idx_reg);
                int64_t idx = (IS_INT(idx_val)) ? AS_INT(idx_val) : 0;
                
                if (IS_ARRAY(arr_val)) {
                    ObjArray* arr = AS_ARRAY(arr_val);
                    
                    if (idx >= arr->count) {
                        /* Done iterating, jump to end */
                        frame->ip += jump_offset;
                    } else {
                        /* Get current item and increment index */
                        REG(val_reg) = arr->items[idx];
                        REG(idx_reg) = INT_VAL(idx + 1);
                    }
                } else if (IS_STRING(arr_val)) {
                    ObjString* str = AS_STRING(arr_val);
                    
                    if (idx >= str->char_count) {
                        /* Done iterating, jump to end */
                        frame->ip += jump_offset;
                    } else {
                        /* Get current character and increment index */
                        ObjString* ch = string_char_at(str, (int)idx);
                        REG(val_reg) = OBJ_VAL(ch);
                        REG(idx_reg) = INT_VAL(idx + 1);
                    }
                } else {
                    vm_error(vm, "Can only iterate over arrays and strings");
                    return NULL_VAL;
                }
                break;
            }
            
            case OP_TRY_BEGIN: {
                /* Push exception handler */
                uint16_t offset = READ_BYTE();
                offset |= (READ_BYTE() << 8);
                
                if (vm->exception_handler_count >= ZEX_MAX_EXCEPTION_HANDLERS) {
                    vm_error(vm, "Too many nested exception handlers");
                    return NULL_VAL;
                }
                
                ExceptionHandler* handler = &vm->exception_handlers[vm->exception_handler_count++];
                handler->handler_ip = frame->ip + offset;
                handler->frame_index = vm->frame_count - 1;
                handler->reg_top = vm->reg_top;
                break;
            }
            
            case OP_TRY_END: {
                /* Pop exception handler (successful try completion) */
                if (vm->exception_handler_count > 0) {
                    vm->exception_handler_count--;
                }
                break;
            }
            
            case OP_RAISE: {
                uint8_t reg = READ_BYTE();
                
                if (reg == 0xFF) {
                    /* Re-raise current exception */
                    if (!vm->has_exception) {
                        vm_error(vm, "No exception to re-raise");
                        return NULL_VAL;
                    }
                } else {
                    /* Raise exception from register */
                    Value exc_val = REG(reg);
                    if (!IS_EXCEPTION(exc_val)) {
                        /* Wrap non-exception value in RuntimeError */
                        ObjString* msg = value_to_string(exc_val);
                        ObjException* exc = new_exception(get_runtime_error_class(), msg, 
                            frame->function->chunk->lines[frame->ip - frame->function->chunk->code - 1],
                            0, 0);
                        vm->current_exception = OBJ_VAL(exc);
                    } else {
                        vm->current_exception = exc_val;
                    }
                    vm->has_exception = true;
                }
                
                /* Unwind to nearest handler */
                if (vm->exception_handler_count > 0) {
                    ExceptionHandler* handler = &vm->exception_handlers[--vm->exception_handler_count];
                    
                    /* Unwind call stack if necessary */
                    while (vm->frame_count > handler->frame_index + 1) {
                        vm->frame_count--;
                    }
                    vm->reg_top = handler->reg_top;
                    frame = &vm->frames[vm->frame_count - 1];
                    frame->ip = handler->handler_ip;
                } else {
                    /* No handler - print exception and return error */
                    ObjException* exc = AS_EXCEPTION(vm->current_exception);
                    if (exc->exception_class && exc->exception_class->name) {
                        zex_error(ERROR_RUNTIME, exc->line, exc->column, exc->span,
                                  "%s: %s", exc->exception_class->name->chars,
                                  exc->message ? exc->message->chars : "");
                    } else {
                        zex_error(ERROR_RUNTIME, exc->line, exc->column, exc->span,
                                  "Exception: %s", exc->message ? exc->message->chars : "");
                    }
                    return NULL_VAL;
                }
                break;
            }
            
            case OP_CHECK_EXC_TYPE: {
                /* Check if current exception matches type */
                uint8_t dst = READ_BYTE();
                uint16_t idx = READ_BYTE();
                idx |= (READ_BYTE() << 8);
                
                ObjString* type_name = AS_STRING(frame->function->chunk->constants[idx]);
                
                /* Look up exception class by name */
                Value class_val;
                bool found = table_get(&vm->globals, type_name, &class_val);
                
                bool matches = false;
                if (found && class_val.obj && class_val.obj->type == OBJ_CLASS) {
                    ObjClass* target_class = AS_CLASS(class_val);
                    matches = exception_is_type(vm->current_exception, target_class);
                }
                
                REG(dst) = BOOL_VAL(matches);
                break;
            }
            
            case OP_GET_EXCEPTION: {
                /* Get current exception into register */
                uint8_t reg = READ_BYTE();
                REG(reg) = vm->current_exception;
                break;
            }
            
            case OP_CLEAR_EXCEPTION: {
                /* Clear current exception (after handling) */
                vm->has_exception = false;
                vm->current_exception = NULL_VAL;
                break;
            }
            
            default:
                vm_error(vm, "Unknown opcode %d", instruction);
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
    
    return vm_run(vm, compiled.function);
}
