/*
 * Zex Programming Language
 * Compiler/compiler.c - AST to bytecode compiler
 */

#include "compiler.h"
#include "symtable.h"
#include "parser.h"
#include "stringobject.h"
#include "intobject.h"
#include "floatobject.h"
#include "classobject.h"
#include "error.h"
#include "memory.h"

/* Compiler state */
typedef struct CompilerState {
    ObjFunction* function;
    Scope scope;
    struct CompilerState* enclosing;
    
    int next_reg;           /* Next available register */
    bool had_error;
    
    /* For class compilation */
    bool in_class;
    const char* class_name;
    
    /* For loop compilation */
    int loop_start;             /* Offset of loop start */
    int continue_target;        /* Offset for continue (may differ from loop_start for for loops) */
    int* break_jumps;           /* Array of break jump locations to patch */
    int break_count;
    int break_capacity;
    int* continue_jumps;        /* Array of continue jump locations for for loops */
    int continue_count;
    int continue_capacity;
    int loop_depth;             /* Nesting depth of loops */
} CompilerState;

static CompilerState* current = NULL;

/* Forward declarations */
static void compile_node(ASTNode* node, int dest_reg);
static void compile_expression(ASTNode* node, int dest_reg);
static void compile_statement(ASTNode* node);
static void compile_closure(ASTNode* node, int dest_reg);

/*
 * Utility functions
 */

static Chunk* current_chunk(void) {
    return current->function->chunk;
}

static void emit_byte(uint8_t byte, int line) {
    chunk_write(current_chunk(), byte, line);
}

static void emit_bytes(uint8_t b1, uint8_t b2, int line) {
    emit_byte(b1, line);
    emit_byte(b2, line);
}

static void emit_byte16(uint16_t value, int line) {
    chunk_write16(current_chunk(), value, line);
}

static int emit_jump(OpCode op, uint8_t reg, int line) {
    emit_byte(op, line);
    emit_byte(reg, line);
    emit_byte(0xFF, line);
    emit_byte(0xFF, line);
    return current_chunk()->count - 2;  /* Position of jump offset */
}

static void patch_jump(int offset) {
    int jump = current_chunk()->count - offset - 2;
    
    if (jump > UINT16_MAX) {
        zex_error(ERROR_COMPILE, 0, 0, 0, "Jump too large");
        current->had_error = true;
        return;
    }
    
    current_chunk()->code[offset] = jump & 0xFF;
    current_chunk()->code[offset + 1] = (jump >> 8) & 0xFF;
}

static int make_constant(Value value) {
    int constant = chunk_add_constant(current_chunk(), value);
    if (constant > UINT16_MAX) {
        zex_error(ERROR_COMPILE, 0, 0, 0, "Too many constants in one chunk");
        current->had_error = true;
        return 0;
    }
    return constant;
}

static int identifier_constant(const char* name) {
    return make_constant(STRING_VAL(new_string_cstr(name)));
}

static int alloc_reg(void) {
    if (current->next_reg >= 256) {
        zex_error(ERROR_COMPILE, 0, 0, 0, "Too many registers in use");
        current->had_error = true;
        return 0;
    }
    return current->next_reg++;
}

static void free_reg(int count) {
    current->next_reg -= count;
    if (current->next_reg < 0) current->next_reg = 0;
}

/*
 * Compiler initialization
 */

static void init_compiler(CompilerState* state, ObjFunction* function) {
    state->function = function;
    state->enclosing = current;
    state->next_reg = 0;
    state->had_error = false;
    state->in_class = false;
    state->class_name = NULL;
    
    /* Initialize loop context */
    state->loop_start = -1;
    state->continue_target = -1;
    state->break_jumps = NULL;
    state->break_count = 0;
    state->break_capacity = 0;
    state->continue_jumps = NULL;
    state->continue_count = 0;
    state->continue_capacity = 0;
    state->loop_depth = 0;
    
    scope_init(&state->scope, current ? &current->scope : NULL);
    
    current = state;
}

static ObjFunction* end_compiler(void) {
    emit_byte(OP_LOAD_NULL, 0);
    emit_byte(0, 0);  /* R0 */
    emit_bytes(OP_RETURN, 0, 0);
    
    ObjFunction* function = current->function;
    
    current = current->enclosing;
    return function;
}

/*
 * Expression compilation
 */

static void compile_int_literal(ASTNode* node, int dest_reg) {
    Value value = INT_VAL(node->as.int_literal.value);
    int constant = make_constant(value);
    
    emit_byte(OP_LOAD_CONST, node->line);
    emit_byte(dest_reg, node->line);
    emit_byte16(constant, node->line);
}

static void compile_float_literal(ASTNode* node, int dest_reg) {
    Value value = FLOAT_VAL(node->as.float_literal.value);
    int constant = make_constant(value);
    
    emit_byte(OP_LOAD_CONST, node->line);
    emit_byte(dest_reg, node->line);
    emit_byte16(constant, node->line);
}

static void compile_string_literal(ASTNode* node, int dest_reg) {
    ObjString* str = new_string(node->as.string_literal.value, 
                                node->as.string_literal.length);
    Value value = STRING_VAL(str);
    int constant = make_constant(value);
    
    emit_byte(OP_LOAD_CONST, node->line);
    emit_byte(dest_reg, node->line);
    emit_byte16(constant, node->line);
}

static void compile_bool_literal(ASTNode* node, int dest_reg) {
    if (node->as.bool_literal.value) {
        emit_bytes(OP_LOAD_TRUE, dest_reg, node->line);
    } else {
        emit_bytes(OP_LOAD_FALSE, dest_reg, node->line);
    }
}

static void compile_null_literal(ASTNode* node, int dest_reg) {
    emit_bytes(OP_LOAD_NULL, dest_reg, node->line);
}

static void compile_identifier(ASTNode* node, int dest_reg) {
    const char* name = node->as.identifier.name;
    int len = strlen(name);
    
    /* Check local first */
    int slot = scope_resolve_local(&current->scope, name, len);
    if (slot != -1) {
        if (slot != dest_reg) {
            emit_byte(OP_MOVE, node->line);
            emit_byte(dest_reg, node->line);
            emit_byte(slot, node->line);
        }
        return;
    }
    
    /* Global */
    int idx = identifier_constant(name);
    emit_byte(OP_GET_GLOBAL, node->line);
    emit_byte(dest_reg, node->line);
    emit_byte16(idx, node->line);
}

static void compile_binary(ASTNode* node, int dest_reg) {
    int left_reg = alloc_reg();
    int right_reg = alloc_reg();
    
    /* Short-circuit for && and || */
    if (node->as.binary.op == BINOP_AND) {
        compile_expression(node->as.binary.left, dest_reg);
        int jump = emit_jump(OP_JUMP_IF_FALSE, dest_reg, node->line);
        compile_expression(node->as.binary.right, dest_reg);
        patch_jump(jump);
        free_reg(2);
        return;
    }
    
    if (node->as.binary.op == BINOP_OR) {
        compile_expression(node->as.binary.left, dest_reg);
        int jump = emit_jump(OP_JUMP_IF_TRUE, dest_reg, node->line);
        compile_expression(node->as.binary.right, dest_reg);
        patch_jump(jump);
        free_reg(2);
        return;
    }
    
    compile_expression(node->as.binary.left, left_reg);
    compile_expression(node->as.binary.right, right_reg);
    
    OpCode op;
    switch (node->as.binary.op) {
        case BINOP_ADD: op = OP_ADD; break;
        case BINOP_SUB: op = OP_SUB; break;
        case BINOP_MUL: op = OP_MUL; break;
        case BINOP_DIV: op = OP_DIV; break;
        case BINOP_MOD: op = OP_MOD; break;
        case BINOP_EQ:  op = OP_EQ; break;
        case BINOP_NE:  op = OP_NE; break;
        case BINOP_LT:  op = OP_LT; break;
        case BINOP_LE:  op = OP_LE; break;
        case BINOP_GT:  op = OP_GT; break;
        case BINOP_GE:  op = OP_GE; break;
        default:
            zex_error(ERROR_COMPILE, node->line, 0, 0, "Unknown binary operator");
            current->had_error = true;
            free_reg(2);
            return;
    }
    
    emit_byte(op, node->line);
    emit_byte(dest_reg, node->line);
    emit_byte(left_reg, node->line);
    emit_byte(right_reg, node->line);
    
    free_reg(2);
}

static void compile_unary(ASTNode* node, int dest_reg) {
    int operand_reg = alloc_reg();
    compile_expression(node->as.unary.operand, operand_reg);
    
    switch (node->as.unary.op) {
        case UNOP_NEG:
            emit_byte(OP_NEG, node->line);
            emit_byte(dest_reg, node->line);
            emit_byte(operand_reg, node->line);
            break;
        case UNOP_NOT:
            emit_byte(OP_NOT, node->line);
            emit_byte(dest_reg, node->line);
            emit_byte(operand_reg, node->line);
            break;
        case UNOP_POS:
            /* No-op, just move */
            if (dest_reg != operand_reg) {
                emit_byte(OP_MOVE, node->line);
                emit_byte(dest_reg, node->line);
                emit_byte(operand_reg, node->line);
            }
            break;
    }
    
    free_reg(1);
}

static void compile_call(ASTNode* node, int dest_reg) {
    /* Compile callee */
    int callee_reg = alloc_reg();
    compile_expression(node->as.call.callee, callee_reg);
    
    /* Compile arguments */
    int arg_base = current->next_reg;
    for (int i = 0; i < node->as.call.arg_count; i++) {
        int arg_reg = alloc_reg();
        compile_expression(node->as.call.arguments[i], arg_reg);
    }
    
    /* Emit call */
    emit_byte(OP_CALL, node->line);
    emit_byte(callee_reg, node->line);
    emit_byte(node->as.call.arg_count, node->line);
    emit_byte(arg_base, node->line);
    
    /* Move result to dest */
    if (dest_reg != callee_reg) {
        emit_byte(OP_MOVE, node->line);
        emit_byte(dest_reg, node->line);
        emit_byte(callee_reg, node->line);
    }
    
    /* Free argument registers */
    free_reg(node->as.call.arg_count + 1);
}

static void compile_get(ASTNode* node, int dest_reg) {
    int obj_reg = alloc_reg();
    compile_expression(node->as.get.object, obj_reg);
    
    int idx = identifier_constant(node->as.get.property);
    
    emit_byte(OP_GET_PROPERTY, node->line);
    emit_byte(dest_reg, node->line);
    emit_byte(obj_reg, node->line);
    emit_byte16(idx, node->line);
    
    free_reg(1);
}

static void compile_set(ASTNode* node, int dest_reg) {
    int obj_reg = alloc_reg();
    int val_reg = alloc_reg();
    
    compile_expression(node->as.set.object, obj_reg);
    compile_expression(node->as.set.value, val_reg);
    
    int idx = identifier_constant(node->as.set.property);
    
    emit_byte(OP_SET_PROPERTY, node->line);
    emit_byte(obj_reg, node->line);
    emit_byte16(idx, node->line);
    emit_byte(val_reg, node->line);
    
    /* Result is the assigned value */
    if (dest_reg != val_reg) {
        emit_byte(OP_MOVE, node->line);
        emit_byte(dest_reg, node->line);
        emit_byte(val_reg, node->line);
    }
    
    free_reg(2);
}

static void compile_set_compound(ASTNode* node, int dest_reg) {
    int obj_reg = alloc_reg();
    int old_val_reg = alloc_reg();
    int new_val_reg = alloc_reg();
    
    /* Get the object */
    compile_expression(node->as.set_compound.object, obj_reg);
    
    /* Get current property value */
    int idx = identifier_constant(node->as.set_compound.property);
    emit_byte(OP_GET_PROPERTY, node->line);
    emit_byte(old_val_reg, node->line);
    emit_byte(obj_reg, node->line);
    emit_byte16(idx, node->line);
    
    /* Compile the right-hand value */
    compile_expression(node->as.set_compound.value, new_val_reg);
    
    /* Perform the operation */
    OpCode op;
    switch (node->as.set_compound.op) {
        case BINOP_ADD: op = OP_ADD; break;
        case BINOP_SUB: op = OP_SUB; break;
        case BINOP_MUL: op = OP_MUL; break;
        case BINOP_DIV: op = OP_DIV; break;
        default: op = OP_ADD; break;
    }
    
    emit_byte(op, node->line);
    emit_byte(old_val_reg, node->line);  /* Result goes into old_val_reg */
    emit_byte(old_val_reg, node->line);
    emit_byte(new_val_reg, node->line);
    
    /* Set the property with the new value */
    emit_byte(OP_SET_PROPERTY, node->line);
    emit_byte(obj_reg, node->line);
    emit_byte16(idx, node->line);
    emit_byte(old_val_reg, node->line);
    
    /* Result is the new value */
    if (dest_reg != old_val_reg) {
        emit_byte(OP_MOVE, node->line);
        emit_byte(dest_reg, node->line);
        emit_byte(old_val_reg, node->line);
    }
    
    free_reg(3);
}

static void compile_self(ASTNode* node, int dest_reg) {
    /* 'self' is always in register 0 for methods */
    if (dest_reg != 0) {
        emit_byte(OP_MOVE, node->line);
        emit_byte(dest_reg, node->line);
        emit_byte(0, node->line);
    }
}

static void compile_grouping(ASTNode* node, int dest_reg) {
    compile_expression(node->as.grouping.expression, dest_reg);
}

static void compile_array(ASTNode* node, int dest_reg) {
    int count = node->as.array.count;
    int start_reg = current->next_reg;
    
    /* Compile each element into consecutive registers */
    for (int i = 0; i < count; i++) {
        int elem_reg = alloc_reg();
        compile_expression(node->as.array.elements[i], elem_reg);
    }
    
    /* Emit OP_ARRAY: dest_reg, start_reg, count */
    emit_byte(OP_ARRAY, node->line);
    emit_byte(dest_reg, node->line);
    emit_byte(count, node->line);
    emit_byte(start_reg, node->line);  /* First element register */
    
    /* Free the temporary registers */
    free_reg(count);
}

static void compile_index_get(ASTNode* node, int dest_reg) {
    int arr_reg = alloc_reg();
    int idx_reg = alloc_reg();
    
    compile_expression(node->as.index_get.object, arr_reg);
    compile_expression(node->as.index_get.index, idx_reg);
    
    emit_byte(OP_INDEX_GET, node->line);
    emit_byte(dest_reg, node->line);
    emit_byte(arr_reg, node->line);
    emit_byte(idx_reg, node->line);
    
    free_reg(2);
}

static void compile_index_set(ASTNode* node, int dest_reg) {
    int arr_reg = alloc_reg();
    int idx_reg = alloc_reg();
    int val_reg = alloc_reg();
    
    compile_expression(node->as.index_set.object, arr_reg);
    compile_expression(node->as.index_set.index, idx_reg);
    compile_expression(node->as.index_set.value, val_reg);
    
    emit_byte(OP_INDEX_SET, node->line);
    emit_byte(arr_reg, node->line);
    emit_byte(idx_reg, node->line);
    emit_byte(val_reg, node->line);
    
    /* Result is the assigned value */
    if (dest_reg != val_reg) {
        emit_byte(OP_MOVE, node->line);
        emit_byte(dest_reg, node->line);
        emit_byte(val_reg, node->line);
    }
    
    free_reg(3);
}

static void compile_expression(ASTNode* node, int dest_reg) {
    switch (node->type) {
        case AST_INT_LITERAL:
            compile_int_literal(node, dest_reg);
            break;
        case AST_FLOAT_LITERAL:
            compile_float_literal(node, dest_reg);
            break;
        case AST_STRING_LITERAL:
            compile_string_literal(node, dest_reg);
            break;
        case AST_BOOL_LITERAL:
            compile_bool_literal(node, dest_reg);
            break;
        case AST_NULL_LITERAL:
            compile_null_literal(node, dest_reg);
            break;
        case AST_IDENTIFIER:
            compile_identifier(node, dest_reg);
            break;
        case AST_BINARY:
            compile_binary(node, dest_reg);
            break;
        case AST_UNARY:
            compile_unary(node, dest_reg);
            break;
        case AST_CALL:
            compile_call(node, dest_reg);
            break;
        case AST_GET:
            compile_get(node, dest_reg);
            break;
        case AST_SET:
            compile_set(node, dest_reg);
            break;
        case AST_SET_COMPOUND:
            compile_set_compound(node, dest_reg);
            break;
        case AST_SELF:
            compile_self(node, dest_reg);
            break;
        case AST_GROUPING:
            compile_grouping(node, dest_reg);
            break;
        case AST_ARRAY:
            compile_array(node, dest_reg);
            break;
        case AST_INDEX_GET:
            compile_index_get(node, dest_reg);
            break;
        case AST_INDEX_SET:
            compile_index_set(node, dest_reg);
            break;
        case AST_CLOSURE:
            compile_closure(node, dest_reg);
            break;
        default:
            zex_error(ERROR_COMPILE, node->line, 0, 0, "Unknown expression type: %d", node->type);
            current->had_error = true;
            break;
    }
}

/*
 * Statement compilation
 */

static void compile_var_decl(ASTNode* node) {
    const char* name = node->as.var_decl.name;
    int len = strlen(name);
    
    if (current->scope.scope_depth > 0) {
        /* Local variable */
        int slot = scope_add_local(&current->scope, name, len);
        if (slot == -1) {
            zex_error(ERROR_COMPILE, node->line, 0, 0, "Too many local variables");
            current->had_error = true;
            return;
        }
        if (slot == -2) {
            zex_error(ERROR_COMPILE, node->line, 0, 0, "Variable '%s' already declared in this scope", name);
            current->had_error = true;
            return;
        }
        
        if (node->as.var_decl.initializer) {
            compile_expression(node->as.var_decl.initializer, slot);
        } else {
            emit_bytes(OP_LOAD_NULL, slot, node->line);
        }
        
        /* Ensure next_reg is past this local's register */
        if (current->next_reg <= slot) {
            current->next_reg = slot + 1;
        }
    } else {
        /* Global variable */
        int idx = identifier_constant(name);
        int val_reg = alloc_reg();
        
        if (node->as.var_decl.initializer) {
            compile_expression(node->as.var_decl.initializer, val_reg);
        } else {
            emit_bytes(OP_LOAD_NULL, val_reg, node->line);
        }
        
        emit_byte(OP_DEF_GLOBAL, node->line);
        emit_byte16(idx, node->line);
        emit_byte(val_reg, node->line);
        
        free_reg(1);
    }
}

static void compile_assign(ASTNode* node) {
    const char* name = node->as.assign.name;
    int len = strlen(name);
    
    int val_reg = alloc_reg();
    compile_expression(node->as.assign.value, val_reg);
    
    /* Check local first */
    int slot = scope_resolve_local(&current->scope, name, len);
    if (slot != -1) {
        emit_byte(OP_MOVE, node->line);
        emit_byte(slot, node->line);
        emit_byte(val_reg, node->line);
    } else {
        /* Global */
        int idx = identifier_constant(name);
        emit_byte(OP_SET_GLOBAL, node->line);
        emit_byte16(idx, node->line);
        emit_byte(val_reg, node->line);
    }
    
    free_reg(1);
}

static void compile_compound_assign(ASTNode* node) {
    const char* name = node->as.compound_assign.name;
    int len = strlen(name);
    
    int left_reg = alloc_reg();
    int right_reg = alloc_reg();
    
    /* Load current value */
    int slot = scope_resolve_local(&current->scope, name, len);
    if (slot != -1) {
        emit_byte(OP_MOVE, node->line);
        emit_byte(left_reg, node->line);
        emit_byte(slot, node->line);
    } else {
        int idx = identifier_constant(name);
        emit_byte(OP_GET_GLOBAL, node->line);
        emit_byte(left_reg, node->line);
        emit_byte16(idx, node->line);
    }
    
    /* Compile RHS */
    compile_expression(node->as.compound_assign.value, right_reg);
    
    /* Perform operation */
    OpCode op;
    switch (node->as.compound_assign.op) {
        case COMPOUND_ADD: op = OP_ADD; break;
        case COMPOUND_SUB: op = OP_SUB; break;
        case COMPOUND_MUL: op = OP_MUL; break;
        case COMPOUND_DIV: op = OP_DIV; break;
        case COMPOUND_MOD: op = OP_MOD; break;
    }
    
    emit_byte(op, node->line);
    emit_byte(left_reg, node->line);
    emit_byte(left_reg, node->line);
    emit_byte(right_reg, node->line);
    
    /* Store result */
    if (slot != -1) {
        emit_byte(OP_MOVE, node->line);
        emit_byte(slot, node->line);
        emit_byte(left_reg, node->line);
    } else {
        int idx = identifier_constant(name);
        emit_byte(OP_SET_GLOBAL, node->line);
        emit_byte16(idx, node->line);
        emit_byte(left_reg, node->line);
    }
    
    free_reg(2);
}

static void compile_expr_stmt(ASTNode* node) {
    ASTNode* expr = node->as.expr_stmt.expression;
    
    /* Handle assignment statements that are wrapped as expressions */
    if (expr->type == AST_ASSIGN) {
        compile_assign(expr);
        return;
    }
    if (expr->type == AST_COMPOUND_ASSIGN) {
        compile_compound_assign(expr);
        return;
    }
    if (expr->type == AST_SET_COMPOUND) {
        int reg = alloc_reg();
        compile_set_compound(expr, reg);
        free_reg(1);
        return;
    }
    
    int reg = alloc_reg();
    compile_expression(expr, reg);
    free_reg(1);
}

static void compile_block(ASTNode* node) {
    scope_begin(&current->scope);
    
    for (int i = 0; i < node->as.block.count; i++) {
        compile_node(node->as.block.statements[i], 0);
    }
    
    int popped = scope_end(&current->scope);
    current->next_reg -= popped;
    if (current->next_reg < 0) current->next_reg = 0;
}

static void compile_if(ASTNode* node) {
    /* Condition */
    int cond_reg = alloc_reg();
    compile_expression(node->as.if_stmt.condition, cond_reg);
    
    int then_jump = emit_jump(OP_JUMP_IF_FALSE, cond_reg, node->line);
    free_reg(1);
    
    /* Then branch */
    compile_node(node->as.if_stmt.then_branch, 0);
    
    if (node->as.if_stmt.else_branch) {
        int else_jump = current_chunk()->count;
        emit_byte(OP_JUMP, node->line);
        emit_byte16(0xFFFF, node->line);
        
        patch_jump(then_jump);
        
        /* Else branch */
        compile_node(node->as.if_stmt.else_branch, 0);
        
        /* Patch else jump */
        int jump = current_chunk()->count - else_jump - 3;
        current_chunk()->code[else_jump + 1] = jump & 0xFF;
        current_chunk()->code[else_jump + 2] = (jump >> 8) & 0xFF;
    } else {
        patch_jump(then_jump);
    }
}

static void compile_while(ASTNode* node) {
    /* Save previous loop context */
    int prev_loop_start = current->loop_start;
    int prev_continue_target = current->continue_target;
    int* prev_break_jumps = current->break_jumps;
    int prev_break_count = current->break_count;
    int prev_break_capacity = current->break_capacity;
    int* prev_continue_jumps = current->continue_jumps;
    int prev_continue_count = current->continue_count;
    int prev_continue_capacity = current->continue_capacity;
    
    /* Initialize new loop context */
    current->loop_start = current_chunk()->count;
    current->continue_target = current->loop_start;  /* For while, continue goes to start */
    current->break_jumps = NULL;
    current->break_count = 0;
    current->break_capacity = 0;
    current->continue_jumps = NULL;
    current->continue_count = 0;
    current->continue_capacity = 0;
    current->loop_depth++;
    
    /* Condition */
    int cond_reg = alloc_reg();
    compile_expression(node->as.while_stmt.condition, cond_reg);
    
    /* Jump to end if false */
    int exit_jump = emit_jump(OP_JUMP_IF_FALSE, cond_reg, node->line);
    free_reg(1);
    
    /* Body */
    compile_node(node->as.while_stmt.body, 0);
    
    /* Jump back to start */
    emit_byte(OP_JUMP_BACK, node->line);
    int loop_offset = current_chunk()->count - current->loop_start + 2;
    emit_byte16(loop_offset, node->line);
    
    /* Patch exit jump */
    patch_jump(exit_jump);
    
    /* Patch all break jumps */
    for (int i = 0; i < current->break_count; i++) {
        int jump = current_chunk()->count - current->break_jumps[i] - 3;
        current_chunk()->code[current->break_jumps[i] + 1] = jump & 0xFF;
        current_chunk()->code[current->break_jumps[i] + 2] = (jump >> 8) & 0xFF;
    }
    if (current->break_jumps) {
        zex_free(current->break_jumps, current->break_capacity * sizeof(int));
    }
    
    /* Restore previous loop context */
    current->loop_start = prev_loop_start;
    current->continue_target = prev_continue_target;
    current->break_jumps = prev_break_jumps;
    current->break_count = prev_break_count;
    current->break_capacity = prev_break_capacity;
    current->continue_jumps = prev_continue_jumps;
    current->continue_count = prev_continue_count;
    current->continue_capacity = prev_continue_capacity;
    current->loop_depth--;
}

static void compile_do_while(ASTNode* node) {
    /* Save previous loop context */
    int prev_loop_start = current->loop_start;
    int prev_continue_target = current->continue_target;
    int* prev_break_jumps = current->break_jumps;
    int prev_break_count = current->break_count;
    int prev_break_capacity = current->break_capacity;
    int* prev_continue_jumps = current->continue_jumps;
    int prev_continue_count = current->continue_count;
    int prev_continue_capacity = current->continue_capacity;
    
    /* Initialize new loop context */
    current->loop_start = current_chunk()->count;
    current->continue_target = current->loop_start;  /* For do-while, continue goes to start */
    current->break_jumps = NULL;
    current->break_count = 0;
    current->break_capacity = 0;
    current->continue_jumps = NULL;
    current->continue_count = 0;
    current->continue_capacity = 0;
    current->loop_depth++;
    
    /* Body first */
    compile_node(node->as.do_while_stmt.body, 0);
    
    /* Condition */
    int cond_reg = alloc_reg();
    compile_expression(node->as.do_while_stmt.condition, cond_reg);
    
    /* Skip past jump back if false */
    int skip_jump = emit_jump(OP_JUMP_IF_FALSE, cond_reg, node->line);
    free_reg(1);
    
    /* Jump back to loop start */
    emit_byte(OP_JUMP_BACK, node->line);
    int loop_offset = current_chunk()->count - current->loop_start + 2;
    emit_byte16(loop_offset, node->line);
    
    /* Patch skip jump */
    patch_jump(skip_jump);
    
    /* Patch all break jumps */
    for (int i = 0; i < current->break_count; i++) {
        int jump = current_chunk()->count - current->break_jumps[i] - 3;
        current_chunk()->code[current->break_jumps[i] + 1] = jump & 0xFF;
        current_chunk()->code[current->break_jumps[i] + 2] = (jump >> 8) & 0xFF;
    }
    if (current->break_jumps) {
        zex_free(current->break_jumps, current->break_capacity * sizeof(int));
    }
    
    /* Restore previous loop context */
    current->loop_start = prev_loop_start;
    current->continue_target = prev_continue_target;
    current->break_jumps = prev_break_jumps;
    current->break_count = prev_break_count;
    current->break_capacity = prev_break_capacity;
    current->continue_jumps = prev_continue_jumps;
    current->continue_count = prev_continue_count;
    current->continue_capacity = prev_continue_capacity;
    current->loop_depth--;
}

static void compile_break(ASTNode* node) {
    if (current->loop_depth == 0) {
        zex_error(ERROR_COMPILE, node->line, 0, 0, "'break' outside of loop");
        current->had_error = true;
        return;
    }
    
    /* Emit a forward jump, save location to patch later */
    int jump_loc = current_chunk()->count;
    emit_byte(OP_JUMP, node->line);
    emit_byte16(0xFFFF, node->line);  /* Placeholder */
    
    /* Add to break jump list */
    if (current->break_count >= current->break_capacity) {
        int old_cap = current->break_capacity;
        current->break_capacity = old_cap < 8 ? 8 : old_cap * 2;
        current->break_jumps = GROW_ARRAY(int, current->break_jumps, old_cap, current->break_capacity);
    }
    current->break_jumps[current->break_count++] = jump_loc;
}

static void compile_continue(ASTNode* node) {
    if (current->loop_depth == 0) {
        zex_error(ERROR_COMPILE, node->line, 0, 0, "'continue' outside of loop");
        current->had_error = true;
        return;
    }
    
    if (current->continue_target == -1) {
        /* Deferred continue (for C-style for loops) - emit forward jump to patch later */
        int jump_loc = current_chunk()->count;
        emit_byte(OP_JUMP, node->line);
        emit_byte16(0xFFFF, node->line);  /* Placeholder */
        
        /* Add to continue jump list */
        if (current->continue_count >= current->continue_capacity) {
            int old_cap = current->continue_capacity;
            current->continue_capacity = old_cap < 8 ? 8 : old_cap * 2;
            current->continue_jumps = GROW_ARRAY(int, current->continue_jumps, old_cap, current->continue_capacity);
        }
        current->continue_jumps[current->continue_count++] = jump_loc;
    } else {
        /* Immediate continue (for while/do-while loops) */
        emit_byte(OP_JUMP_BACK, node->line);
        int loop_offset = current_chunk()->count - current->continue_target + 2;
        emit_byte16(loop_offset, node->line);
    }
}

static void compile_for(ASTNode* node) {
    /* C-style for loop: for var i = 0; i < 10; i += 1 { } */
    
    scope_begin(&current->scope);
    
    /* Initializer */
    if (node->as.for_stmt.initializer) {
        compile_node(node->as.for_stmt.initializer, 0);
    }
    
    /* Save previous loop context */
    int prev_loop_start = current->loop_start;
    int prev_continue_target = current->continue_target;
    int* prev_break_jumps = current->break_jumps;
    int prev_break_count = current->break_count;
    int prev_break_capacity = current->break_capacity;
    int* prev_continue_jumps = current->continue_jumps;
    int prev_continue_count = current->continue_count;
    int prev_continue_capacity = current->continue_capacity;
    
    /* Initialize new loop context */
    current->loop_start = current_chunk()->count;
    current->continue_target = -1;  /* Use deferred patching for continues */
    current->break_jumps = NULL;
    current->break_count = 0;
    current->break_capacity = 0;
    current->continue_jumps = NULL;
    current->continue_count = 0;
    current->continue_capacity = 0;
    current->loop_depth++;
    
    /* Condition */
    int exit_jump = -1;
    if (node->as.for_stmt.condition) {
        int cond_reg = alloc_reg();
        compile_expression(node->as.for_stmt.condition, cond_reg);
        exit_jump = emit_jump(OP_JUMP_IF_FALSE, cond_reg, node->line);
        free_reg(1);
    }
    
    /* Body */
    compile_node(node->as.for_stmt.body, 0);
    
    /* Patch all continue jumps to point to current position (start of update) */
    int update_start = current_chunk()->count;
    for (int i = 0; i < current->continue_count; i++) {
        int jump = update_start - current->continue_jumps[i] - 3;
        current_chunk()->code[current->continue_jumps[i] + 1] = jump & 0xFF;
        current_chunk()->code[current->continue_jumps[i] + 2] = (jump >> 8) & 0xFF;
    }
    if (current->continue_jumps) {
        zex_free(current->continue_jumps, current->continue_capacity * sizeof(int));
    }
    
    /* Update (can be assignment or compound assignment, so use compile_node) */
    if (node->as.for_stmt.update) {
        compile_node(node->as.for_stmt.update, 0);
    }
    
    /* Jump back to condition */
    emit_byte(OP_JUMP_BACK, node->line);
    int loop_offset = current_chunk()->count - current->loop_start + 2;
    emit_byte16(loop_offset, node->line);
    
    /* Patch exit jump */
    if (exit_jump >= 0) {
        patch_jump(exit_jump);
    }
    
    /* Patch all break jumps */
    for (int i = 0; i < current->break_count; i++) {
        int jump = current_chunk()->count - current->break_jumps[i] - 3;
        current_chunk()->code[current->break_jumps[i] + 1] = jump & 0xFF;
        current_chunk()->code[current->break_jumps[i] + 2] = (jump >> 8) & 0xFF;
    }
    if (current->break_jumps) {
        zex_free(current->break_jumps, current->break_capacity * sizeof(int));
    }
    
    /* Restore previous loop context */
    current->loop_start = prev_loop_start;
    current->continue_target = prev_continue_target;
    current->break_jumps = prev_break_jumps;
    current->break_count = prev_break_count;
    current->break_capacity = prev_break_capacity;
    current->continue_jumps = prev_continue_jumps;
    current->continue_count = prev_continue_count;
    current->continue_capacity = prev_continue_capacity;
    current->loop_depth--;
    
    scope_end(&current->scope);
}

static void compile_for_in(ASTNode* node) {
    /* For-in loop with iterator: for var x in arr { } */
    
    scope_begin(&current->scope);
    
    /* Evaluate iterable */
    int arr_reg = alloc_reg();
    compile_expression(node->as.for_in_stmt.iterable, arr_reg);
    
    /* Allocate register for index counter (starts at 0) */
    int idx_reg = alloc_reg();
    emit_byte(OP_LOAD_CONST, node->line);
    emit_byte(idx_reg, node->line);
    int zero_const = make_constant(INT_VAL(0));
    emit_byte16(zero_const, node->line);
    
    /* Create loop variable as local */
    int var_slot = scope_add_local(&current->scope, node->as.for_in_stmt.var_name,
                                   strlen(node->as.for_in_stmt.var_name));
    if (var_slot == -1) {
        zex_error(ERROR_COMPILE, node->line, 0, 0, "Too many local variables");
        current->had_error = true;
        return;
    }
    int var_reg = alloc_reg();
    current->scope.locals[var_slot].reg = var_reg;
    
    /* Save previous loop context */
    int prev_loop_start = current->loop_start;
    int prev_continue_target = current->continue_target;
    int* prev_break_jumps = current->break_jumps;
    int prev_break_count = current->break_count;
    int prev_break_capacity = current->break_capacity;
    int* prev_continue_jumps = current->continue_jumps;
    int prev_continue_count = current->continue_count;
    int prev_continue_capacity = current->continue_capacity;
    
    /* Initialize new loop context */
    current->loop_start = current_chunk()->count;
    current->continue_target = current->loop_start;  /* For for-in, continue goes to start */
    current->break_jumps = NULL;
    current->break_count = 0;
    current->break_capacity = 0;
    current->continue_jumps = NULL;
    current->continue_count = 0;
    current->continue_capacity = 0;
    current->loop_depth++;
    
    /* OP_ITER_NEXT: get next value, increment index, jump if done */
    emit_byte(OP_ITER_NEXT, node->line);
    emit_byte(var_reg, node->line);
    emit_byte(idx_reg, node->line);
    emit_byte(arr_reg, node->line);
    int exit_jump = current_chunk()->count;
    emit_byte16(0xFFFF, node->line);  /* Placeholder for jump offset */
    
    /* Body */
    compile_node(node->as.for_in_stmt.body, 0);
    
    /* Jump back to loop start */
    emit_byte(OP_JUMP_BACK, node->line);
    int loop_offset = current_chunk()->count - current->loop_start + 2;
    emit_byte16(loop_offset, node->line);
    
    /* Patch exit jump */
    int jump_dist = current_chunk()->count - exit_jump - 2;
    current_chunk()->code[exit_jump] = jump_dist & 0xFF;
    current_chunk()->code[exit_jump + 1] = (jump_dist >> 8) & 0xFF;
    
    /* Patch all break jumps */
    for (int i = 0; i < current->break_count; i++) {
        int jump = current_chunk()->count - current->break_jumps[i] - 3;
        current_chunk()->code[current->break_jumps[i] + 1] = jump & 0xFF;
        current_chunk()->code[current->break_jumps[i] + 2] = (jump >> 8) & 0xFF;
    }
    if (current->break_jumps) {
        zex_free(current->break_jumps, current->break_capacity * sizeof(int));
    }
    
    /* Restore previous loop context */
    current->loop_start = prev_loop_start;
    current->continue_target = prev_continue_target;
    current->break_jumps = prev_break_jumps;
    current->break_count = prev_break_count;
    current->break_capacity = prev_break_capacity;
    current->continue_jumps = prev_continue_jumps;
    current->continue_count = prev_continue_count;
    current->continue_capacity = prev_continue_capacity;
    current->loop_depth--;
    
    scope_end(&current->scope);
    free_reg(3);  /* var_reg, idx_reg, arr_reg */
}

static void compile_return(ASTNode* node) {
    if (node->as.return_stmt.value) {
        int reg = alloc_reg();
        compile_expression(node->as.return_stmt.value, reg);
        emit_bytes(OP_RETURN, reg, node->line);
        free_reg(1);
    } else {
        emit_bytes(OP_LOAD_NULL, 0, node->line);
        emit_bytes(OP_RETURN, 0, node->line);
    }
}

static void compile_closure(ASTNode* node, int dest_reg) {
    ObjFunction* function = new_function();
    function->name = NULL;  /* Anonymous function */
    function->arity = node->as.closure.params.count;
    function->chunk = ALLOCATE(Chunk, 1);
    chunk_init(function->chunk);
    
    CompilerState compiler;
    init_compiler(&compiler, function);
    scope_begin(&current->scope);
    
    /* Parameters are local variables starting at reg 0 */
    for (int i = 0; i < node->as.closure.params.count; i++) {
        const char* param = node->as.closure.params.names[i];
        scope_add_local(&current->scope, param, strlen(param));
        current->next_reg++;
    }
    
    /* Compile body */
    if (node->as.closure.is_expression) {
        /* Expression body - compile as implicit return */
        int reg = alloc_reg();
        compile_expression(node->as.closure.body, reg);
        emit_bytes(OP_RETURN, reg, node->line);
        free_reg(1);
    } else {
        /* Block body - compile statements with implicit return for last expr */
        ASTNode* body = node->as.closure.body;
        int count = body->as.block.count;
        
        for (int i = 0; i < count; i++) {
            ASTNode* stmt = body->as.block.statements[i];
            
            /* Check if this is the last statement and it's an expression statement */
            if (i == count - 1 && stmt->type == AST_EXPR_STMT) {
                /* Implicit return: compile expression and return it */
                int reg = alloc_reg();
                compile_expression(stmt->as.expr_stmt.expression, reg);
                emit_bytes(OP_RETURN, reg, stmt->line);
                free_reg(1);
            } else {
                compile_node(stmt, 0);
            }
        }
    }
    
    end_compiler();
    
    /* Emit closure instruction in enclosing function */
    int idx = make_constant(OBJ_VAL(function));
    emit_byte(OP_CLOSURE, node->line);
    emit_byte(dest_reg, node->line);
    emit_byte16(idx, node->line);
}

static void compile_function(ASTNode* node, bool is_method) {
    ObjFunction* function = new_function();
    function->name = new_string_cstr(node->as.fun_decl.name);
    function->arity = node->as.fun_decl.params.count;
    function->chunk = ALLOCATE(Chunk, 1);
    chunk_init(function->chunk);
    
    CompilerState compiler;
    init_compiler(&compiler, function);
    scope_begin(&current->scope);
    
    /* Parameters are local variables starting at reg 0 (or 1 if method with self) */
    int start_reg = 0;
    if (is_method && node->as.fun_decl.params.count > 0 &&
        strcmp(node->as.fun_decl.params.names[0], "self") == 0) {
        /* 'self' is implicitly in R0 */
        scope_add_local(&current->scope, "self", 4);
        start_reg = 1;
        current->next_reg = 1;
    }
    
    for (int i = (start_reg > 0 ? 1 : 0); i < node->as.fun_decl.params.count; i++) {
        const char* param = node->as.fun_decl.params.names[i];
        scope_add_local(&current->scope, param, strlen(param));
        current->next_reg++;
    }
    
    /* Compile body with implicit return for last expression statement */
    ASTNode* body = node->as.fun_decl.body;
    int count = body->as.block.count;
    
    for (int i = 0; i < count; i++) {
        ASTNode* stmt = body->as.block.statements[i];
        
        /* Check if this is the last statement and it's an expression statement */
        if (i == count - 1 && stmt->type == AST_EXPR_STMT) {
            /* Implicit return: compile expression and return it */
            int reg = alloc_reg();
            compile_expression(stmt->as.expr_stmt.expression, reg);
            emit_bytes(OP_RETURN, reg, stmt->line);
            free_reg(1);
        } else {
            compile_node(stmt, 0);
        }
    }
    
    end_compiler();
    
    /* Emit closure instruction in enclosing function */
    int idx = make_constant(OBJ_VAL(function));
    int dest = alloc_reg();
    emit_byte(OP_CLOSURE, node->line);
    emit_byte(dest, node->line);
    emit_byte16(idx, node->line);
}

static void compile_fun_decl(ASTNode* node) {
    const char* name = node->as.fun_decl.name;
    int name_idx = identifier_constant(name);
    
    compile_function(node, false);
    
    /* Store function in global */
    int func_reg = current->next_reg - 1;
    emit_byte(OP_DEF_GLOBAL, node->line);
    emit_byte16(name_idx, node->line);
    emit_byte(func_reg, node->line);
    
    free_reg(1);
}

static void compile_class_decl(ASTNode* node) {
    const char* name = node->as.class_decl.name;
    const char* superclass = node->as.class_decl.superclass;
    int name_idx = identifier_constant(name);
    
    /* Create class object */
    int class_reg = alloc_reg();
    emit_byte(OP_CLASS, node->line);
    emit_byte(class_reg, node->line);
    emit_byte16(name_idx, node->line);
    
    /* Handle inheritance */
    if (superclass != NULL) {
        int super_idx = identifier_constant(superclass);
        int super_reg = alloc_reg();
        
        /* Get superclass */
        emit_byte(OP_GET_GLOBAL, node->line);
        emit_byte(super_reg, node->line);
        emit_byte16(super_idx, node->line);
        
        /* Inherit methods from superclass */
        emit_byte(OP_INHERIT, node->line);
        emit_byte(class_reg, node->line);
        emit_byte(super_reg, node->line);
        
        free_reg(1);
    }
    
    /* Define global first so methods can reference the class */
    emit_byte(OP_DEF_GLOBAL, node->line);
    emit_byte16(name_idx, node->line);
    emit_byte(class_reg, node->line);
    
    /* Compile methods */
    current->in_class = true;
    current->class_name = name;
    
    for (int i = 0; i < node->as.class_decl.method_count; i++) {
        ASTNode* method = node->as.class_decl.methods[i];
        
        compile_function(method, true);
        
        int method_idx = identifier_constant(method->as.fun_decl.name);
        int method_reg = current->next_reg - 1;
        
        /* Add method to class */
        emit_byte(OP_GET_GLOBAL, node->line);
        emit_byte(class_reg, node->line);
        emit_byte16(name_idx, node->line);
        
        emit_byte(OP_METHOD, node->line);
        emit_byte(class_reg, node->line);
        emit_byte16(method_idx, node->line);
        emit_byte(method_reg, node->line);
        
        free_reg(1);
    }
    
    current->in_class = false;
    current->class_name = NULL;
    
    free_reg(1);
}

static void compile_node(ASTNode* node, int dest_reg) {
    UNUSED(dest_reg);
    
    switch (node->type) {
        case AST_VAR_DECL:
            compile_var_decl(node);
            break;
        case AST_ASSIGN:
            compile_assign(node);
            break;
        case AST_COMPOUND_ASSIGN:
            compile_compound_assign(node);
            break;
        case AST_EXPR_STMT:
            compile_expr_stmt(node);
            break;
        case AST_BLOCK:
            compile_block(node);
            break;
        case AST_IF:
            compile_if(node);
            break;
        case AST_WHILE:
            compile_while(node);
            break;
        case AST_DO_WHILE:
            compile_do_while(node);
            break;
        case AST_FOR:
            compile_for(node);
            break;
        case AST_FOR_IN:
            compile_for_in(node);
            break;
        case AST_BREAK:
            compile_break(node);
            break;
        case AST_CONTINUE:
            compile_continue(node);
            break;
        case AST_RETURN:
            compile_return(node);
            break;
        case AST_FUN_DECL:
            compile_fun_decl(node);
            break;
        case AST_CLASS_DECL:
            compile_class_decl(node);
            break;
        default:
            /* Expression statement */
            if (node->type >= AST_INT_LITERAL && node->type <= AST_GROUPING) {
                int reg = alloc_reg();
                compile_expression(node, reg);
                free_reg(1);
            } else {
                zex_error(ERROR_COMPILE, node->line, 0, 0, "Unknown node type: %d", node->type);
                current->had_error = true;
            }
            break;
    }
}

static void compile_statement(ASTNode* node) {
    compile_node(node, 0);
}

/*
 * Public API
 */

CompileResult compile(ASTNode* ast) {
    CompileResult result = {NULL, false};
    
    if (ast == NULL) {
        result.had_error = true;
        return result;
    }
    
    ObjFunction* function = new_function();
    function->chunk = ALLOCATE(Chunk, 1);
    chunk_init(function->chunk);
    
    CompilerState compiler;
    init_compiler(&compiler, function);
    
    /* Compile program statements */
    if (ast->type == AST_PROGRAM) {
        for (int i = 0; i < ast->as.program.count; i++) {
            compile_statement(ast->as.program.statements[i]);
        }
    } else {
        compile_statement(ast);
    }
    
    result.function = end_compiler();
    result.had_error = current != NULL ? current->had_error : compiler.had_error;
    
    return result;
}

CompileResult compile_source(const char* source) {
    ASTNode* ast = parse(source);
    if (ast == NULL) {
        CompileResult result = {NULL, true};
        return result;
    }
    
    CompileResult result = compile(ast);
    ast_free(ast);
    
    return result;
}
