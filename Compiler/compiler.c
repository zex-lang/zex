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
} CompilerState;

static CompilerState* current = NULL;

/* Forward declarations */
static void compile_node(ASTNode* node, int dest_reg);
static void compile_expression(ASTNode* node, int dest_reg);
static void compile_statement(ASTNode* node);

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
        fprintf(stderr, "Jump too large\n");
        current->had_error = true;
        return;
    }
    
    current_chunk()->code[offset] = jump & 0xFF;
    current_chunk()->code[offset + 1] = (jump >> 8) & 0xFF;
}

static int make_constant(Value value) {
    int constant = chunk_add_constant(current_chunk(), value);
    if (constant > UINT16_MAX) {
        fprintf(stderr, "Too many constants in one chunk\n");
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
        fprintf(stderr, "Too many registers in use\n");
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
    
    scope_init(&state->scope, current ? &current->scope : NULL);
    
    current = state;
}

static ObjFunction* end_compiler(void) {
    emit_byte(OP_LOAD_NULL, 0);
    emit_byte(0, 0);  /* R0 */
    emit_bytes(OP_RETURN, 0, 0);
    
    ObjFunction* function = current->function;
    
#ifdef DEBUG_PRINT_CODE
    if (!current->had_error) {
        chunk_disassemble(current_chunk(), 
                         function->name ? function->name->chars : "<script>");
    }
#endif
    
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
        case BINOP_EQ:  op = OP_EQ; break;
        case BINOP_NE:  op = OP_NE; break;
        case BINOP_LT:  op = OP_LT; break;
        case BINOP_LE:  op = OP_LE; break;
        case BINOP_GT:  op = OP_GT; break;
        case BINOP_GE:  op = OP_GE; break;
        default:
            fprintf(stderr, "Unknown binary op\n");
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
        case AST_SELF:
            compile_self(node, dest_reg);
            break;
        case AST_GROUPING:
            compile_grouping(node, dest_reg);
            break;
        default:
            fprintf(stderr, "Unknown expression type: %d\n", node->type);
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
            fprintf(stderr, "Too many local variables\n");
            current->had_error = true;
            return;
        }
        if (slot == -2) {
            fprintf(stderr, "Variable '%s' already declared in this scope\n", name);
            current->had_error = true;
            return;
        }
        
        if (node->as.var_decl.initializer) {
            compile_expression(node->as.var_decl.initializer, slot);
        } else {
            emit_bytes(OP_LOAD_NULL, slot, node->line);
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
    /* Save loop start position */
    int loop_start = current_chunk()->count;
    
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
    int loop_offset = current_chunk()->count - loop_start + 2;
    emit_byte16(loop_offset, node->line);
    
    /* Patch exit jump */
    patch_jump(exit_jump);
}

static void compile_do_while(ASTNode* node) {
    /* Save loop start position */
    int loop_start = current_chunk()->count;
    
    /* Body first */
    compile_node(node->as.do_while_stmt.body, 0);
    
    /* Condition */
    int cond_reg = alloc_reg();
    compile_expression(node->as.do_while_stmt.condition, cond_reg);
    
    /* Jump back to start if true */
    emit_byte(OP_JUMP_IF_TRUE, node->line);
    emit_byte(cond_reg, node->line);
    int loop_offset = current_chunk()->count - loop_start + 2;
    /* For jump_if_true going backwards, we use a forward offset and negate it */
    emit_byte(0, node->line);  /* Placeholder */
    emit_byte(0, node->line);  /* Placeholder */
    
    /* Actually use jump_back after condition if true */
    /* Simpler approach: use OP_JUMP_IF_FALSE to skip past OP_JUMP_BACK */
    current_chunk()->count -= 4;  /* Remove the OP_JUMP_IF_TRUE and placeholders */
    
    int skip_jump = emit_jump(OP_JUMP_IF_FALSE, cond_reg, node->line);
    free_reg(1);
    
    /* Jump back to loop start */
    emit_byte(OP_JUMP_BACK, node->line);
    loop_offset = current_chunk()->count - loop_start + 2;
    emit_byte16(loop_offset, node->line);
    
    /* Patch skip jump */
    patch_jump(skip_jump);
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
    
    /* Compile body */
    ASTNode* body = node->as.fun_decl.body;
    for (int i = 0; i < body->as.block.count; i++) {
        compile_node(body->as.block.statements[i], 0);
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
    int name_idx = identifier_constant(name);
    
    /* Create class object */
    int class_reg = alloc_reg();
    emit_byte(OP_CLASS, node->line);
    emit_byte(class_reg, node->line);
    emit_byte16(name_idx, node->line);
    
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
                fprintf(stderr, "Unknown node type: %d\n", node->type);
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
