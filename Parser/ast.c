/*
 * Zex Programming Language
 * Parser/ast.c - AST utilities
 */

#include "ast.h"
#include "memory.h"

static ASTNode* alloc_node(ASTNodeType type, int line, int column) {
    ASTNode* node = ALLOCATE(ASTNode, 1);
    node->type = type;
    node->line = line;
    node->column = column;
    return node;
}

ASTNode* ast_new_int_literal(int64_t value, int line, int column) {
    ASTNode* node = alloc_node(AST_INT_LITERAL, line, column);
    node->as.int_literal.value = value;
    return node;
}

ASTNode* ast_new_float_literal(double value, int line, int column) {
    ASTNode* node = alloc_node(AST_FLOAT_LITERAL, line, column);
    node->as.float_literal.value = value;
    return node;
}

ASTNode* ast_new_string_literal(const char* value, int length, int line, int column) {
    ASTNode* node = alloc_node(AST_STRING_LITERAL, line, column);
    node->as.string_literal.value = zex_strndup(value, length);
    node->as.string_literal.length = length;
    return node;
}

ASTNode* ast_new_bool_literal(bool value, int line, int column) {
    ASTNode* node = alloc_node(AST_BOOL_LITERAL, line, column);
    node->as.bool_literal.value = value;
    return node;
}

ASTNode* ast_new_null_literal(int line, int column) {
    return alloc_node(AST_NULL_LITERAL, line, column);
}

ASTNode* ast_new_identifier(const char* name, int line, int column) {
    ASTNode* node = alloc_node(AST_IDENTIFIER, line, column);
    node->as.identifier.name = zex_strdup(name);
    return node;
}

ASTNode* ast_new_binary(BinaryOp op, ASTNode* left, ASTNode* right, int line, int column) {
    ASTNode* node = alloc_node(AST_BINARY, line, column);
    node->as.binary.op = op;
    node->as.binary.left = left;
    node->as.binary.right = right;
    return node;
}

ASTNode* ast_new_unary(UnaryOp op, ASTNode* operand, int line, int column) {
    ASTNode* node = alloc_node(AST_UNARY, line, column);
    node->as.unary.op = op;
    node->as.unary.operand = operand;
    return node;
}

ASTNode* ast_new_call(ASTNode* callee, ASTNode** args, int arg_count, int line, int column) {
    ASTNode* node = alloc_node(AST_CALL, line, column);
    node->as.call.callee = callee;
    node->as.call.arguments = args;
    node->as.call.arg_count = arg_count;
    return node;
}

ASTNode* ast_new_get(ASTNode* object, const char* property, int line, int column) {
    ASTNode* node = alloc_node(AST_GET, line, column);
    node->as.get.object = object;
    node->as.get.property = zex_strdup(property);
    return node;
}

ASTNode* ast_new_set(ASTNode* object, const char* property, ASTNode* value, int line, int column) {
    ASTNode* node = alloc_node(AST_SET, line, column);
    node->as.set.object = object;
    node->as.set.property = zex_strdup(property);
    node->as.set.value = value;
    return node;
}

ASTNode* ast_new_set_compound(ASTNode* object, const char* property, BinaryOp op, ASTNode* value, int line, int column) {
    ASTNode* node = alloc_node(AST_SET_COMPOUND, line, column);
    node->as.set_compound.object = object;
    node->as.set_compound.property = zex_strdup(property);
    node->as.set_compound.op = op;
    node->as.set_compound.value = value;
    return node;
}

ASTNode* ast_new_self(int line, int column) {
    return alloc_node(AST_SELF, line, column);
}

ASTNode* ast_new_grouping(ASTNode* expression, int line, int column) {
    ASTNode* node = alloc_node(AST_GROUPING, line, column);
    node->as.grouping.expression = expression;
    return node;
}

ASTNode* ast_new_var_decl(const char* name, ASTNode* initializer, int line, int column) {
    ASTNode* node = alloc_node(AST_VAR_DECL, line, column);
    node->as.var_decl.name = zex_strdup(name);
    node->as.var_decl.initializer = initializer;
    return node;
}

ASTNode* ast_new_assign(const char* name, ASTNode* value, int line, int column) {
    ASTNode* node = alloc_node(AST_ASSIGN, line, column);
    node->as.assign.name = zex_strdup(name);
    node->as.assign.value = value;
    return node;
}

ASTNode* ast_new_compound_assign(const char* name, CompoundOp op, ASTNode* value, int line, int column) {
    ASTNode* node = alloc_node(AST_COMPOUND_ASSIGN, line, column);
    node->as.compound_assign.name = zex_strdup(name);
    node->as.compound_assign.op = op;
    node->as.compound_assign.value = value;
    return node;
}

ASTNode* ast_new_expr_stmt(ASTNode* expression, int line, int column) {
    ASTNode* node = alloc_node(AST_EXPR_STMT, line, column);
    node->as.expr_stmt.expression = expression;
    return node;
}

ASTNode* ast_new_block(ASTNode** statements, int count, int line, int column) {
    ASTNode* node = alloc_node(AST_BLOCK, line, column);
    node->as.block.statements = statements;
    node->as.block.count = count;
    return node;
}

ASTNode* ast_new_if(ASTNode* condition, ASTNode* then_branch, ASTNode* else_branch, int line, int column) {
    ASTNode* node = alloc_node(AST_IF, line, column);
    node->as.if_stmt.condition = condition;
    node->as.if_stmt.then_branch = then_branch;
    node->as.if_stmt.else_branch = else_branch;
    return node;
}

ASTNode* ast_new_while(ASTNode* condition, ASTNode* body, int line, int column) {
    ASTNode* node = alloc_node(AST_WHILE, line, column);
    node->as.while_stmt.condition = condition;
    node->as.while_stmt.body = body;
    return node;
}

ASTNode* ast_new_do_while(ASTNode* body, ASTNode* condition, int line, int column) {
    ASTNode* node = alloc_node(AST_DO_WHILE, line, column);
    node->as.do_while_stmt.body = body;
    node->as.do_while_stmt.condition = condition;
    return node;
}

ASTNode* ast_new_for(ASTNode* initializer, ASTNode* condition, ASTNode* update, ASTNode* body, int line, int column) {
    ASTNode* node = alloc_node(AST_FOR, line, column);
    node->as.for_stmt.initializer = initializer;
    node->as.for_stmt.condition = condition;
    node->as.for_stmt.update = update;
    node->as.for_stmt.body = body;
    return node;
}

ASTNode* ast_new_for_in(const char* var_name, ASTNode* iterable, ASTNode* body, int line, int column) {
    ASTNode* node = alloc_node(AST_FOR_IN, line, column);
    node->as.for_in_stmt.var_name = zex_strdup(var_name);
    node->as.for_in_stmt.iterable = iterable;
    node->as.for_in_stmt.body = body;
    return node;
}

ASTNode* ast_new_break(int line, int column) {
    return alloc_node(AST_BREAK, line, column);
}

ASTNode* ast_new_continue(int line, int column) {
    return alloc_node(AST_CONTINUE, line, column);
}

ASTNode* ast_new_return(ASTNode* value, int line, int column) {
    ASTNode* node = alloc_node(AST_RETURN, line, column);
    node->as.return_stmt.value = value;
    return node;
}

ASTNode* ast_new_fun_decl(const char* name, ParameterList params, ASTNode* body, int line, int column) {
    ASTNode* node = alloc_node(AST_FUN_DECL, line, column);
    node->as.fun_decl.name = zex_strdup(name);
    node->as.fun_decl.params = params;
    node->as.fun_decl.body = body;
    return node;
}

ASTNode* ast_new_class_decl(const char* name, const char* superclass, ASTNode** methods, int method_count, int line, int column) {
    ASTNode* node = alloc_node(AST_CLASS_DECL, line, column);
    node->as.class_decl.name = zex_strdup(name);
    node->as.class_decl.superclass = superclass ? zex_strdup(superclass) : NULL;
    node->as.class_decl.methods = methods;
    node->as.class_decl.method_count = method_count;
    return node;
}

ASTNode* ast_new_array(ASTNode** elements, int count, int line, int column) {
    ASTNode* node = alloc_node(AST_ARRAY, line, column);
    node->as.array.elements = elements;
    node->as.array.count = count;
    return node;
}

ASTNode* ast_new_index_get(ASTNode* object, ASTNode* index, int line, int column) {
    ASTNode* node = alloc_node(AST_INDEX_GET, line, column);
    node->as.index_get.object = object;
    node->as.index_get.index = index;
    return node;
}

ASTNode* ast_new_index_set(ASTNode* object, ASTNode* index, ASTNode* value, int line, int column) {
    ASTNode* node = alloc_node(AST_INDEX_SET, line, column);
    node->as.index_set.object = object;
    node->as.index_set.index = index;
    node->as.index_set.value = value;
    return node;
}

ASTNode* ast_new_closure(ParameterList params, ASTNode* body, bool is_expression, int line, int column) {
    ASTNode* node = alloc_node(AST_CLOSURE, line, column);
    node->as.closure.params = params;
    node->as.closure.body = body;
    node->as.closure.is_expression = is_expression;
    return node;
}

ASTNode* ast_new_program(ASTNode** statements, int count) {
    ASTNode* node = alloc_node(AST_PROGRAM, 1, 1);
    node->as.program.statements = statements;
    node->as.program.count = count;
    return node;
}

void ast_free(ASTNode* node) {
    if (node == NULL) return;
    
    switch (node->type) {
        case AST_INT_LITERAL:
        case AST_FLOAT_LITERAL:
        case AST_BOOL_LITERAL:
        case AST_NULL_LITERAL:
        case AST_SELF:
            break;
            
        case AST_STRING_LITERAL:
            zex_free(node->as.string_literal.value, node->as.string_literal.length + 1);
            break;
            
        case AST_IDENTIFIER:
            zex_free(node->as.identifier.name, strlen(node->as.identifier.name) + 1);
            break;
            
        case AST_BINARY:
            ast_free(node->as.binary.left);
            ast_free(node->as.binary.right);
            break;
            
        case AST_UNARY:
            ast_free(node->as.unary.operand);
            break;
            
        case AST_CALL:
            ast_free(node->as.call.callee);
            for (int i = 0; i < node->as.call.arg_count; i++) {
                ast_free(node->as.call.arguments[i]);
            }
            FREE_ARRAY(ASTNode*, node->as.call.arguments, node->as.call.arg_count);
            break;
            
        case AST_GET:
            ast_free(node->as.get.object);
            zex_free(node->as.get.property, strlen(node->as.get.property) + 1);
            break;
            
        case AST_SET:
            ast_free(node->as.set.object);
            zex_free(node->as.set.property, strlen(node->as.set.property) + 1);
            ast_free(node->as.set.value);
            break;
            
        case AST_SET_COMPOUND:
            ast_free(node->as.set_compound.object);
            zex_free(node->as.set_compound.property, strlen(node->as.set_compound.property) + 1);
            ast_free(node->as.set_compound.value);
            break;
            
        case AST_GROUPING:
            ast_free(node->as.grouping.expression);
            break;
            
        case AST_ARRAY:
            for (int i = 0; i < node->as.array.count; i++) {
                ast_free(node->as.array.elements[i]);
            }
            FREE_ARRAY(ASTNode*, node->as.array.elements, node->as.array.count);
            break;
            
        case AST_INDEX_GET:
            ast_free(node->as.index_get.object);
            ast_free(node->as.index_get.index);
            break;
            
        case AST_INDEX_SET:
            ast_free(node->as.index_set.object);
            ast_free(node->as.index_set.index);
            ast_free(node->as.index_set.value);
            break;
            
        case AST_VAR_DECL:
            zex_free(node->as.var_decl.name, strlen(node->as.var_decl.name) + 1);
            ast_free(node->as.var_decl.initializer);
            break;
            
        case AST_ASSIGN:
            zex_free(node->as.assign.name, strlen(node->as.assign.name) + 1);
            ast_free(node->as.assign.value);
            break;
            
        case AST_COMPOUND_ASSIGN:
            zex_free(node->as.compound_assign.name, strlen(node->as.compound_assign.name) + 1);
            ast_free(node->as.compound_assign.value);
            break;
            
        case AST_EXPR_STMT:
            ast_free(node->as.expr_stmt.expression);
            break;
            
        case AST_BLOCK:
            for (int i = 0; i < node->as.block.count; i++) {
                ast_free(node->as.block.statements[i]);
            }
            FREE_ARRAY(ASTNode*, node->as.block.statements, node->as.block.count);
            break;
            
        case AST_IF:
            ast_free(node->as.if_stmt.condition);
            ast_free(node->as.if_stmt.then_branch);
            ast_free(node->as.if_stmt.else_branch);
            break;
            
        case AST_WHILE:
            ast_free(node->as.while_stmt.condition);
            ast_free(node->as.while_stmt.body);
            break;
            
        case AST_DO_WHILE:
            ast_free(node->as.do_while_stmt.body);
            ast_free(node->as.do_while_stmt.condition);
            break;
            
        case AST_FOR:
            ast_free(node->as.for_stmt.initializer);
            ast_free(node->as.for_stmt.condition);
            ast_free(node->as.for_stmt.update);
            ast_free(node->as.for_stmt.body);
            break;
            
        case AST_FOR_IN:
            zex_free(node->as.for_in_stmt.var_name, strlen(node->as.for_in_stmt.var_name) + 1);
            ast_free(node->as.for_in_stmt.iterable);
            ast_free(node->as.for_in_stmt.body);
            break;
            
        case AST_BREAK:
        case AST_CONTINUE:
            /* No children to free */
            break;
            
        case AST_RETURN:
            ast_free(node->as.return_stmt.value);
            break;
            
        case AST_FUN_DECL:
            zex_free(node->as.fun_decl.name, strlen(node->as.fun_decl.name) + 1);
            for (int i = 0; i < node->as.fun_decl.params.count; i++) {
                zex_free(node->as.fun_decl.params.names[i], 
                        strlen(node->as.fun_decl.params.names[i]) + 1);
            }
            FREE_ARRAY(char*, node->as.fun_decl.params.names, node->as.fun_decl.params.capacity);
            ast_free(node->as.fun_decl.body);
            break;
            
        case AST_CLASS_DECL:
            zex_free(node->as.class_decl.name, strlen(node->as.class_decl.name) + 1);
            if (node->as.class_decl.superclass) {
                zex_free(node->as.class_decl.superclass, strlen(node->as.class_decl.superclass) + 1);
            }
            for (int i = 0; i < node->as.class_decl.method_count; i++) {
                ast_free(node->as.class_decl.methods[i]);
            }
            FREE_ARRAY(ASTNode*, node->as.class_decl.methods, node->as.class_decl.method_count);
            break;
            
        case AST_CLOSURE:
            for (int i = 0; i < node->as.closure.params.count; i++) {
                zex_free(node->as.closure.params.names[i],
                        strlen(node->as.closure.params.names[i]) + 1);
            }
            FREE_ARRAY(char*, node->as.closure.params.names, node->as.closure.params.capacity);
            ast_free(node->as.closure.body);
            break;
            
        case AST_PROGRAM:
            for (int i = 0; i < node->as.program.count; i++) {
                ast_free(node->as.program.statements[i]);
            }
            FREE_ARRAY(ASTNode*, node->as.program.statements, node->as.program.count);
            break;
    }
    
    FREE(ASTNode, node);
}

static void print_indent(int indent) {
    for (int i = 0; i < indent; i++) {
        printf("  ");
    }
}

void ast_print(ASTNode* node, int indent) {
    if (node == NULL) {
        print_indent(indent);
        printf("(null)\n");
        return;
    }
    
    print_indent(indent);
    
    switch (node->type) {
        case AST_INT_LITERAL:
            printf("INT(%lld)\n", (long long)node->as.int_literal.value);
            break;
        case AST_FLOAT_LITERAL:
            printf("FLOAT(%g)\n", node->as.float_literal.value);
            break;
        case AST_STRING_LITERAL:
            printf("STRING(\"%s\")\n", node->as.string_literal.value);
            break;
        case AST_BOOL_LITERAL:
            printf("BOOL(%s)\n", node->as.bool_literal.value ? "true" : "false");
            break;
        case AST_NULL_LITERAL:
            printf("NULL\n");
            break;
        case AST_IDENTIFIER:
            printf("ID(%s)\n", node->as.identifier.name);
            break;
        case AST_SELF:
            printf("SELF\n");
            break;
        case AST_BINARY:
            printf("BINARY(op=%d)\n", node->as.binary.op);
            ast_print(node->as.binary.left, indent + 1);
            ast_print(node->as.binary.right, indent + 1);
            break;
        case AST_UNARY:
            printf("UNARY(op=%d)\n", node->as.unary.op);
            ast_print(node->as.unary.operand, indent + 1);
            break;
        case AST_CALL:
            printf("CALL(argc=%d)\n", node->as.call.arg_count);
            ast_print(node->as.call.callee, indent + 1);
            for (int i = 0; i < node->as.call.arg_count; i++) {
                ast_print(node->as.call.arguments[i], indent + 1);
            }
            break;
        case AST_GET:
            printf("GET(.%s)\n", node->as.get.property);
            ast_print(node->as.get.object, indent + 1);
            break;
        case AST_SET:
            printf("SET(.%s)\n", node->as.set.property);
            ast_print(node->as.set.object, indent + 1);
            ast_print(node->as.set.value, indent + 1);
            break;
        case AST_SET_COMPOUND:
            printf("SET_COMPOUND(.%s)\n", node->as.set_compound.property);
            ast_print(node->as.set_compound.object, indent + 1);
            ast_print(node->as.set_compound.value, indent + 1);
            break;
        case AST_GROUPING:
            printf("GROUPING\n");
            ast_print(node->as.grouping.expression, indent + 1);
            break;
        case AST_ARRAY:
            printf("ARRAY(%d elements)\n", node->as.array.count);
            for (int i = 0; i < node->as.array.count; i++) {
                ast_print(node->as.array.elements[i], indent + 1);
            }
            break;
        case AST_INDEX_GET:
            printf("INDEX_GET\n");
            ast_print(node->as.index_get.object, indent + 1);
            ast_print(node->as.index_get.index, indent + 1);
            break;
        case AST_INDEX_SET:
            printf("INDEX_SET\n");
            ast_print(node->as.index_set.object, indent + 1);
            ast_print(node->as.index_set.index, indent + 1);
            ast_print(node->as.index_set.value, indent + 1);
            break;
        case AST_VAR_DECL:
            printf("VAR(%s)\n", node->as.var_decl.name);
            if (node->as.var_decl.initializer) {
                ast_print(node->as.var_decl.initializer, indent + 1);
            }
            break;
        case AST_ASSIGN:
            printf("ASSIGN(%s)\n", node->as.assign.name);
            ast_print(node->as.assign.value, indent + 1);
            break;
        case AST_COMPOUND_ASSIGN:
            printf("COMPOUND_ASSIGN(%s, op=%d)\n", node->as.compound_assign.name, 
                   node->as.compound_assign.op);
            ast_print(node->as.compound_assign.value, indent + 1);
            break;
        case AST_EXPR_STMT:
            printf("EXPR_STMT\n");
            ast_print(node->as.expr_stmt.expression, indent + 1);
            break;
        case AST_BLOCK:
            printf("BLOCK(%d stmts)\n", node->as.block.count);
            for (int i = 0; i < node->as.block.count; i++) {
                ast_print(node->as.block.statements[i], indent + 1);
            }
            break;
        case AST_IF:
            printf("IF\n");
            ast_print(node->as.if_stmt.condition, indent + 1);
            ast_print(node->as.if_stmt.then_branch, indent + 1);
            if (node->as.if_stmt.else_branch) {
                print_indent(indent);
                printf("ELSE\n");
                ast_print(node->as.if_stmt.else_branch, indent + 1);
            }
            break;
        case AST_WHILE:
            printf("WHILE\n");
            ast_print(node->as.while_stmt.condition, indent + 1);
            ast_print(node->as.while_stmt.body, indent + 1);
            break;
        case AST_DO_WHILE:
            printf("DO_WHILE\n");
            ast_print(node->as.do_while_stmt.body, indent + 1);
            ast_print(node->as.do_while_stmt.condition, indent + 1);
            break;
        case AST_FOR:
            printf("FOR\n");
            if (node->as.for_stmt.initializer) {
                print_indent(indent);
                printf("INIT:\n");
                ast_print(node->as.for_stmt.initializer, indent + 1);
            }
            if (node->as.for_stmt.condition) {
                print_indent(indent);
                printf("COND:\n");
                ast_print(node->as.for_stmt.condition, indent + 1);
            }
            if (node->as.for_stmt.update) {
                print_indent(indent);
                printf("UPDATE:\n");
                ast_print(node->as.for_stmt.update, indent + 1);
            }
            ast_print(node->as.for_stmt.body, indent + 1);
            break;
        case AST_FOR_IN:
            printf("FOR_IN(%s)\n", node->as.for_in_stmt.var_name);
            ast_print(node->as.for_in_stmt.iterable, indent + 1);
            ast_print(node->as.for_in_stmt.body, indent + 1);
            break;
        case AST_BREAK:
            printf("BREAK\n");
            break;
        case AST_CONTINUE:
            printf("CONTINUE\n");
            break;
        case AST_RETURN:
            printf("RETURN\n");
            if (node->as.return_stmt.value) {
                ast_print(node->as.return_stmt.value, indent + 1);
            }
            break;
        case AST_FUN_DECL:
            printf("FUN(%s, params=%d)\n", node->as.fun_decl.name, 
                   node->as.fun_decl.params.count);
            ast_print(node->as.fun_decl.body, indent + 1);
            break;
        case AST_CLASS_DECL:
            if (node->as.class_decl.superclass) {
                printf("CLASS(%s < %s)\n", node->as.class_decl.name, node->as.class_decl.superclass);
            } else {
                printf("CLASS(%s)\n", node->as.class_decl.name);
            }
            for (int i = 0; i < node->as.class_decl.method_count; i++) {
                ast_print(node->as.class_decl.methods[i], indent + 1);
            }
            break;
        case AST_CLOSURE:
            printf("CLOSURE(params=%d, expr=%s)\n", node->as.closure.params.count,
                   node->as.closure.is_expression ? "true" : "false");
            ast_print(node->as.closure.body, indent + 1);
            break;
        case AST_PROGRAM:
            printf("PROGRAM(%d stmts)\n", node->as.program.count);
            for (int i = 0; i < node->as.program.count; i++) {
                ast_print(node->as.program.statements[i], indent + 1);
            }
            break;
    }
}
