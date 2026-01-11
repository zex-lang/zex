#include "zex/semantic.hpp"

namespace zex {

static std::string type_to_string(const Type& t) {
    switch (t.kind) {
        case TypeKind::VOID:
            return "void";
        case TypeKind::CHAR:
            return "char";
        case TypeKind::I8:
            return "i8";
        case TypeKind::I16:
            return "i16";
        case TypeKind::I32:
            return "i32";
        case TypeKind::I64:
            return "i64";
        case TypeKind::F32:
            return "f32";
        case TypeKind::BOOL:
            return "bool";
        case TypeKind::POINTER:
            return "*" + (t.element_type ? type_to_string(*t.element_type) : "unknown");
        case TypeKind::ARRAY:
            return "[" + (t.element_type ? type_to_string(*t.element_type) : "unknown") + "]";
    }
    return "unknown";
}

static int type_rank(TypeKind k) {
    switch (k) {
        case TypeKind::BOOL:
            return 0;
        case TypeKind::CHAR:
        case TypeKind::I8:
            return 1;
        case TypeKind::I16:
            return 2;
        case TypeKind::I32:
            return 3;
        case TypeKind::I64:
            return 4;
        default:
            return -1;
    }
}

static bool value_fits_in_type(int64_t value, TypeKind kind) {
    switch (kind) {
        case TypeKind::I8:
            return value >= -128 && value <= 127;
        case TypeKind::I16:
            return value >= -32768 && value <= 32767;
        case TypeKind::I32:
            return value >= -2147483648LL && value <= 2147483647LL;
        case TypeKind::I64:
            return true;
        case TypeKind::CHAR:
            return value >= 0 && value <= 255;
        case TypeKind::BOOL:
            return value == 0 || value == 1;
        default:
            return false;
    }
}

static bool can_implicit_convert(const Type& from, const Type& to) {
    if (from.equals(to)) {
        return true;
    }

    int from_rank = type_rank(from.kind);
    int to_rank = type_rank(to.kind);

    if (from_rank >= 0 && to_rank >= 0) {
        return from_rank <= to_rank;
    }

    return false;
}

static bool can_literal_convert(int64_t value, const Type& to) {
    return value_fits_in_type(value, to.kind);
}

static bool types_compatible(const Type& expected, const Type& actual) {
    return can_implicit_convert(actual, expected);
}

void SemanticAnalyzer::analyze(Program& program) {
    register_functions(program);

    if (function_table_.find("main") == function_table_.end()) {
        throw CompileError(ErrorCode::NO_MAIN_FUNCTION, {});
    }

    for (auto& func : program.functions) {
        analyze_function(*func);
    }
}

void SemanticAnalyzer::register_functions(Program& program) {
    function_table_.clear();
    functions_.clear();

    size_t index = 0;
    for (auto& func : program.functions) {
        if (function_table_.find(func->name) != function_table_.end()) {
            throw CompileError(ErrorCode::DUPLICATE_FUNCTION, {}, "'" + func->name + "'");
        }

        FunctionInfo info;
        info.name = func->name;
        info.return_type = func->return_type;
        info.index = index;

        for (const auto& param : func->params) {
            info.param_types.push_back(param.type);
        }

        function_table_[func->name] = info;
        functions_.push_back(info);
        index++;
    }
}

void SemanticAnalyzer::analyze_function(Function& func) {
    local_variables_.clear();
    next_stack_offset_ = -8;
    current_return_type_ = func.return_type;

    for (const auto& param : func.params) {
        if (local_variables_.find(param.name) != local_variables_.end()) {
            throw CompileError(ErrorCode::DUPLICATE_VARIABLE, {}, "'" + param.name + "'");
        }

        VariableInfo info;
        info.name = param.name;
        info.type = param.type;
        info.stack_offset = next_stack_offset_;
        info.is_const = false;
        info.const_value = 0;
        next_stack_offset_ -= 8;

        local_variables_[param.name] = info;
    }

    for (auto& stmt : func.body) {
        analyze_statement(stmt.get());
    }
}

void SemanticAnalyzer::analyze_statement(Statement* stmt) {
    if (auto* var_decl = dynamic_cast<VarDecl*>(stmt)) {
        if (local_variables_.find(var_decl->name) != local_variables_.end()) {
            throw CompileError(ErrorCode::DUPLICATE_VARIABLE, {}, "'" + var_decl->name + "'");
        }

        analyze_expression(var_decl->initializer.get());

        VariableInfo info;
        info.name = var_decl->name;
        info.type = var_decl->type;
        info.stack_offset = next_stack_offset_;
        info.is_const = false;
        info.const_value = 0;
        next_stack_offset_ -= 8;

        local_variables_[var_decl->name] = info;
    } else if (auto* const_decl = dynamic_cast<ConstDecl*>(stmt)) {
        if (local_variables_.find(const_decl->name) != local_variables_.end()) {
            throw CompileError(ErrorCode::DUPLICATE_VARIABLE, {}, "'" + const_decl->name + "'");
        }

        VariableInfo info;
        info.name = const_decl->name;
        info.type = const_decl->type;
        info.stack_offset = 0;
        info.is_const = true;
        info.const_value = const_decl->value;

        local_variables_[const_decl->name] = info;
    } else if (auto* assign = dynamic_cast<AssignStmt*>(stmt)) {
        auto it = local_variables_.find(assign->name);
        if (it == local_variables_.end()) {
            throw CompileError(ErrorCode::UNDEFINED_VARIABLE, {}, "'" + assign->name + "'");
        }
        if (it->second.is_const) {
            throw CompileError(ErrorCode::DUPLICATE_VARIABLE, {}, "'" + assign->name + "'");
        }
        analyze_expression(assign->value.get());
    } else if (auto* idx_assign = dynamic_cast<IndexAssignStmt*>(stmt)) {
        analyze_expression(idx_assign->target.get());
        analyze_expression(idx_assign->value.get());
    } else if (auto* ret = dynamic_cast<ReturnStmt*>(stmt)) {
        if (ret->value) {
            analyze_expression(ret->value.get());
            Type expr_type = get_expression_type(ret->value.get());

            bool compatible = false;
            if (auto* int_lit = dynamic_cast<IntLiteral*>(ret->value.get())) {
                compatible = can_literal_convert(int_lit->value, current_return_type_);
            } else {
                compatible = types_compatible(current_return_type_, expr_type);
            }

            if (!compatible) {
                std::string msg = "cannot convert '" + type_to_string(expr_type) + "' to '" +
                                  type_to_string(current_return_type_) + "'";
                throw CompileError(ErrorCode::IMPLICIT_NARROWING, {}, msg);
            }
        } else if (current_return_type_.kind != TypeKind::VOID) {
            throw CompileError(ErrorCode::INVALID_RETURN_TYPE, {},
                               "function requires return value");
        }
    } else if (auto* if_stmt = dynamic_cast<IfStmt*>(stmt)) {
        analyze_expression(if_stmt->condition.get());
        for (auto& s : if_stmt->then_body) {
            analyze_statement(s.get());
        }
        for (auto& s : if_stmt->else_body) {
            analyze_statement(s.get());
        }
    } else if (auto* asm_block = dynamic_cast<AsmBlock*>(stmt)) {
        for (const auto& instr : asm_block->instructions) {
            for (const auto& op : instr.operands) {
                if (op.kind == AsmOperandKind::VAR) {
                    if (local_variables_.find(op.var_name) == local_variables_.end()) {
                        throw CompileError(ErrorCode::UNDEFINED_VARIABLE, {},
                                           "'" + op.var_name + "'");
                    }
                }
            }
        }
    } else if (auto* expr_stmt = dynamic_cast<ExprStmt*>(stmt)) {
        analyze_expression(expr_stmt->expr.get());
    }
}

void SemanticAnalyzer::analyze_expression(Expression* expr) {
    if (dynamic_cast<IntLiteral*>(expr)) {
        return;
    }

    if (dynamic_cast<FloatLiteral*>(expr)) {
        return;
    }

    if (dynamic_cast<CharLiteral*>(expr)) {
        return;
    }

    if (dynamic_cast<StringLiteral*>(expr)) {
        return;
    }

    if (dynamic_cast<BoolLiteral*>(expr)) {
        return;
    }

    if (dynamic_cast<SizeofExpr*>(expr)) {
        return;
    }

    if (auto* arr_lit = dynamic_cast<ArrayLiteral*>(expr)) {
        for (auto& elem : arr_lit->elements) {
            analyze_expression(elem.get());
        }
        return;
    }

    if (auto* idx = dynamic_cast<IndexExpr*>(expr)) {
        analyze_expression(idx->array.get());
        analyze_expression(idx->index.get());
        return;
    }

    if (auto* ident = dynamic_cast<Identifier*>(expr)) {
        if (local_variables_.find(ident->name) == local_variables_.end()) {
            throw CompileError(ErrorCode::UNDEFINED_VARIABLE, {}, "'" + ident->name + "'");
        }
        return;
    }

    if (auto* call = dynamic_cast<CallExpr*>(expr)) {
        if (function_table_.find(call->callee) == function_table_.end()) {
            throw CompileError(ErrorCode::UNDEFINED_FUNCTION, {}, "'" + call->callee + "'");
        }
        for (auto& arg : call->args) {
            analyze_expression(arg.get());
        }
        return;
    }

    if (auto* unary = dynamic_cast<UnaryExpr*>(expr)) {
        analyze_expression(unary->operand.get());
        return;
    }

    if (auto* binary = dynamic_cast<BinaryExpr*>(expr)) {
        analyze_expression(binary->left.get());
        analyze_expression(binary->right.get());
        return;
    }

    if (auto* cast = dynamic_cast<CastExpr*>(expr)) {
        analyze_expression(cast->expr.get());
        return;
    }

    throw CompileError(ErrorCode::UNKNOWN_EXPRESSION, {});
}

const FunctionInfo* SemanticAnalyzer::get_function(const std::string& name) const {
    auto it = function_table_.find(name);
    if (it != function_table_.end()) {
        return &it->second;
    }
    return nullptr;
}

const VariableInfo* SemanticAnalyzer::get_variable(const std::string& name) const {
    auto it = local_variables_.find(name);
    if (it != local_variables_.end()) {
        return &it->second;
    }
    return nullptr;
}

Type SemanticAnalyzer::get_expression_type(Expression* expr) {
    if (dynamic_cast<IntLiteral*>(expr)) {
        return Type(TypeKind::I64);
    }
    if (dynamic_cast<FloatLiteral*>(expr)) {
        return Type(TypeKind::F32);
    }
    if (dynamic_cast<CharLiteral*>(expr)) {
        return Type(TypeKind::CHAR);
    }
    if (dynamic_cast<BoolLiteral*>(expr)) {
        return Type(TypeKind::BOOL);
    }
    if (dynamic_cast<StringLiteral*>(expr)) {
        Type ptr(TypeKind::POINTER);
        ptr.element_type = std::make_unique<Type>(TypeKind::CHAR);
        return ptr;
    }
    if (auto* ident = dynamic_cast<Identifier*>(expr)) {
        auto it = local_variables_.find(ident->name);
        if (it != local_variables_.end()) {
            return it->second.type;
        }
        return Type(TypeKind::I64);
    }
    if (auto* call = dynamic_cast<CallExpr*>(expr)) {
        auto it = function_table_.find(call->callee);
        if (it != function_table_.end()) {
            return it->second.return_type;
        }
        return Type(TypeKind::I64);
    }
    if (auto* binary = dynamic_cast<BinaryExpr*>(expr)) {
        switch (binary->op) {
            case BinaryOp::EQ:
            case BinaryOp::NE:
            case BinaryOp::LT:
            case BinaryOp::LE:
            case BinaryOp::GT:
            case BinaryOp::GE:
            case BinaryOp::AND:
            case BinaryOp::OR:
                return Type(TypeKind::BOOL);
            default:
                return get_expression_type(binary->left.get());
        }
    }
    if (auto* unary = dynamic_cast<UnaryExpr*>(expr)) {
        if (unary->op == UnaryOp::NOT) {
            return Type(TypeKind::BOOL);
        }
        return get_expression_type(unary->operand.get());
    }
    if (auto* idx = dynamic_cast<IndexExpr*>(expr)) {
        Type arr = get_expression_type(idx->array.get());
        if ((arr.kind == TypeKind::ARRAY || arr.kind == TypeKind::POINTER) && arr.element_type) {
            return *arr.element_type;
        }
        return Type(TypeKind::I64);
    }
    if (dynamic_cast<SizeofExpr*>(expr)) {
        return Type(TypeKind::I64);
    }
    if (auto* cast = dynamic_cast<CastExpr*>(expr)) {
        return cast->target_type;
    }
    return Type(TypeKind::I64);
}

}  // namespace zex
