#include "zex/semantic.hpp"

namespace zex {

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
            throw CompileError(ErrorCode::DUPLICATE_FUNCTION, {}, func->name);
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

    for (const auto& param : func.params) {
        if (local_variables_.find(param.name) != local_variables_.end()) {
            throw CompileError(ErrorCode::DUPLICATE_VARIABLE, {}, param.name);
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
            throw CompileError(ErrorCode::DUPLICATE_VARIABLE, {}, var_decl->name);
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
            throw CompileError(ErrorCode::DUPLICATE_VARIABLE, {}, const_decl->name);
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
            throw CompileError(ErrorCode::UNDEFINED_VARIABLE, {}, assign->name);
        }
        if (it->second.is_const) {
            throw CompileError(ErrorCode::DUPLICATE_VARIABLE, {}, assign->name);
        }
        analyze_expression(assign->value.get());
    } else if (auto* ret = dynamic_cast<ReturnStmt*>(stmt)) {
        analyze_expression(ret->value.get());
    } else if (auto* if_stmt = dynamic_cast<IfStmt*>(stmt)) {
        analyze_expression(if_stmt->condition.get());
        for (auto& s : if_stmt->then_body) {
            analyze_statement(s.get());
        }
        for (auto& s : if_stmt->else_body) {
            analyze_statement(s.get());
        }
    }
}

void SemanticAnalyzer::analyze_expression(Expression* expr) {
    if (dynamic_cast<IntLiteral*>(expr)) {
        return;
    }

    if (dynamic_cast<BoolLiteral*>(expr)) {
        return;
    }

    if (auto* ident = dynamic_cast<Identifier*>(expr)) {
        if (local_variables_.find(ident->name) == local_variables_.end()) {
            throw CompileError(ErrorCode::UNDEFINED_VARIABLE, {}, ident->name);
        }
        return;
    }

    if (auto* call = dynamic_cast<CallExpr*>(expr)) {
        if (function_table_.find(call->callee) == function_table_.end()) {
            throw CompileError(ErrorCode::UNDEFINED_FUNCTION, {}, call->callee);
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

}  // namespace zex
