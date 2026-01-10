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

        function_table_[func->name] = info;
        functions_.push_back(info);
        index++;
    }
}

void SemanticAnalyzer::analyze_function(Function& func) {
    local_variables_.clear();
    next_stack_offset_ = -8;

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
        next_stack_offset_ -= 8;

        local_variables_[var_decl->name] = info;
    } else if (auto* ret = dynamic_cast<ReturnStmt*>(stmt)) {
        analyze_expression(ret->value.get());
    }
}

void SemanticAnalyzer::analyze_expression(Expression* expr) {
    if (dynamic_cast<IntLiteral*>(expr)) {
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

}  // namespace zex
