#include "zex/codegen.hpp"

namespace zex {

CodeGenerator::CodeGenerator(SemanticAnalyzer& semantic)
    : semantic_(semantic), stack_size_(0), entry_offset_(0) {}

void CodeGenerator::generate(const Program& program) {
    entry_offset_ = emitter_.current_offset();

    emitter_.emit_call_rel32(0);
    size_t main_call_site = emitter_.current_offset() - 4;

    emitter_.emit_mov_rdi_rax();
    emitter_.emit_mov_rax_imm32(60);
    emitter_.emit_syscall();

    for (const auto& func : program.functions) {
        generate_function(*func);
    }

    resolve_calls();

    auto it = function_offsets_.find("main");
    if (it != function_offsets_.end()) {
        size_t target = it->second;
        size_t after_call = main_call_site + 4;
        int32_t rel = static_cast<int32_t>(target - after_call);
        emitter_.patch_rel32(main_call_site, rel);
    }
}

int32_t CodeGenerator::calculate_stack_size(const Function& func) {
    int32_t size = 0;

    for (const auto& stmt : func.body) {
        if (dynamic_cast<VarDecl*>(stmt.get())) {
            size += 8;
        }
    }

    if (size % 16 != 0) {
        size = ((size / 16) + 1) * 16;
    }

    return size;
}

void CodeGenerator::generate_function(const Function& func) {
    function_offsets_[func.name] = emitter_.current_offset();

    local_offsets_.clear();
    stack_size_ = calculate_stack_size(func);

    emitter_.emit_push_rbp();
    emitter_.emit_mov_rbp_rsp();

    if (stack_size_ > 0) {
        emitter_.emit_sub_rsp_imm8(static_cast<uint8_t>(stack_size_));
    }

    int32_t offset = -8;
    for (const auto& stmt : func.body) {
        if (auto* var = dynamic_cast<VarDecl*>(stmt.get())) {
            local_offsets_[var->name] = offset;
            offset -= 8;
        }
    }

    for (const auto& stmt : func.body) {
        generate_statement(stmt.get());
    }
}

void CodeGenerator::generate_statement(const Statement* stmt) {
    if (auto* var_decl = dynamic_cast<const VarDecl*>(stmt)) {
        generate_expression(var_decl->initializer.get());
        int32_t offset = local_offsets_[var_decl->name];
        emitter_.emit_mov_rbp_offset_rax(offset);
    } else if (auto* assign = dynamic_cast<const AssignStmt*>(stmt)) {
        generate_expression(assign->value.get());
        int32_t offset = local_offsets_[assign->name];
        emitter_.emit_mov_rbp_offset_rax(offset);
    } else if (auto* ret = dynamic_cast<const ReturnStmt*>(stmt)) {
        generate_expression(ret->value.get());

        if (stack_size_ > 0) {
            emitter_.emit_add_rsp_imm8(static_cast<uint8_t>(stack_size_));
        }
        emitter_.emit_pop_rbp();
        emitter_.emit_ret();
    }
}

void CodeGenerator::generate_expression(const Expression* expr) {
    if (auto* lit = dynamic_cast<const IntLiteral*>(expr)) {
        emitter_.emit_mov_rax_imm64(lit->value);
    } else if (auto* ident = dynamic_cast<const Identifier*>(expr)) {
        int32_t offset = local_offsets_[ident->name];
        emitter_.emit_mov_rax_rbp_offset(offset);
    } else if (auto* call = dynamic_cast<const CallExpr*>(expr)) {
        emitter_.emit_call_rel32(0);

        CallPatch patch;
        patch.call_site = emitter_.current_offset() - 4;
        patch.target = call->callee;
        call_patches_.push_back(patch);
    } else if (auto* binary = dynamic_cast<const BinaryExpr*>(expr)) {
        generate_expression(binary->right.get());
        emitter_.emit_push_rax();
        generate_expression(binary->left.get());
        emitter_.emit_pop_rcx();

        switch (binary->op) {
            case BinaryOp::ADD:
                emitter_.emit_add_rax_rcx();
                break;
            case BinaryOp::SUB:
                emitter_.emit_sub_rax_rcx();
                break;
            case BinaryOp::MUL:
                emitter_.emit_imul_rax_rcx();
                break;
            case BinaryOp::DIV:
                emitter_.emit_cqo();
                emitter_.emit_idiv_rcx();
                break;
            case BinaryOp::MOD:
                emitter_.emit_cqo();
                emitter_.emit_idiv_rcx();
                emitter_.emit_mov_rax_rdx();
                break;
        }
    }
}

void CodeGenerator::resolve_calls() {
    for (const auto& patch : call_patches_) {
        auto it = function_offsets_.find(patch.target);
        if (it != function_offsets_.end()) {
            size_t target = it->second;
            size_t after_call = patch.call_site + 4;
            int32_t rel = static_cast<int32_t>(target - after_call);
            emitter_.patch_rel32(patch.call_site, rel);
        }
    }
}

}  // namespace zex
