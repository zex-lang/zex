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
    int32_t size = static_cast<int32_t>(func.params.size() * 8);

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

    locals_.clear();
    stack_size_ = calculate_stack_size(func);

    emitter_.emit_push_rbp();
    emitter_.emit_mov_rbp_rsp();

    if (stack_size_ > 0) {
        emitter_.emit_sub_rsp_imm8(static_cast<uint8_t>(stack_size_));
    }

    int32_t offset = -8;
    for (size_t i = 0; i < func.params.size(); i++) {
        LocalVar lv;
        lv.stack_offset = offset;
        lv.is_const = false;
        lv.const_value = 0;
        locals_[func.params[i].name] = lv;

        switch (i) {
            case 0:
                emitter_.emit_mov_rbp_offset_rdi(offset);
                break;
            case 1:
                emitter_.emit_mov_rbp_offset_rsi(offset);
                break;
            case 2:
                emitter_.emit_mov_rbp_offset_rdx(offset);
                break;
            case 3:
                emitter_.emit_mov_rbp_offset_rcx(offset);
                break;
            case 4:
                emitter_.emit_mov_rbp_offset_r8(offset);
                break;
            case 5:
                emitter_.emit_mov_rbp_offset_r9(offset);
                break;
        }
        offset -= 8;
    }

    for (const auto& stmt : func.body) {
        if (auto* var = dynamic_cast<VarDecl*>(stmt.get())) {
            LocalVar lv;
            lv.stack_offset = offset;
            lv.is_const = false;
            lv.const_value = 0;
            locals_[var->name] = lv;
            offset -= 8;
        } else if (auto* cst = dynamic_cast<ConstDecl*>(stmt.get())) {
            LocalVar lv;
            lv.stack_offset = 0;
            lv.is_const = true;
            lv.const_value = cst->value;
            locals_[cst->name] = lv;
        }
    }

    for (const auto& stmt : func.body) {
        generate_statement(stmt.get());
    }
}

void CodeGenerator::generate_statement(const Statement* stmt) {
    if (auto* var_decl = dynamic_cast<const VarDecl*>(stmt)) {
        generate_expression(var_decl->initializer.get());
        int32_t offset = locals_[var_decl->name].stack_offset;
        emitter_.emit_mov_rbp_offset_rax(offset);
    } else if (auto* assign = dynamic_cast<const AssignStmt*>(stmt)) {
        generate_expression(assign->value.get());
        int32_t offset = locals_[assign->name].stack_offset;
        emitter_.emit_mov_rbp_offset_rax(offset);
    } else if (auto* ret = dynamic_cast<const ReturnStmt*>(stmt)) {
        generate_expression(ret->value.get());

        if (stack_size_ > 0) {
            emitter_.emit_add_rsp_imm8(static_cast<uint8_t>(stack_size_));
        }
        emitter_.emit_pop_rbp();
        emitter_.emit_ret();
    } else if (auto* if_stmt = dynamic_cast<const IfStmt*>(stmt)) {
        generate_expression(if_stmt->condition.get());
        emitter_.emit_test_rax_rax();
        emitter_.emit_jz_rel32(0);
        size_t else_patch = emitter_.current_offset() - 4;

        for (const auto& s : if_stmt->then_body) {
            generate_statement(s.get());
        }

        if (!if_stmt->else_body.empty()) {
            emitter_.emit_jmp_rel32(0);
            size_t end_patch = emitter_.current_offset() - 4;

            size_t else_target = emitter_.current_offset();
            int32_t else_rel = static_cast<int32_t>(else_target - (else_patch + 4));
            emitter_.patch_rel32(else_patch, else_rel);

            for (const auto& s : if_stmt->else_body) {
                generate_statement(s.get());
            }

            size_t end_target = emitter_.current_offset();
            int32_t end_rel = static_cast<int32_t>(end_target - (end_patch + 4));
            emitter_.patch_rel32(end_patch, end_rel);
        } else {
            size_t end_target = emitter_.current_offset();
            int32_t else_rel = static_cast<int32_t>(end_target - (else_patch + 4));
            emitter_.patch_rel32(else_patch, else_rel);
        }
    }
}

void CodeGenerator::generate_expression(const Expression* expr) {
    if (auto* lit = dynamic_cast<const IntLiteral*>(expr)) {
        emitter_.emit_mov_rax_imm64(lit->value);
    } else if (auto* bl = dynamic_cast<const BoolLiteral*>(expr)) {
        emitter_.emit_mov_rax_imm64(bl->value ? 1 : 0);
    } else if (auto* ident = dynamic_cast<const Identifier*>(expr)) {
        const LocalVar& lv = locals_[ident->name];
        if (lv.is_const) {
            emitter_.emit_mov_rax_imm64(lv.const_value);
        } else {
            emitter_.emit_mov_rax_rbp_offset(lv.stack_offset);
        }
    } else if (auto* unary = dynamic_cast<const UnaryExpr*>(expr)) {
        generate_expression(unary->operand.get());
        switch (unary->op) {
            case UnaryOp::NEG:
                emitter_.emit_neg_rax();
                break;
            case UnaryOp::POS:
                break;
            case UnaryOp::NOT:
                emitter_.emit_test_rax_rax();
                emitter_.emit_setz_al();
                emitter_.emit_movzx_rax_al();
                break;
        }
    } else if (auto* call = dynamic_cast<const CallExpr*>(expr)) {
        for (size_t i = call->args.size(); i > 0; i--) {
            generate_expression(call->args[i - 1].get());
            emitter_.emit_push_rax();
        }
        for (size_t i = 0; i < call->args.size(); i++) {
            switch (i) {
                case 0:
                    emitter_.emit_pop_rdi();
                    break;
                case 1:
                    emitter_.emit_pop_rsi();
                    break;
                case 2:
                    emitter_.emit_pop_rdx();
                    break;
                case 3:
                    emitter_.emit_pop_rcx();
                    break;
                case 4:
                    emitter_.emit_pop_r8();
                    break;
                case 5:
                    emitter_.emit_pop_r9();
                    break;
            }
        }

        emitter_.emit_call_rel32(0);

        CallPatch patch;
        patch.call_site = emitter_.current_offset() - 4;
        patch.target = call->callee;
        call_patches_.push_back(patch);
    } else if (auto* binary = dynamic_cast<const BinaryExpr*>(expr)) {
        if (binary->op == BinaryOp::AND) {
            generate_expression(binary->left.get());
            emitter_.emit_test_rax_rax();
            emitter_.emit_setne_al();
            emitter_.emit_movzx_rax_al();
            emitter_.emit_push_rax();
            generate_expression(binary->right.get());
            emitter_.emit_test_rax_rax();
            emitter_.emit_setne_al();
            emitter_.emit_movzx_rax_al();
            emitter_.emit_pop_rcx();
            emitter_.emit_test_rax_rax();
            emitter_.emit_setne_al();
            emitter_.emit_movzx_rax_al();
            emitter_.emit_imul_rax_rcx();
            return;
        }

        if (binary->op == BinaryOp::OR) {
            generate_expression(binary->left.get());
            emitter_.emit_push_rax();
            generate_expression(binary->right.get());
            emitter_.emit_pop_rcx();
            emitter_.emit_add_rax_rcx();
            emitter_.emit_test_rax_rax();
            emitter_.emit_setne_al();
            emitter_.emit_movzx_rax_al();
            return;
        }

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
            case BinaryOp::EQ:
                emitter_.emit_cmp_rax_rcx();
                emitter_.emit_sete_al();
                emitter_.emit_movzx_rax_al();
                break;
            case BinaryOp::NE:
                emitter_.emit_cmp_rax_rcx();
                emitter_.emit_setne_al();
                emitter_.emit_movzx_rax_al();
                break;
            case BinaryOp::LT:
                emitter_.emit_cmp_rax_rcx();
                emitter_.emit_setl_al();
                emitter_.emit_movzx_rax_al();
                break;
            case BinaryOp::GT:
                emitter_.emit_cmp_rax_rcx();
                emitter_.emit_setg_al();
                emitter_.emit_movzx_rax_al();
                break;
            case BinaryOp::LE:
                emitter_.emit_cmp_rax_rcx();
                emitter_.emit_setle_al();
                emitter_.emit_movzx_rax_al();
                break;
            case BinaryOp::GE:
                emitter_.emit_cmp_rax_rcx();
                emitter_.emit_setge_al();
                emitter_.emit_movzx_rax_al();
                break;
            case BinaryOp::AND:
            case BinaryOp::OR:
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
