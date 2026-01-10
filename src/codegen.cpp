#include "zex/codegen.hpp"

#include <cstring>

namespace zex {

CodeGenerator::CodeGenerator(SemanticAnalyzer& semantic)
    : semantic_(semantic), stack_size_(0), entry_offset_(0) {}

void CodeGenerator::generate(const Program& program) {
    entry_offset_ = emitter_.current_offset();

    emitter_.call(0);
    size_t main_call_site = emitter_.current_offset() - 4;

    emitter_.mov(Reg::RDI, Reg::RAX);
    emitter_.mov(Reg::RAX, static_cast<int32_t>(60));
    emitter_.syscall();

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

    for (const auto& param : func.params) {
        size += static_cast<int32_t>(param.type.size());
    }

    for (const auto& stmt : func.body) {
        if (auto* var_decl = dynamic_cast<VarDecl*>(stmt.get())) {
            size += static_cast<int32_t>(var_decl->type.size());
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

    emitter_.push(Reg::RBP);
    emitter_.mov(Reg::RBP, Reg::RSP);

    if (stack_size_ > 0) {
        emitter_.sub(Reg::RSP, static_cast<int32_t>(stack_size_));
    }

    int32_t offset = -8;
    Reg arg_regs[] = {Reg::RDI, Reg::RSI, Reg::RDX, Reg::RCX, Reg::R8, Reg::R9};

    for (size_t i = 0; i < func.params.size() && i < 6; i++) {
        const auto& param = func.params[i];
        LocalVar lv;
        lv.stack_offset = offset;
        lv.is_const = false;
        lv.const_value = 0;
        lv.type = param.type;

        emitter_.mov(Mem(Reg::RBP, offset), arg_regs[i]);

        locals_[param.name] = lv;
        offset -= static_cast<int32_t>(param.type.size());
    }

    for (const auto& stmt : func.body) {
        if (auto* var_decl = dynamic_cast<VarDecl*>(stmt.get())) {
            LocalVar lv;
            lv.stack_offset = offset;
            lv.is_const = false;
            lv.const_value = 0;
            lv.type = var_decl->type;
            locals_[var_decl->name] = lv;
            offset -= static_cast<int32_t>(var_decl->type.size());
        } else if (auto* const_decl = dynamic_cast<ConstDecl*>(stmt.get())) {
            LocalVar lv;
            lv.stack_offset = 0;
            lv.is_const = true;
            lv.const_value = const_decl->value;
            lv.type = const_decl->type;
            locals_[const_decl->name] = lv;
        }
    }

    for (const auto& stmt : func.body) {
        generate_statement(stmt.get());
    }

    if (func.return_type.kind == TypeKind::VOID) {
        if (stack_size_ > 0) {
            emitter_.add(Reg::RSP, static_cast<int32_t>(stack_size_));
        }
        emitter_.pop(Reg::RBP);
        emitter_.ret();
    }
}

void CodeGenerator::generate_statement(const Statement* stmt) {
    if (auto* var_decl = dynamic_cast<const VarDecl*>(stmt)) {
        generate_expression(var_decl->initializer.get());
        int32_t offset = locals_[var_decl->name].stack_offset;
        emitter_.mov(Mem(Reg::RBP, offset), Reg::RAX);
    } else if (auto* assign = dynamic_cast<const AssignStmt*>(stmt)) {
        generate_expression(assign->value.get());
        int32_t offset = locals_[assign->name].stack_offset;
        emitter_.mov(Mem(Reg::RBP, offset), Reg::RAX);
    } else if (auto* idx_assign = dynamic_cast<const IndexAssignStmt*>(stmt)) {
        generate_expression(idx_assign->value.get());
        emitter_.push(Reg::RAX);

        auto* idx_expr = dynamic_cast<IndexExpr*>(idx_assign->target.get());
        if (auto* ident = dynamic_cast<Identifier*>(idx_expr->array.get())) {
            const LocalVar& lv = locals_[ident->name];

            emitter_.mov(Reg::RAX, static_cast<int64_t>(lv.stack_offset));
            emitter_.push(Reg::RAX);

            generate_expression(idx_expr->index.get());

            if (lv.type.kind == TypeKind::ARRAY && lv.type.element_type) {
                size_t elem_size = lv.type.element_type->size();
                emitter_.mov(Reg::RCX, Reg::RAX);
                emitter_.mov(Reg::RAX, static_cast<int64_t>(elem_size));
                emitter_.imul(Reg::RAX, Reg::RCX);
            }

            emitter_.pop(Reg::RCX);
            emitter_.add(Reg::RAX, Reg::RCX);

            emitter_.mov(Reg::RCX, Reg::RAX);
            emitter_.pop(Reg::RAX);
        }
    } else if (auto* ret = dynamic_cast<const ReturnStmt*>(stmt)) {
        if (ret->value) {
            generate_expression(ret->value.get());
        }

        if (stack_size_ > 0) {
            emitter_.add(Reg::RSP, static_cast<int32_t>(stack_size_));
        }
        emitter_.pop(Reg::RBP);
        emitter_.ret();
    } else if (auto* if_stmt = dynamic_cast<const IfStmt*>(stmt)) {
        generate_expression(if_stmt->condition.get());
        emitter_.test(Reg::RAX, Reg::RAX);
        emitter_.jcc(Cond::Z, 0);
        size_t else_patch = emitter_.current_offset() - 4;

        for (const auto& s : if_stmt->then_body) {
            generate_statement(s.get());
        }

        if (!if_stmt->else_body.empty()) {
            emitter_.jmp(0);
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

size_t CodeGenerator::add_string_literal(const std::string& str) {
    for (const auto& sl : string_literals_) {
        if (sl.value == str) {
            return sl.offset;
        }
    }

    size_t offset = emitter_.current_offset();
    for (char c : str) {
        emitter_.code().push_back(static_cast<uint8_t>(c));
    }
    emitter_.code().push_back(0);

    string_literals_.push_back({str, offset});
    return offset;
}

void CodeGenerator::generate_expression(const Expression* expr) {
    if (auto* lit = dynamic_cast<const IntLiteral*>(expr)) {
        emitter_.mov(Reg::RAX, lit->value);
    } else if (auto* fl = dynamic_cast<const FloatLiteral*>(expr)) {
        uint32_t bits;
        std::memcpy(&bits, &fl->value, sizeof(float));
        emitter_.mov(Reg::RAX, static_cast<int64_t>(bits));
        emitter_.movd(Reg::XMM0, Reg::RAX);
    } else if (auto* cl = dynamic_cast<const CharLiteral*>(expr)) {
        emitter_.mov(Reg::RAX, static_cast<int64_t>(cl->value));
    } else if (auto* sl = dynamic_cast<const StringLiteral*>(expr)) {
        size_t str_offset = add_string_literal(sl->value);
        emitter_.mov(Reg::RAX, static_cast<int64_t>(str_offset));
    } else if (auto* bl = dynamic_cast<const BoolLiteral*>(expr)) {
        emitter_.mov(Reg::RAX, bl->value ? static_cast<int64_t>(1) : static_cast<int64_t>(0));
    } else if (auto* sz = dynamic_cast<const SizeofExpr*>(expr)) {
        emitter_.mov(Reg::RAX, static_cast<int64_t>(sz->target_type.size()));
    } else if (auto* arr_lit = dynamic_cast<const ArrayLiteral*>(expr)) {
        for (size_t i = 0; i < arr_lit->elements.size(); i++) {
            generate_expression(arr_lit->elements[i].get());
            emitter_.push(Reg::RAX);
        }
    } else if (auto* idx = dynamic_cast<const IndexExpr*>(expr)) {
        if (auto* ident = dynamic_cast<Identifier*>(idx->array.get())) {
            const LocalVar& lv = locals_[ident->name];

            generate_expression(idx->index.get());

            if (lv.type.kind == TypeKind::ARRAY && lv.type.element_type) {
                size_t elem_size = lv.type.element_type->size();
                emitter_.mov(Reg::RCX, Reg::RAX);
                emitter_.mov(Reg::RAX, static_cast<int64_t>(elem_size));
                emitter_.imul(Reg::RAX, Reg::RCX);
            }

            emitter_.mov(Reg::RCX, Reg::RAX);
            emitter_.mov(Reg::RAX, static_cast<int64_t>(lv.stack_offset));
            emitter_.add(Reg::RAX, Reg::RCX);

            emitter_.mov(Reg::RCX, Reg::RAX);
            emitter_.mov(Reg::RAX, Mem(Reg::RBP, 0));
            emitter_.add(Reg::RAX, Reg::RCX);
            emitter_.mov(Reg::RAX, Mem(Reg::RBP, 0));
        }
    } else if (auto* ident = dynamic_cast<const Identifier*>(expr)) {
        const LocalVar& lv = locals_[ident->name];
        if (lv.is_const) {
            emitter_.mov(Reg::RAX, lv.const_value);
        } else {
            emitter_.mov(Reg::RAX, Mem(Reg::RBP, lv.stack_offset));
        }
    } else if (auto* unary = dynamic_cast<const UnaryExpr*>(expr)) {
        generate_expression(unary->operand.get());
        switch (unary->op) {
            case UnaryOp::NEG:
                emitter_.neg(Reg::RAX);
                break;
            case UnaryOp::POS:
                break;
            case UnaryOp::NOT:
                emitter_.test(Reg::RAX, Reg::RAX);
                emitter_.setcc(Cond::Z, Reg::AL);
                emitter_.movzx(Reg::RAX, Reg::AL);
                break;
            case UnaryOp::ADDR:
                if (auto* id = dynamic_cast<Identifier*>(unary->operand.get())) {
                    const LocalVar& lv = locals_[id->name];
                    emitter_.lea(Reg::RAX, Mem(Reg::RBP, lv.stack_offset));
                }
                break;
            case UnaryOp::DEREF:
                emitter_.mov(Reg::RAX, Mem(Reg::RAX, 0));
                break;
        }
    } else if (auto* call = dynamic_cast<const CallExpr*>(expr)) {
        Reg arg_regs[] = {Reg::RDI, Reg::RSI, Reg::RDX, Reg::RCX, Reg::R8, Reg::R9};

        for (size_t i = call->args.size(); i > 0; i--) {
            generate_expression(call->args[i - 1].get());
            emitter_.push(Reg::RAX);
        }
        for (size_t i = 0; i < call->args.size() && i < 6; i++) {
            emitter_.pop(arg_regs[i]);
        }

        emitter_.call(0);

        CallPatch patch;
        patch.call_site = emitter_.current_offset() - 4;
        patch.target = call->callee;
        call_patches_.push_back(patch);
    } else if (auto* binary = dynamic_cast<const BinaryExpr*>(expr)) {
        if (binary->op == BinaryOp::AND) {
            generate_expression(binary->left.get());
            emitter_.test(Reg::RAX, Reg::RAX);
            emitter_.setcc(Cond::NE, Reg::AL);
            emitter_.movzx(Reg::RAX, Reg::AL);
            emitter_.push(Reg::RAX);
            generate_expression(binary->right.get());
            emitter_.test(Reg::RAX, Reg::RAX);
            emitter_.setcc(Cond::NE, Reg::AL);
            emitter_.movzx(Reg::RAX, Reg::AL);
            emitter_.pop(Reg::RCX);
            emitter_.test(Reg::RAX, Reg::RAX);
            emitter_.setcc(Cond::NE, Reg::AL);
            emitter_.movzx(Reg::RAX, Reg::AL);
            emitter_.imul(Reg::RAX, Reg::RCX);
            return;
        }

        if (binary->op == BinaryOp::OR) {
            generate_expression(binary->left.get());
            emitter_.push(Reg::RAX);
            generate_expression(binary->right.get());
            emitter_.pop(Reg::RCX);
            emitter_.add(Reg::RAX, Reg::RCX);
            emitter_.test(Reg::RAX, Reg::RAX);
            emitter_.setcc(Cond::NE, Reg::AL);
            emitter_.movzx(Reg::RAX, Reg::AL);
            return;
        }

        generate_expression(binary->right.get());
        emitter_.push(Reg::RAX);
        generate_expression(binary->left.get());
        emitter_.pop(Reg::RCX);

        switch (binary->op) {
            case BinaryOp::ADD:
                emitter_.add(Reg::RAX, Reg::RCX);
                break;
            case BinaryOp::SUB:
                emitter_.sub(Reg::RAX, Reg::RCX);
                break;
            case BinaryOp::MUL:
                emitter_.imul(Reg::RAX, Reg::RCX);
                break;
            case BinaryOp::DIV:
                emitter_.cqo();
                emitter_.idiv(Reg::RCX);
                break;
            case BinaryOp::MOD:
                emitter_.cqo();
                emitter_.idiv(Reg::RCX);
                emitter_.mov(Reg::RAX, Reg::RDX);
                break;
            case BinaryOp::EQ:
                emitter_.cmp(Reg::RAX, Reg::RCX);
                emitter_.setcc(Cond::E, Reg::AL);
                emitter_.movzx(Reg::RAX, Reg::AL);
                break;
            case BinaryOp::NE:
                emitter_.cmp(Reg::RAX, Reg::RCX);
                emitter_.setcc(Cond::NE, Reg::AL);
                emitter_.movzx(Reg::RAX, Reg::AL);
                break;
            case BinaryOp::LT:
                emitter_.cmp(Reg::RAX, Reg::RCX);
                emitter_.setcc(Cond::L, Reg::AL);
                emitter_.movzx(Reg::RAX, Reg::AL);
                break;
            case BinaryOp::GT:
                emitter_.cmp(Reg::RAX, Reg::RCX);
                emitter_.setcc(Cond::G, Reg::AL);
                emitter_.movzx(Reg::RAX, Reg::AL);
                break;
            case BinaryOp::LE:
                emitter_.cmp(Reg::RAX, Reg::RCX);
                emitter_.setcc(Cond::LE, Reg::AL);
                emitter_.movzx(Reg::RAX, Reg::AL);
                break;
            case BinaryOp::GE:
                emitter_.cmp(Reg::RAX, Reg::RCX);
                emitter_.setcc(Cond::GE, Reg::AL);
                emitter_.movzx(Reg::RAX, Reg::AL);
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
