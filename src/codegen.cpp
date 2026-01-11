#include "zex/codegen.hpp"

#include <cstring>

namespace zex {

CodeGenerator::CodeGenerator(SemanticAnalyzer& semantic)
    : semantic_(semantic), stack_size_(0), entry_offset_(0) {}

void CodeGenerator::generate(const Program& program) {
    entry_offset_ = emitter_.current_offset();

    emitter_.call(0);
    size_t main_call_site = emitter_.current_offset() - 4;

    emitter_.mov(Reg::rdi(), Reg::rax());
    emitter_.mov(Reg::rax(), static_cast<int32_t>(60));
    emitter_.syscall();

    for (const auto& func : program.functions) {
        generate_function(*func);
    }

    resolve_calls();
    emit_string_literals();

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
        size += 8;    // use 8 bytes per param for alignment
        (void)param;  // suppress unused warning
    }

    for (const auto& stmt : func.body) {
        if (auto* var_decl = dynamic_cast<VarDecl*>(stmt.get())) {
            if (var_decl->type.kind == TypeKind::ARRAY) {
                size += static_cast<int32_t>(var_decl->type.size());
            } else {
                size += 8;  // 8 bytes per scalar for alignment
            }
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

    emitter_.push(Reg::rbp());
    emitter_.mov(Reg::rbp(), Reg::rsp());

    if (stack_size_ > 0) {
        emitter_.sub(Reg::rsp(), static_cast<int32_t>(stack_size_));
    }

    int32_t offset = -8;
    Reg gpr_regs[] = {Reg::rdi(), Reg::rsi(), Reg::rdx(), Reg::rcx(), Reg::r8(), Reg::r9()};
    XMM xmm_regs[] = {XMM::XMM0, XMM::XMM1, XMM::XMM2, XMM::XMM3,
                      XMM::XMM4, XMM::XMM5, XMM::XMM6, XMM::XMM7};
    size_t gpr_idx = 0;
    size_t xmm_idx = 0;

    for (size_t i = 0; i < func.params.size(); i++) {
        const auto& param = func.params[i];
        LocalVar lv;
        lv.stack_offset = offset;
        lv.is_const = false;
        lv.const_value = 0;
        lv.type = param.type;

        if (param.type.kind == TypeKind::F32) {
            // Float params passed in XMM0-XMM7
            if (xmm_idx < 8) {
                emitter_.movss(Mem(Reg::rbp(), offset), xmm_regs[xmm_idx++]);
            }
        } else {
            // Integer/pointer params passed in RDI/RSI/RDX/RCX/R8/R9
            if (gpr_idx < 6) {
                emitter_.mov(Mem(Reg::rbp(), offset), gpr_regs[gpr_idx++]);
            } else {
                int32_t stack_param_offset = 16 + static_cast<int32_t>((gpr_idx - 6) * 8);
                emitter_.mov(Reg::rax(), Mem(Reg::rbp(), stack_param_offset));
                emitter_.mov(Mem(Reg::rbp(), offset), Reg::rax());
                gpr_idx++;
            }
        }

        locals_[param.name] = lv;
        offset -= 8;
    }

    for (const auto& stmt : func.body) {
        if (auto* var_decl = dynamic_cast<VarDecl*>(stmt.get())) {
            LocalVar lv;
            lv.is_const = false;
            lv.const_value = 0;
            lv.type = var_decl->type;

            // For arrays: decrement first so arr[0] is at lowest address
            // Then adding i*elem_size gives correct higher addresses
            int32_t var_size = static_cast<int32_t>(var_decl->type.size());
            offset -= var_size;
            lv.stack_offset = offset;

            locals_[var_decl->name] = lv;
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
            emitter_.add(Reg::rsp(), static_cast<int32_t>(stack_size_));
        }
        emitter_.pop(Reg::rbp());
        emitter_.ret();
    }
}

void CodeGenerator::generate_statement(const Statement* stmt) {
    if (auto* var_decl = dynamic_cast<const VarDecl*>(stmt)) {
        int32_t offset = locals_[var_decl->name].stack_offset;

        if (auto* arr_lit = dynamic_cast<ArrayLiteral*>(var_decl->initializer.get())) {
            size_t elem_size = 8;
            if (var_decl->type.kind == TypeKind::ARRAY && var_decl->type.element_type) {
                elem_size = var_decl->type.element_type->size();
            }
            for (size_t i = 0; i < arr_lit->elements.size(); i++) {
                generate_expression(arr_lit->elements[i].get());
                int32_t elem_offset = offset + static_cast<int32_t>(i * elem_size);
                emitter_.mov(Mem(Reg::rbp(), elem_offset), Reg::rax());
            }
        } else {
            generate_expression(var_decl->initializer.get());
            if (var_decl->type.kind == TypeKind::F32) {
                emitter_.movss(Mem(Reg::rbp(), offset), XMM::XMM0);
            } else {
                emitter_.mov(Mem(Reg::rbp(), offset), Reg::rax());
            }
        }
    } else if (auto* assign = dynamic_cast<const AssignStmt*>(stmt)) {
        generate_expression(assign->value.get());
        int32_t offset = locals_[assign->name].stack_offset;
        const LocalVar& lv = locals_[assign->name];
        if (lv.type.kind == TypeKind::F32) {
            emitter_.movss(Mem(Reg::rbp(), offset), XMM::XMM0);
        } else {
            emitter_.mov(Mem(Reg::rbp(), offset), Reg::rax());
        }
    } else if (auto* idx_assign = dynamic_cast<const IndexAssignStmt*>(stmt)) {
        if (auto* idx_expr = dynamic_cast<IndexExpr*>(idx_assign->target.get())) {
            generate_expression(idx_assign->value.get());
            emitter_.push(Reg::rax());  // save value

            if (auto* ident = dynamic_cast<Identifier*>(idx_expr->array.get())) {
                const LocalVar& lv = locals_[ident->name];

                generate_expression(idx_expr->index.get());

                size_t elem_size = 8;
                if (lv.type.kind == TypeKind::ARRAY && lv.type.element_type) {
                    elem_size = lv.type.element_type->size();
                }

                emitter_.mov(Reg::rcx(), Reg::rax());
                emitter_.mov(Reg::rax(), static_cast<int64_t>(elem_size));
                emitter_.imul(Reg::rax(), Reg::rcx());  // RAX = index * elem_size

                emitter_.lea(Reg::rcx(), Mem(Reg::rbp(), lv.stack_offset));  // base address
                emitter_.add(Reg::rcx(), Reg::rax());  // RCX = element address

                emitter_.pop(Reg::rax());                      // restore value
                emitter_.mov(Mem(Reg::rcx(), 0), Reg::rax());  // store value
            }
        }
    } else if (auto* ret = dynamic_cast<const ReturnStmt*>(stmt)) {
        if (ret->value) {
            generate_expression(ret->value.get());
        }

        if (stack_size_ > 0) {
            emitter_.add(Reg::rsp(), static_cast<int32_t>(stack_size_));
        }
        emitter_.pop(Reg::rbp());
        emitter_.ret();
    } else if (auto* if_stmt = dynamic_cast<const IfStmt*>(stmt)) {
        generate_expression(if_stmt->condition.get());
        emitter_.test(Reg::rax(), Reg::rax());
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
    } else if (auto* asm_block = dynamic_cast<const AsmBlock*>(stmt)) {
        generate_asm_block(asm_block);
    } else if (auto* expr_stmt = dynamic_cast<const ExprStmt*>(stmt)) {
        generate_expression(expr_stmt->expr.get());
    }
}

size_t CodeGenerator::add_string_literal(const std::string& str) {
    // Check if string already exists
    for (size_t i = 0; i < string_literals_.size(); i++) {
        if (string_literals_[i].value == str) {
            return i;
        }
    }

    // Add new string with placeholder offset
    size_t index = string_literals_.size();
    string_literals_.push_back({str, 0});
    return index;
}

void CodeGenerator::emit_string_literals() {
    // Append all strings at end of code section
    for (auto& sl : string_literals_) {
        sl.offset = emitter_.current_offset();
        for (char c : sl.value) {
            emitter_.code().push_back(static_cast<uint8_t>(c));
        }
        emitter_.code().push_back(0);
    }

    // Patch all string address references
    for (const auto& patch : string_patches_) {
        int64_t addr =
            0x400000 + 120 + static_cast<int64_t>(string_literals_[patch.string_index].offset);
        for (int i = 0; i < 8; i++) {
            emitter_.code()[patch.patch_location + i] =
                static_cast<uint8_t>((addr >> (i * 8)) & 0xFF);
        }
    }
}

void CodeGenerator::generate_expression(const Expression* expr) {
    if (auto* lit = dynamic_cast<const IntLiteral*>(expr)) {
        emitter_.mov(Reg::rax(), lit->value);
    } else if (auto* fl = dynamic_cast<const FloatLiteral*>(expr)) {
        uint32_t bits;
        std::memcpy(&bits, &fl->value, sizeof(float));
        emitter_.mov(Reg::rax(), static_cast<int64_t>(bits));
        emitter_.movd(XMM::XMM0, Reg::eax());
    } else if (auto* cl = dynamic_cast<const CharLiteral*>(expr)) {
        emitter_.mov(Reg::rax(), static_cast<int64_t>(cl->value));
    } else if (auto* sl = dynamic_cast<const StringLiteral*>(expr)) {
        size_t str_index = add_string_literal(sl->value);
        // Emit mov with placeholder, record patch location
        size_t patch_loc = emitter_.current_offset() + 2;
        emitter_.mov(Reg::rax(), static_cast<int64_t>(0));
        string_patches_.push_back({patch_loc, str_index});
    } else if (auto* bl = dynamic_cast<const BoolLiteral*>(expr)) {
        emitter_.mov(Reg::rax(), bl->value ? static_cast<int64_t>(1) : static_cast<int64_t>(0));
    } else if (auto* sz = dynamic_cast<const SizeofExpr*>(expr)) {
        emitter_.mov(Reg::rax(), static_cast<int64_t>(sz->target_type.size()));
    } else if (auto* arr_lit = dynamic_cast<const ArrayLiteral*>(expr)) {
        for (size_t i = 0; i < arr_lit->elements.size(); i++) {
            generate_expression(arr_lit->elements[i].get());
            emitter_.push(Reg::rax());
        }
    } else if (auto* idx = dynamic_cast<const IndexExpr*>(expr)) {
        if (auto* ident = dynamic_cast<Identifier*>(idx->array.get())) {
            const LocalVar& lv = locals_[ident->name];

            generate_expression(idx->index.get());

            size_t elem_size = 8;
            if (lv.type.kind == TypeKind::ARRAY && lv.type.element_type) {
                elem_size = lv.type.element_type->size();
            }

            emitter_.mov(Reg::rcx(), Reg::rax());
            emitter_.mov(Reg::rax(), static_cast<int64_t>(elem_size));
            emitter_.imul(Reg::rax(), Reg::rcx());  // RAX = index * elem_size

            emitter_.lea(Reg::rcx(), Mem(Reg::rbp(), lv.stack_offset));  // base address
            emitter_.add(Reg::rax(), Reg::rcx());                        // RAX = element address
            emitter_.mov(Reg::rax(), Mem(Reg::rax(), 0));                // load element
        }
    } else if (auto* ident = dynamic_cast<const Identifier*>(expr)) {
        const LocalVar& lv = locals_[ident->name];
        if (lv.is_const) {
            emitter_.mov(Reg::rax(), lv.const_value);
        } else if (lv.type.kind == TypeKind::F32) {
            // Load float into XMM0
            emitter_.movss(XMM::XMM0, Mem(Reg::rbp(), lv.stack_offset));
        } else {
            emitter_.mov(Reg::rax(), Mem(Reg::rbp(), lv.stack_offset));
        }
    } else if (auto* unary = dynamic_cast<const UnaryExpr*>(expr)) {
        generate_expression(unary->operand.get());
        switch (unary->op) {
            case UnaryOp::NEG:
                emitter_.neg(Reg::rax());
                break;
            case UnaryOp::POS:
                break;
            case UnaryOp::NOT:
                emitter_.test(Reg::rax(), Reg::rax());
                emitter_.setcc(Cond::Z, Reg::al());
                emitter_.movzx(Reg::rax(), Reg::al());
                break;
            case UnaryOp::ADDR:
                if (auto* id = dynamic_cast<Identifier*>(unary->operand.get())) {
                    const LocalVar& lv = locals_[id->name];
                    emitter_.lea(Reg::rax(), Mem(Reg::rbp(), lv.stack_offset));
                }
                break;
            case UnaryOp::DEREF:
                emitter_.mov(Reg::rax(), Mem(Reg::rax(), 0));
                break;
        }
    } else if (auto* call = dynamic_cast<const CallExpr*>(expr)) {
        Reg gpr_regs[] = {Reg::rdi(), Reg::rsi(), Reg::rdx(), Reg::rcx(), Reg::r8(), Reg::r9()};
        XMM xmm_regs[] = {XMM::XMM0, XMM::XMM1, XMM::XMM2, XMM::XMM3,
                          XMM::XMM4, XMM::XMM5, XMM::XMM6, XMM::XMM7};
        size_t num_args = call->args.size();

        // Count GPR args for stack alignment
        size_t gpr_count = 0;
        for (size_t i = 0; i < num_args; i++) {
            Type arg_type = get_expression_type(call->args[i].get());
            if (arg_type.kind != TypeKind::F32) {
                gpr_count++;
            }
        }
        size_t stack_gpr_args = gpr_count > 6 ? gpr_count - 6 : 0;
        bool need_align = (stack_gpr_args % 2) == 1;
        if (need_align) {
            emitter_.sub(Reg::rsp(), 8);
        }

        // Determine which args are floats
        std::vector<bool> is_float_arg(num_args);
        size_t gpr_idx = 0;
        size_t xmm_idx = 0;

        for (size_t i = 0; i < num_args; i++) {
            Type arg_type = get_expression_type(call->args[i].get());
            is_float_arg[i] = (arg_type.kind == TypeKind::F32);
        }

        // Push all args to stack in reverse order then pop to registers
        for (size_t i = num_args; i > 0; i--) {
            generate_expression(call->args[i - 1].get());
            if (is_float_arg[i - 1]) {
                emitter_.movd(Reg::eax(), XMM::XMM0);
            }
            emitter_.push(Reg::rax());
        }

        // Now pop values into registers
        gpr_idx = 0;
        xmm_idx = 0;
        for (size_t i = 0; i < num_args; i++) {
            emitter_.pop(Reg::rax());
            if (is_float_arg[i]) {
                if (xmm_idx < 8) {
                    emitter_.movd(xmm_regs[xmm_idx++], Reg::eax());
                }
            } else {
                if (gpr_idx < 6) {
                    emitter_.mov(gpr_regs[gpr_idx++], Reg::rax());
                } else {
                    emitter_.push(Reg::rax());
                }
            }
        }

        emitter_.call(0);

        CallPatch patch;
        patch.call_site = emitter_.current_offset() - 4;
        patch.target = call->callee;
        call_patches_.push_back(patch);

        // Clean up stack args
        if (stack_gpr_args > 0 || need_align) {
            int32_t cleanup = static_cast<int32_t>(stack_gpr_args * 8);
            if (need_align)
                cleanup += 8;
            emitter_.add(Reg::rsp(), cleanup);
        }
    } else if (auto* binary = dynamic_cast<const BinaryExpr*>(expr)) {
        if (binary->op == BinaryOp::AND) {
            generate_expression(binary->left.get());
            emitter_.test(Reg::rax(), Reg::rax());
            emitter_.setcc(Cond::NE, Reg::al());
            emitter_.movzx(Reg::rax(), Reg::al());
            emitter_.push(Reg::rax());
            generate_expression(binary->right.get());
            emitter_.test(Reg::rax(), Reg::rax());
            emitter_.setcc(Cond::NE, Reg::al());
            emitter_.movzx(Reg::rax(), Reg::al());
            emitter_.pop(Reg::rcx());
            emitter_.test(Reg::rax(), Reg::rax());
            emitter_.setcc(Cond::NE, Reg::al());
            emitter_.movzx(Reg::rax(), Reg::al());
            emitter_.imul(Reg::rax(), Reg::rcx());
            return;
        }

        if (binary->op == BinaryOp::OR) {
            generate_expression(binary->left.get());
            emitter_.push(Reg::rax());
            generate_expression(binary->right.get());
            emitter_.pop(Reg::rcx());
            emitter_.add(Reg::rax(), Reg::rcx());
            emitter_.test(Reg::rax(), Reg::rax());
            emitter_.setcc(Cond::NE, Reg::al());
            emitter_.movzx(Reg::rax(), Reg::al());
            return;
        }

        Type left_type = get_expression_type(binary->left.get());
        Type right_type = get_expression_type(binary->right.get());
        bool is_float = (left_type.kind == TypeKind::F32 || right_type.kind == TypeKind::F32);

        generate_expression(binary->right.get());
        if (is_float) {
            emitter_.movd(Reg::eax(), XMM::XMM0);
        }
        emitter_.push(Reg::rax());
        generate_expression(binary->left.get());
        emitter_.pop(Reg::rcx());

        if (is_float) {
            emitter_.movd(XMM::XMM1, Reg::ecx());

            switch (binary->op) {
                case BinaryOp::ADD:
                    emitter_.addss(XMM::XMM0, XMM::XMM1);
                    break;
                case BinaryOp::SUB:
                    emitter_.subss(XMM::XMM0, XMM::XMM1);
                    break;
                case BinaryOp::MUL:
                    emitter_.mulss(XMM::XMM0, XMM::XMM1);
                    break;
                case BinaryOp::DIV:
                    emitter_.divss(XMM::XMM0, XMM::XMM1);
                    break;
                case BinaryOp::LT:
                    emitter_.ucomiss(XMM::XMM0, XMM::XMM1);
                    emitter_.setcc(Cond::B, Reg::al());
                    emitter_.movzx(Reg::rax(), Reg::al());
                    break;
                case BinaryOp::GT:
                    emitter_.ucomiss(XMM::XMM0, XMM::XMM1);
                    emitter_.setcc(Cond::A, Reg::al());
                    emitter_.movzx(Reg::rax(), Reg::al());
                    break;
                case BinaryOp::LE:
                    emitter_.ucomiss(XMM::XMM0, XMM::XMM1);
                    emitter_.setcc(Cond::BE, Reg::al());
                    emitter_.movzx(Reg::rax(), Reg::al());
                    break;
                case BinaryOp::GE:
                    emitter_.ucomiss(XMM::XMM0, XMM::XMM1);
                    emitter_.setcc(Cond::AE, Reg::al());
                    emitter_.movzx(Reg::rax(), Reg::al());
                    break;
                case BinaryOp::EQ:
                    emitter_.ucomiss(XMM::XMM0, XMM::XMM1);
                    emitter_.setcc(Cond::E, Reg::al());
                    emitter_.movzx(Reg::rax(), Reg::al());
                    break;
                case BinaryOp::NE:
                    emitter_.ucomiss(XMM::XMM0, XMM::XMM1);
                    emitter_.setcc(Cond::NE, Reg::al());
                    emitter_.movzx(Reg::rax(), Reg::al());
                    break;
                default:
                    break;
            }
            return;
        }

        switch (binary->op) {
            case BinaryOp::ADD:
                emitter_.add(Reg::rax(), Reg::rcx());
                break;
            case BinaryOp::SUB:
                emitter_.sub(Reg::rax(), Reg::rcx());
                break;
            case BinaryOp::MUL:
                emitter_.imul(Reg::rax(), Reg::rcx());
                break;
            case BinaryOp::DIV:
                emitter_.cqo();
                emitter_.idiv(Reg::rcx());
                break;
            case BinaryOp::MOD:
                emitter_.cqo();
                emitter_.idiv(Reg::rcx());
                emitter_.mov(Reg::rax(), Reg::rdx());
                break;
            case BinaryOp::EQ:
                emitter_.cmp(Reg::rax(), Reg::rcx());
                emitter_.setcc(Cond::E, Reg::al());
                emitter_.movzx(Reg::rax(), Reg::al());
                break;
            case BinaryOp::NE:
                emitter_.cmp(Reg::rax(), Reg::rcx());
                emitter_.setcc(Cond::NE, Reg::al());
                emitter_.movzx(Reg::rax(), Reg::al());
                break;
            case BinaryOp::LT:
                emitter_.cmp(Reg::rax(), Reg::rcx());
                emitter_.setcc(Cond::L, Reg::al());
                emitter_.movzx(Reg::rax(), Reg::al());
                break;
            case BinaryOp::GT:
                emitter_.cmp(Reg::rax(), Reg::rcx());
                emitter_.setcc(Cond::G, Reg::al());
                emitter_.movzx(Reg::rax(), Reg::al());
                break;
            case BinaryOp::LE:
                emitter_.cmp(Reg::rax(), Reg::rcx());
                emitter_.setcc(Cond::LE, Reg::al());
                emitter_.movzx(Reg::rax(), Reg::al());
                break;
            case BinaryOp::GE:
                emitter_.cmp(Reg::rax(), Reg::rcx());
                emitter_.setcc(Cond::GE, Reg::al());
                emitter_.movzx(Reg::rax(), Reg::al());
                break;
            case BinaryOp::AND:
            case BinaryOp::OR:
                break;
        }
    } else if (auto* cast = dynamic_cast<const CastExpr*>(expr)) {
        generate_expression(cast->expr.get());

        Type from_type = get_expression_type(cast->expr.get());
        const Type& to_type = cast->target_type;

        if (from_type.kind == TypeKind::F32 && to_type.kind != TypeKind::F32) {
            emitter_.cvttss2si(Reg::rax(), XMM::XMM0);
        } else if (from_type.kind != TypeKind::F32 && to_type.kind == TypeKind::F32) {
            emitter_.cvtsi2ss(XMM::XMM0, Reg::rax());
        }
    }
}

Type CodeGenerator::get_expression_type(const Expression* expr) {
    if (dynamic_cast<const IntLiteral*>(expr)) {
        return Type(TypeKind::I64);
    }
    if (dynamic_cast<const FloatLiteral*>(expr)) {
        return Type(TypeKind::F32);
    }
    if (dynamic_cast<const CharLiteral*>(expr)) {
        return Type(TypeKind::CHAR);
    }
    if (dynamic_cast<const BoolLiteral*>(expr)) {
        return Type(TypeKind::BOOL);
    }
    if (dynamic_cast<const StringLiteral*>(expr)) {
        Type ptr(TypeKind::POINTER);
        ptr.element_type = std::make_unique<Type>(TypeKind::CHAR);
        return ptr;
    }
    if (auto* ident = dynamic_cast<const Identifier*>(expr)) {
        auto it = locals_.find(ident->name);
        if (it != locals_.end()) {
            return it->second.type;
        }
        return Type(TypeKind::I64);
    }
    if (auto* binary = dynamic_cast<const BinaryExpr*>(expr)) {
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
    if (auto* unary = dynamic_cast<const UnaryExpr*>(expr)) {
        if (unary->op == UnaryOp::NOT) {
            return Type(TypeKind::BOOL);
        }
        return get_expression_type(unary->operand.get());
    }
    if (auto* cast = dynamic_cast<const CastExpr*>(expr)) {
        return cast->target_type;
    }
    if (auto* idx = dynamic_cast<const IndexExpr*>(expr)) {
        Type arr = get_expression_type(idx->array.get());
        if ((arr.kind == TypeKind::ARRAY || arr.kind == TypeKind::POINTER) && arr.element_type) {
            return *arr.element_type;
        }
    }
    return Type(TypeKind::I64);
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

Reg CodeGenerator::asm_reg_to_reg(AsmReg ar) {
    switch (ar) {
        case AsmReg::RAX:
            return Reg::rax();
        case AsmReg::RCX:
            return Reg::rcx();
        case AsmReg::RDX:
            return Reg::rdx();
        case AsmReg::RBX:
            return Reg::rbx();
        case AsmReg::RSP:
            return Reg::rsp();
        case AsmReg::RBP:
            return Reg::rbp();
        case AsmReg::RSI:
            return Reg::rsi();
        case AsmReg::RDI:
            return Reg::rdi();
        case AsmReg::R8:
            return Reg::r8();
        case AsmReg::R9:
            return Reg::r9();
        case AsmReg::R10:
            return Reg::r10();
        case AsmReg::R11:
            return Reg::r11();
        case AsmReg::R12:
            return Reg::r12();
        case AsmReg::R13:
            return Reg::r13();
        case AsmReg::R14:
            return Reg::r14();
        case AsmReg::R15:
            return Reg::r15();
        case AsmReg::EAX:
            return Reg::eax();
        case AsmReg::ECX:
            return Reg::ecx();
        case AsmReg::EDX:
            return Reg::edx();
        case AsmReg::EBX:
            return Reg::ebx();
        case AsmReg::ESP:
            return Reg::esp();
        case AsmReg::EBP:
            return Reg::ebp();
        case AsmReg::ESI:
            return Reg::esi();
        case AsmReg::EDI:
            return Reg::edi();
        case AsmReg::R8D:
            return Reg::r8d();
        case AsmReg::R9D:
            return Reg::r9d();
        case AsmReg::R10D:
            return Reg::r10d();
        case AsmReg::R11D:
            return Reg::r11d();
        case AsmReg::R12D:
            return Reg::r12d();
        case AsmReg::R13D:
            return Reg::r13d();
        case AsmReg::R14D:
            return Reg::r14d();
        case AsmReg::R15D:
            return Reg::r15d();
        case AsmReg::AX:
            return Reg::ax();
        case AsmReg::CX:
            return Reg::cx();
        case AsmReg::DX:
            return Reg::dx();
        case AsmReg::BX:
            return Reg::bx();
        case AsmReg::SP:
            return Reg::sp();
        case AsmReg::BP:
            return Reg::bp();
        case AsmReg::SI:
            return Reg::si();
        case AsmReg::DI:
            return Reg::di();
        case AsmReg::AL:
            return Reg::al();
        case AsmReg::CL:
            return Reg::cl();
        case AsmReg::DL:
            return Reg::dl();
        case AsmReg::BL:
            return Reg::bl();
        case AsmReg::AH:
            return Reg::ah();
        case AsmReg::CH:
            return Reg::ch();
        case AsmReg::DH:
            return Reg::dh();
        case AsmReg::BH:
            return Reg::bh();
    }
    return Reg::rax();
}

void CodeGenerator::generate_asm_block(const AsmBlock* block) {
    // Track label byte offsets and jumps that need patching
    std::unordered_map<std::string, size_t> label_offsets;
    std::vector<std::tuple<size_t, std::string, size_t>> jump_patches;

    // Emit all instructions, record labels and jumps
    for (size_t i = 0; i < block->instructions.size(); i++) {
        // Record label offset if this instruction has one
        for (const auto& [label_name, instr_idx] : block->labels) {
            if (instr_idx == i) {
                label_offsets[label_name] = emitter_.current_offset();
            }
        }

        const auto& instr = block->instructions[i];
        const auto& ops = instr.operands;

        switch (instr.opcode) {
            case AsmOpcode::MOV:
                if (ops.size() == 2) {
                    if (ops[0].kind == AsmOperandKind::REG && ops[1].kind == AsmOperandKind::REG) {
                        emitter_.mov(asm_reg_to_reg(ops[0].reg), asm_reg_to_reg(ops[1].reg));
                    } else if (ops[0].kind == AsmOperandKind::REG &&
                               ops[1].kind == AsmOperandKind::IMM) {
                        emitter_.mov(asm_reg_to_reg(ops[0].reg), ops[1].imm);
                    } else if (ops[0].kind == AsmOperandKind::REG &&
                               ops[1].kind == AsmOperandKind::VAR) {
                        int32_t offset = locals_[ops[1].var_name].stack_offset;
                        emitter_.mov(asm_reg_to_reg(ops[0].reg), Mem(Reg::rbp(), offset));
                    } else if (ops[0].kind == AsmOperandKind::VAR &&
                               ops[1].kind == AsmOperandKind::REG) {
                        int32_t offset = locals_[ops[0].var_name].stack_offset;
                        emitter_.mov(Mem(Reg::rbp(), offset), asm_reg_to_reg(ops[1].reg));
                    } else if (ops[0].kind == AsmOperandKind::REG &&
                               ops[1].kind == AsmOperandKind::MEM) {
                        emitter_.mov(asm_reg_to_reg(ops[0].reg),
                                     Mem(asm_reg_to_reg(ops[1].mem_base), ops[1].mem_offset));
                    } else if (ops[0].kind == AsmOperandKind::MEM &&
                               ops[1].kind == AsmOperandKind::REG) {
                        emitter_.mov(Mem(asm_reg_to_reg(ops[0].mem_base), ops[0].mem_offset),
                                     asm_reg_to_reg(ops[1].reg));
                    }
                }
                break;
            case AsmOpcode::ADD:
                if (ops.size() == 2) {
                    if (ops[0].kind == AsmOperandKind::REG && ops[1].kind == AsmOperandKind::REG) {
                        emitter_.add(asm_reg_to_reg(ops[0].reg), asm_reg_to_reg(ops[1].reg));
                    } else if (ops[0].kind == AsmOperandKind::REG &&
                               ops[1].kind == AsmOperandKind::IMM) {
                        emitter_.add(asm_reg_to_reg(ops[0].reg), static_cast<int32_t>(ops[1].imm));
                    } else if (ops[0].kind == AsmOperandKind::REG &&
                               ops[1].kind == AsmOperandKind::VAR) {
                        int32_t offset = locals_[ops[1].var_name].stack_offset;
                        emitter_.mov(Reg::rcx(), Mem(Reg::rbp(), offset));
                        emitter_.add(asm_reg_to_reg(ops[0].reg), Reg::rcx());
                    }
                }
                break;
            case AsmOpcode::SUB:
                if (ops.size() == 2) {
                    if (ops[0].kind == AsmOperandKind::REG && ops[1].kind == AsmOperandKind::REG) {
                        emitter_.sub(asm_reg_to_reg(ops[0].reg), asm_reg_to_reg(ops[1].reg));
                    } else if (ops[0].kind == AsmOperandKind::REG &&
                               ops[1].kind == AsmOperandKind::IMM) {
                        emitter_.sub(asm_reg_to_reg(ops[0].reg), static_cast<int32_t>(ops[1].imm));
                    }
                }
                break;
            case AsmOpcode::IMUL:
                if (ops.size() == 2) {
                    emitter_.imul(asm_reg_to_reg(ops[0].reg), asm_reg_to_reg(ops[1].reg));
                }
                break;
            case AsmOpcode::IDIV:
                if (ops.size() == 1) {
                    emitter_.idiv(asm_reg_to_reg(ops[0].reg));
                }
                break;
            case AsmOpcode::NEG:
                if (ops.size() == 1) {
                    emitter_.neg(asm_reg_to_reg(ops[0].reg));
                }
                break;
            case AsmOpcode::CQO:
                emitter_.cqo();
                break;
            case AsmOpcode::XOR:
                if (ops.size() == 2) {
                    emitter_.xor_(asm_reg_to_reg(ops[0].reg), asm_reg_to_reg(ops[1].reg));
                }
                break;
            case AsmOpcode::AND:
                if (ops.size() == 2) {
                    emitter_.and_(asm_reg_to_reg(ops[0].reg), asm_reg_to_reg(ops[1].reg));
                }
                break;
            case AsmOpcode::OR:
                if (ops.size() == 2) {
                    emitter_.or_(asm_reg_to_reg(ops[0].reg), asm_reg_to_reg(ops[1].reg));
                }
                break;
            case AsmOpcode::TEST:
                if (ops.size() == 2) {
                    emitter_.test(asm_reg_to_reg(ops[0].reg), asm_reg_to_reg(ops[1].reg));
                }
                break;
            case AsmOpcode::CMP:
                if (ops.size() == 2) {
                    if (ops[0].kind == AsmOperandKind::REG && ops[1].kind == AsmOperandKind::REG) {
                        emitter_.cmp(asm_reg_to_reg(ops[0].reg), asm_reg_to_reg(ops[1].reg));
                    } else if (ops[0].kind == AsmOperandKind::REG &&
                               ops[1].kind == AsmOperandKind::IMM) {
                        emitter_.cmp(asm_reg_to_reg(ops[0].reg), static_cast<int32_t>(ops[1].imm));
                    }
                }
                break;
            case AsmOpcode::PUSH:
                if (ops.size() == 1) {
                    emitter_.push(asm_reg_to_reg(ops[0].reg));
                }
                break;
            case AsmOpcode::POP:
                if (ops.size() == 1) {
                    emitter_.pop(asm_reg_to_reg(ops[0].reg));
                }
                break;
            case AsmOpcode::RET:
                emitter_.ret();
                break;
            case AsmOpcode::SYSCALL:
                emitter_.syscall();
                break;
            case AsmOpcode::NOP:
                emitter_.code().push_back(0x90);
                break;
            case AsmOpcode::LEA:
                if (ops.size() == 2 && ops[1].kind == AsmOperandKind::VAR) {
                    int32_t offset = locals_[ops[1].var_name].stack_offset;
                    emitter_.lea(asm_reg_to_reg(ops[0].reg), Mem(Reg::rbp(), offset));
                } else if (ops.size() == 2 && ops[1].kind == AsmOperandKind::MEM) {
                    emitter_.lea(asm_reg_to_reg(ops[0].reg),
                                 Mem(asm_reg_to_reg(ops[1].mem_base), ops[1].mem_offset));
                }
                break;
            case AsmOpcode::MOVZX:
                if (ops.size() == 2) {
                    emitter_.movzx(asm_reg_to_reg(ops[0].reg), asm_reg_to_reg(ops[1].reg));
                }
                break;
            case AsmOpcode::SETE:
                if (ops.size() == 1) {
                    emitter_.setcc(Cond::E, asm_reg_to_reg(ops[0].reg));
                }
                break;
            case AsmOpcode::SETNE:
                if (ops.size() == 1) {
                    emitter_.setcc(Cond::NE, asm_reg_to_reg(ops[0].reg));
                }
                break;
            case AsmOpcode::SETL:
                if (ops.size() == 1) {
                    emitter_.setcc(Cond::L, asm_reg_to_reg(ops[0].reg));
                }
                break;
            case AsmOpcode::SETG:
                if (ops.size() == 1) {
                    emitter_.setcc(Cond::G, asm_reg_to_reg(ops[0].reg));
                }
                break;
            case AsmOpcode::SETLE:
                if (ops.size() == 1) {
                    emitter_.setcc(Cond::LE, asm_reg_to_reg(ops[0].reg));
                }
                break;
            case AsmOpcode::SETGE:
                if (ops.size() == 1) {
                    emitter_.setcc(Cond::GE, asm_reg_to_reg(ops[0].reg));
                }
                break;
            case AsmOpcode::JMP:
                if (ops.size() == 1 && ops[0].kind == AsmOperandKind::LABEL) {
                    emitter_.jmp(0);
                    size_t patch_loc = emitter_.current_offset() - 4;
                    jump_patches.push_back({patch_loc, ops[0].var_name, emitter_.current_offset()});
                }
                break;
            case AsmOpcode::JE:
            case AsmOpcode::JZ:
                if (ops.size() == 1 && ops[0].kind == AsmOperandKind::LABEL) {
                    emitter_.jcc(Cond::Z, 0);
                    size_t patch_loc = emitter_.current_offset() - 4;
                    jump_patches.push_back({patch_loc, ops[0].var_name, emitter_.current_offset()});
                }
                break;
            case AsmOpcode::JNE:
            case AsmOpcode::JNZ:
                if (ops.size() == 1 && ops[0].kind == AsmOperandKind::LABEL) {
                    emitter_.jcc(Cond::NE, 0);
                    size_t patch_loc = emitter_.current_offset() - 4;
                    jump_patches.push_back({patch_loc, ops[0].var_name, emitter_.current_offset()});
                }
                break;
            case AsmOpcode::JL:
                if (ops.size() == 1 && ops[0].kind == AsmOperandKind::LABEL) {
                    emitter_.jcc(Cond::L, 0);
                    size_t patch_loc = emitter_.current_offset() - 4;
                    jump_patches.push_back({patch_loc, ops[0].var_name, emitter_.current_offset()});
                }
                break;
            case AsmOpcode::JG:
                if (ops.size() == 1 && ops[0].kind == AsmOperandKind::LABEL) {
                    emitter_.jcc(Cond::G, 0);
                    size_t patch_loc = emitter_.current_offset() - 4;
                    jump_patches.push_back({patch_loc, ops[0].var_name, emitter_.current_offset()});
                }
                break;
            case AsmOpcode::JLE:
                if (ops.size() == 1 && ops[0].kind == AsmOperandKind::LABEL) {
                    emitter_.jcc(Cond::LE, 0);
                    size_t patch_loc = emitter_.current_offset() - 4;
                    jump_patches.push_back({patch_loc, ops[0].var_name, emitter_.current_offset()});
                }
                break;
            case AsmOpcode::JGE:
                if (ops.size() == 1 && ops[0].kind == AsmOperandKind::LABEL) {
                    emitter_.jcc(Cond::GE, 0);
                    size_t patch_loc = emitter_.current_offset() - 4;
                    jump_patches.push_back({patch_loc, ops[0].var_name, emitter_.current_offset()});
                }
                break;
            default:
                break;
        }
    }

    // Patch all jump addresses
    for (const auto& [patch_loc, label_name, after_jmp] : jump_patches) {
        auto it = label_offsets.find(label_name);
        if (it != label_offsets.end()) {
            int32_t rel = static_cast<int32_t>(it->second - after_jmp);
            emitter_.patch_rel32(patch_loc, rel);
        }
    }
}

}  // namespace zex
