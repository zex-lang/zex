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
        emitter_.mov(Reg::RAX, lit->value);
    } else if (auto* fl = dynamic_cast<const FloatLiteral*>(expr)) {
        uint32_t bits;
        std::memcpy(&bits, &fl->value, sizeof(float));
        emitter_.mov(Reg::RAX, static_cast<int64_t>(bits));
        emitter_.movd(Reg::XMM0, Reg::RAX);
    } else if (auto* cl = dynamic_cast<const CharLiteral*>(expr)) {
        emitter_.mov(Reg::RAX, static_cast<int64_t>(cl->value));
    } else if (auto* sl = dynamic_cast<const StringLiteral*>(expr)) {
        size_t str_index = add_string_literal(sl->value);
        // Emit mov with placeholder, record patch location
        size_t patch_loc = emitter_.current_offset() + 2;
        emitter_.mov(Reg::RAX, static_cast<int64_t>(0));
        string_patches_.push_back({patch_loc, str_index});
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
    } else if (auto* cast = dynamic_cast<const CastExpr*>(expr)) {
        generate_expression(cast->expr.get());
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

Reg CodeGenerator::asm_reg_to_reg(AsmReg ar) {
    switch (ar) {
        // 64 bit
        case AsmReg::RAX:
            return Reg::RAX;
        case AsmReg::RCX:
            return Reg::RCX;
        case AsmReg::RDX:
            return Reg::RDX;
        case AsmReg::RBX:
            return Reg::RBX;
        case AsmReg::RSP:
            return Reg::RSP;
        case AsmReg::RBP:
            return Reg::RBP;
        case AsmReg::RSI:
            return Reg::RSI;
        case AsmReg::RDI:
            return Reg::RDI;
        case AsmReg::R8:
            return Reg::R8;
        case AsmReg::R9:
            return Reg::R9;
        case AsmReg::R10:
            return Reg::R10;
        case AsmReg::R11:
            return Reg::R11;
        case AsmReg::R12:
            return Reg::R12;
        case AsmReg::R13:
            return Reg::R13;
        case AsmReg::R14:
            return Reg::R14;
        case AsmReg::R15:
            return Reg::R15;
        // 32 bit maps to same encoding
        case AsmReg::EAX:
            return Reg::RAX;
        case AsmReg::ECX:
            return Reg::RCX;
        case AsmReg::EDX:
            return Reg::RDX;
        case AsmReg::EBX:
            return Reg::RBX;
        case AsmReg::ESP:
            return Reg::RSP;
        case AsmReg::EBP:
            return Reg::RBP;
        case AsmReg::ESI:
            return Reg::RSI;
        case AsmReg::EDI:
            return Reg::RDI;
        case AsmReg::R8D:
            return Reg::R8;
        case AsmReg::R9D:
            return Reg::R9;
        case AsmReg::R10D:
            return Reg::R10;
        case AsmReg::R11D:
            return Reg::R11;
        case AsmReg::R12D:
            return Reg::R12;
        case AsmReg::R13D:
            return Reg::R13;
        case AsmReg::R14D:
            return Reg::R14;
        case AsmReg::R15D:
            return Reg::R15;
        // 16 bit maps to same encoding
        case AsmReg::AX:
            return Reg::RAX;
        case AsmReg::CX:
            return Reg::RCX;
        case AsmReg::DX:
            return Reg::RDX;
        case AsmReg::BX:
            return Reg::RBX;
        case AsmReg::SP:
            return Reg::RSP;
        case AsmReg::BP:
            return Reg::RBP;
        case AsmReg::SI:
            return Reg::RSI;
        case AsmReg::DI:
            return Reg::RDI;
        // 8 bit low
        case AsmReg::AL:
            return Reg::AL;
        case AsmReg::CL:
            return Reg::CL;
        case AsmReg::DL:
            return Reg::DL;
        case AsmReg::BL:
            return Reg::BL;
        // 8 bit high maps to same for now
        case AsmReg::AH:
            return Reg::RAX;
        case AsmReg::CH:
            return Reg::RCX;
        case AsmReg::DH:
            return Reg::RDX;
        case AsmReg::BH:
            return Reg::RBX;
    }
    return Reg::RAX;
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
                        emitter_.mov(asm_reg_to_reg(ops[0].reg), Mem(Reg::RBP, offset));
                    } else if (ops[0].kind == AsmOperandKind::VAR &&
                               ops[1].kind == AsmOperandKind::REG) {
                        int32_t offset = locals_[ops[0].var_name].stack_offset;
                        emitter_.mov(Mem(Reg::RBP, offset), asm_reg_to_reg(ops[1].reg));
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
                        emitter_.mov(Reg::RCX, Mem(Reg::RBP, offset));
                        emitter_.add(asm_reg_to_reg(ops[0].reg), Reg::RCX);
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
                        emitter_.cmp_imm(asm_reg_to_reg(ops[0].reg),
                                         static_cast<int32_t>(ops[1].imm));
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
                    emitter_.lea(asm_reg_to_reg(ops[0].reg), Mem(Reg::RBP, offset));
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
