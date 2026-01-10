#include "zex/x86_64.hpp"

namespace zex {

void X86_64::emit8(uint8_t byte) {
    code_.push_back(byte);
}

void X86_64::emit32(int32_t value) {
    code_.push_back(static_cast<uint8_t>(value & 0xFF));
    code_.push_back(static_cast<uint8_t>((value >> 8) & 0xFF));
    code_.push_back(static_cast<uint8_t>((value >> 16) & 0xFF));
    code_.push_back(static_cast<uint8_t>((value >> 24) & 0xFF));
}

void X86_64::emit64(int64_t value) {
    for (int i = 0; i < 8; i++) {
        code_.push_back(static_cast<uint8_t>((value >> (i * 8)) & 0xFF));
    }
}

size_t X86_64::current_offset() const {
    return code_.size();
}

void X86_64::patch_rel32(size_t pos, int32_t value) {
    code_[pos] = static_cast<uint8_t>(value & 0xFF);
    code_[pos + 1] = static_cast<uint8_t>((value >> 8) & 0xFF);
    code_[pos + 2] = static_cast<uint8_t>((value >> 16) & 0xFF);
    code_[pos + 3] = static_cast<uint8_t>((value >> 24) & 0xFF);
}

// push rbp
// Opcode: 0x55
void X86_64::emit_push_rbp() {
    emit8(0x55);
}

// mov rbp, rsp
// REX.W + 0x89 + ModR/M(0xE5)
// REX.W = 0x48, ModR/M: mod=11, reg=rsp(4), rm=rbp(5) = 0xE5
void X86_64::emit_mov_rbp_rsp() {
    emit8(0x48);  // REX.W
    emit8(0x89);  // MOV r/m64, r64
    emit8(0xE5);  // ModR/M: rsp -> rbp
}

// pop rbp
// Opcode: 0x5D
void X86_64::emit_pop_rbp() {
    emit8(0x5D);
}

// ret
// Opcode: 0xC3
void X86_64::emit_ret() {
    emit8(0xC3);
}

// sub rsp, imm8
// REX.W + 0x83 + ModR/M(0xEC) + imm8
void X86_64::emit_sub_rsp_imm8(uint8_t imm) {
    emit8(0x48);  // REX.W
    emit8(0x83);  // SUB r/m64, imm8
    emit8(0xEC);  // ModR/M: mod=11, reg=5(/5), rm=rsp(4)
    emit8(imm);
}

// add rsp, imm8
// REX.W + 0x83 + ModR/M(0xC4) + imm8
void X86_64::emit_add_rsp_imm8(uint8_t imm) {
    emit8(0x48);  // REX.W
    emit8(0x83);  // ADD r/m64, imm8
    emit8(0xC4);  // ModR/M: mod=11, reg=0(/0), rm=rsp(4)
    emit8(imm);
}

// mov rax, imm64
// REX.W + 0xB8 + imm64
void X86_64::emit_mov_rax_imm64(int64_t imm) {
    emit8(0x48);  // REX.W
    emit8(0xB8);  // MOV rax, imm64
    emit64(imm);
}

// mov rax, [rbp+offset]
// REX.W + 0x8B + ModR/M + disp32
// For [rbp+disp32]: ModR/M = 0x85 (mod=10, reg=rax(0), rm=rbp(5))
void X86_64::emit_mov_rax_rbp_offset(int32_t offset) {
    emit8(0x48);  // REX.W
    emit8(0x8B);  // MOV r64, r/m64
    emit8(0x45);  // ModR/M: mod=01 (disp8), reg=rax(0), rm=rbp(5)

    // Use disp8 if possible, otherwise disp32
    if (offset >= -128 && offset <= 127) {
        emit8(static_cast<uint8_t>(offset));
    } else {
        // Need to change ModR/M to use disp32
        code_.back() = 0x85;  // mod=10 (disp32)
        emit32(offset);
    }
}

// mov [rbp+offset], rax
// REX.W + 0x89 + ModR/M + disp8/32
void X86_64::emit_mov_rbp_offset_rax(int32_t offset) {
    emit8(0x48);  // REX.W
    emit8(0x89);  // MOV r/m64, r64
    emit8(0x45);  // ModR/M: mod=01 (disp8), reg=rax(0), rm=rbp(5)

    if (offset >= -128 && offset <= 127) {
        emit8(static_cast<uint8_t>(offset));
    } else {
        code_.back() = 0x85;  // mod=10 (disp32)
        emit32(offset);
    }
}

// call rel32
// Opcode: 0xE8 + rel32
// rel32 is relative to the instruction AFTER the call
void X86_64::emit_call_rel32(int32_t offset) {
    emit8(0xE8);
    emit32(offset);
}

// mov eax, imm32 (zero-extends to rax)
// Opcode: 0xB8 + imm32
void X86_64::emit_mov_rax_imm32(int32_t imm) {
    emit8(0xB8);  // MOV eax, imm32
    emit32(imm);
}

// mov rdi, rax
// REX.W + 0x89 + ModR/M
void X86_64::emit_mov_rdi_rax() {
    emit8(0x48);  // REX.W
    emit8(0x89);  // MOV r/m64, r64
    emit8(0xC7);  // ModR/M: mod=11, reg=rax(0), rm=rdi(7)
}

// syscall
// Opcode: 0x0F 0x05
void X86_64::emit_syscall() {
    emit8(0x0F);
    emit8(0x05);
}

}  // namespace zex
