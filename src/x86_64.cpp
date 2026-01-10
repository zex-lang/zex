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

void X86_64::emit_push_rbp() {
    emit8(0x55);
}

void X86_64::emit_mov_rbp_rsp() {
    emit8(0x48);
    emit8(0x89);
    emit8(0xE5);
}

void X86_64::emit_pop_rbp() {
    emit8(0x5D);
}

void X86_64::emit_ret() {
    emit8(0xC3);
}

void X86_64::emit_sub_rsp_imm8(uint8_t imm) {
    emit8(0x48);
    emit8(0x83);
    emit8(0xEC);
    emit8(imm);
}

void X86_64::emit_add_rsp_imm8(uint8_t imm) {
    emit8(0x48);
    emit8(0x83);
    emit8(0xC4);
    emit8(imm);
}

void X86_64::emit_mov_rax_imm64(int64_t imm) {
    emit8(0x48);
    emit8(0xB8);
    emit64(imm);
}

void X86_64::emit_mov_rax_rbp_offset(int32_t offset) {
    emit8(0x48);
    emit8(0x8B);
    emit8(0x45);

    if (offset >= -128 && offset <= 127) {
        emit8(static_cast<uint8_t>(offset));
    } else {
        code_.back() = 0x85;
        emit32(offset);
    }
}

void X86_64::emit_mov_rbp_offset_rax(int32_t offset) {
    emit8(0x48);
    emit8(0x89);
    emit8(0x45);

    if (offset >= -128 && offset <= 127) {
        emit8(static_cast<uint8_t>(offset));
    } else {
        code_.back() = 0x85;
        emit32(offset);
    }
}

void X86_64::emit_call_rel32(int32_t offset) {
    emit8(0xE8);
    emit32(offset);
}

void X86_64::emit_mov_rax_imm32(int32_t imm) {
    emit8(0xB8);
    emit32(imm);
}

void X86_64::emit_mov_rdi_rax() {
    emit8(0x48);
    emit8(0x89);
    emit8(0xC7);
}

void X86_64::emit_mov_rsi_rax() {
    emit8(0x48);
    emit8(0x89);
    emit8(0xC6);
}

void X86_64::emit_mov_rdx_rax() {
    emit8(0x48);
    emit8(0x89);
    emit8(0xC2);
}

void X86_64::emit_mov_rcx_rax() {
    emit8(0x48);
    emit8(0x89);
    emit8(0xC1);
}

void X86_64::emit_mov_r8_rax() {
    emit8(0x49);
    emit8(0x89);
    emit8(0xC0);
}

void X86_64::emit_mov_r9_rax() {
    emit8(0x49);
    emit8(0x89);
    emit8(0xC1);
}

void X86_64::emit_mov_rbp_offset_rdi(int32_t offset) {
    emit8(0x48);
    emit8(0x89);
    emit8(0x7D);
    emit8(static_cast<uint8_t>(offset));
}

void X86_64::emit_mov_rbp_offset_rsi(int32_t offset) {
    emit8(0x48);
    emit8(0x89);
    emit8(0x75);
    emit8(static_cast<uint8_t>(offset));
}

void X86_64::emit_mov_rbp_offset_rdx(int32_t offset) {
    emit8(0x48);
    emit8(0x89);
    emit8(0x55);
    emit8(static_cast<uint8_t>(offset));
}

void X86_64::emit_mov_rbp_offset_rcx(int32_t offset) {
    emit8(0x48);
    emit8(0x89);
    emit8(0x4D);
    emit8(static_cast<uint8_t>(offset));
}

void X86_64::emit_mov_rbp_offset_r8(int32_t offset) {
    emit8(0x4C);
    emit8(0x89);
    emit8(0x45);
    emit8(static_cast<uint8_t>(offset));
}

void X86_64::emit_mov_rbp_offset_r9(int32_t offset) {
    emit8(0x4C);
    emit8(0x89);
    emit8(0x4D);
    emit8(static_cast<uint8_t>(offset));
}

void X86_64::emit_syscall() {
    emit8(0x0F);
    emit8(0x05);
}

void X86_64::emit_push_rax() {
    emit8(0x50);
}

void X86_64::emit_pop_rcx() {
    emit8(0x59);
}

void X86_64::emit_pop_rdi() {
    emit8(0x5F);
}

void X86_64::emit_pop_rsi() {
    emit8(0x5E);
}

void X86_64::emit_pop_rdx() {
    emit8(0x5A);
}

void X86_64::emit_pop_r8() {
    emit8(0x41);
    emit8(0x58);
}

void X86_64::emit_pop_r9() {
    emit8(0x41);
    emit8(0x59);
}

void X86_64::emit_add_rax_rcx() {
    emit8(0x48);
    emit8(0x01);
    emit8(0xC8);
}

void X86_64::emit_sub_rax_rcx() {
    emit8(0x48);
    emit8(0x29);
    emit8(0xC8);
}

void X86_64::emit_imul_rax_rcx() {
    emit8(0x48);
    emit8(0x0F);
    emit8(0xAF);
    emit8(0xC1);
}

void X86_64::emit_cqo() {
    emit8(0x48);
    emit8(0x99);
}

void X86_64::emit_idiv_rcx() {
    emit8(0x48);
    emit8(0xF7);
    emit8(0xF9);
}

void X86_64::emit_mov_rax_rdx() {
    emit8(0x48);
    emit8(0x89);
    emit8(0xD0);
}

void X86_64::emit_neg_rax() {
    emit8(0x48);
    emit8(0xF7);
    emit8(0xD8);
}

void X86_64::emit_cmp_rax_rcx() {
    emit8(0x48);
    emit8(0x39);
    emit8(0xC8);
}

void X86_64::emit_sete_al() {
    emit8(0x0F);
    emit8(0x94);
    emit8(0xC0);
}

void X86_64::emit_setne_al() {
    emit8(0x0F);
    emit8(0x95);
    emit8(0xC0);
}

void X86_64::emit_setl_al() {
    emit8(0x0F);
    emit8(0x9C);
    emit8(0xC0);
}

void X86_64::emit_setg_al() {
    emit8(0x0F);
    emit8(0x9F);
    emit8(0xC0);
}

void X86_64::emit_setle_al() {
    emit8(0x0F);
    emit8(0x9E);
    emit8(0xC0);
}

void X86_64::emit_setge_al() {
    emit8(0x0F);
    emit8(0x9D);
    emit8(0xC0);
}

void X86_64::emit_movzx_rax_al() {
    emit8(0x48);
    emit8(0x0F);
    emit8(0xB6);
    emit8(0xC0);
}

void X86_64::emit_test_rax_rax() {
    emit8(0x48);
    emit8(0x85);
    emit8(0xC0);
}

void X86_64::emit_setz_al() {
    emit8(0x0F);
    emit8(0x94);
    emit8(0xC0);
}

void X86_64::emit_xor_rax_rax() {
    emit8(0x48);
    emit8(0x31);
    emit8(0xC0);
}

void X86_64::emit_test_rcx_rcx() {
    emit8(0x48);
    emit8(0x85);
    emit8(0xC9);
}

void X86_64::emit_jz_rel32(int32_t offset) {
    emit8(0x0F);
    emit8(0x84);
    emit32(offset);
}

void X86_64::emit_jmp_rel32(int32_t offset) {
    emit8(0xE9);
    emit32(offset);
}

}  // namespace zex
