#include "zex/x86_64.hpp"

namespace zex {

void X86_64::emit8(uint8_t byte) {
    code_.push_back(byte);
}

void X86_64::emit32(int32_t value) {
    emit8(static_cast<uint8_t>(value & 0xFF));
    emit8(static_cast<uint8_t>((value >> 8) & 0xFF));
    emit8(static_cast<uint8_t>((value >> 16) & 0xFF));
    emit8(static_cast<uint8_t>((value >> 24) & 0xFF));
}

void X86_64::emit64(int64_t value) {
    emit32(static_cast<int32_t>(value & 0xFFFFFFFF));
    emit32(static_cast<int32_t>((value >> 32) & 0xFFFFFFFF));
}

void X86_64::patch_rel32(size_t pos, int32_t value) {
    code_[pos] = static_cast<uint8_t>(value & 0xFF);
    code_[pos + 1] = static_cast<uint8_t>((value >> 8) & 0xFF);
    code_[pos + 2] = static_cast<uint8_t>((value >> 16) & 0xFF);
    code_[pos + 3] = static_cast<uint8_t>((value >> 24) & 0xFF);
}

void X86_64::emit_rex(bool w, Reg reg, Reg rm) {
    uint8_t rex = 0x40;
    if (w)
        rex |= 0x08;  // REX.W
    if (needs_rex_r(reg))
        rex |= 0x04;  // REX.R
    if (needs_rex_r(rm))
        rex |= 0x01;  // REX.B
    if (rex != 0x40 || w)
        emit8(rex);
}

void X86_64::emit_rex(bool w, uint8_t reg, Reg rm) {
    uint8_t rex = 0x40;
    if (w)
        rex |= 0x08;
    if (reg >= 8)
        rex |= 0x04;
    if (needs_rex_r(rm))
        rex |= 0x01;
    if (rex != 0x40 || w)
        emit8(rex);
}

void X86_64::emit_rex_single(bool w, Reg rm) {
    uint8_t rex = 0x40;
    if (w)
        rex |= 0x08;
    if (needs_rex_r(rm))
        rex |= 0x01;
    if (rex != 0x40 || w)
        emit8(rex);
}

void X86_64::emit_modrm(uint8_t mod, uint8_t reg, uint8_t rm) {
    emit8((mod << 6) | ((reg & 0x07) << 3) | (rm & 0x07));
}

void X86_64::emit_modrm_reg(Reg reg, Reg rm) {
    emit_modrm(0b11, reg_num(reg), reg_num(rm));
}

void X86_64::emit_modrm_mem(Reg reg, Mem mem) {
    int32_t offset = mem.offset;
    uint8_t base = reg_num(mem.base);
    uint8_t r = reg_num(reg);

    bool needs_sib = (base == 4);     // RSP or R12 require SIB byte
    bool is_rbp_r13 = (base == 5);    // RBP or R13 require offset

    if (offset == 0 && !is_rbp_r13) {
        emit_modrm(0b00, r, needs_sib ? 4 : base);
        if (needs_sib) emit8((0 << 6) | (4 << 3) | base);  // SIB: scale=0, index=none, base
    } else if (offset >= -128 && offset <= 127) {
        emit_modrm(0b01, r, needs_sib ? 4 : base);
        if (needs_sib) emit8((0 << 6) | (4 << 3) | base);
        emit8(static_cast<uint8_t>(offset));
    } else {
        emit_modrm(0b10, r, needs_sib ? 4 : base);
        if (needs_sib) emit8((0 << 6) | (4 << 3) | base);
        emit32(offset);
    }
}

void X86_64::mov(Reg dst, Reg src) {
    emit_rex(true, src, dst);
    emit8(0x89);  // MOV r/m64, r64
    emit_modrm_reg(src, dst);
}

void X86_64::mov(Reg dst, int64_t imm) {
    emit_rex_single(true, dst);
    emit8(0xB8 + reg_num(dst));  // MOV r64, imm64
    emit64(imm);
}

void X86_64::mov(Reg dst, int32_t imm) {
    if (needs_rex_r(dst)) {
        emit8(0x41);
    }
    emit8(0xB8 + reg_num(dst));  // MOV r32, imm32
    emit32(imm);
}

void X86_64::mov(Reg dst, Mem src) {
    emit_rex(true, dst, src.base);
    emit8(0x8B);  // MOV r64, r/m64
    emit_modrm_mem(dst, src);
}

void X86_64::mov(Mem dst, Reg src) {
    emit_rex(true, src, dst.base);
    emit8(0x89);  // MOV r/m64, r64
    emit_modrm_mem(src, dst);
}

void X86_64::lea(Reg dst, Mem src) {
    emit_rex(true, dst, src.base);
    emit8(0x8D);  // LEA r64, m
    emit_modrm_mem(dst, src);
}

void X86_64::add(Reg dst, Reg src) {
    emit_rex(true, src, dst);
    emit8(0x01);  // ADD r/m64, r64
    emit_modrm_reg(src, dst);
}

void X86_64::add(Reg dst, int32_t imm) {
    emit_rex_single(true, dst);
    if (imm >= -128 && imm <= 127) {
        emit8(0x83);  // ADD r/m64, imm8
        emit_modrm(0b11, 0, reg_num(dst));
        emit8(static_cast<uint8_t>(imm));
    } else {
        emit8(0x81);  // ADD r/m64, imm32
        emit_modrm(0b11, 0, reg_num(dst));
        emit32(imm);
    }
}

void X86_64::sub(Reg dst, Reg src) {
    emit_rex(true, src, dst);
    emit8(0x29);  // SUB r/m64, r64
    emit_modrm_reg(src, dst);
}

void X86_64::sub(Reg dst, int32_t imm) {
    emit_rex_single(true, dst);
    if (imm >= -128 && imm <= 127) {
        emit8(0x83);  // SUB r/m64, imm8
        emit_modrm(0b11, 5, reg_num(dst));
        emit8(static_cast<uint8_t>(imm));
    } else {
        emit8(0x81);  // SUB r/m64, imm32
        emit_modrm(0b11, 5, reg_num(dst));
        emit32(imm);
    }
}

void X86_64::imul(Reg dst, Reg src) {
    emit_rex(true, dst, src);
    emit8(0x0F);
    emit8(0xAF);  // IMUL r64, r/m64
    emit_modrm_reg(dst, src);
}

void X86_64::idiv(Reg src) {
    emit_rex_single(true, src);
    emit8(0xF7);  // IDIV r/m64
    emit_modrm(0b11, 7, reg_num(src));
}

void X86_64::neg(Reg reg) {
    emit_rex_single(true, reg);
    emit8(0xF7);  // NEG r/m64
    emit_modrm(0b11, 3, reg_num(reg));
}

void X86_64::cqo() {
    emit8(0x48);
    emit8(0x99);  // CQO
}

void X86_64::xor_(Reg dst, Reg src) {
    emit_rex(true, src, dst);
    emit8(0x31);  // XOR r/m64, r64
    emit_modrm_reg(src, dst);
}

void X86_64::and_(Reg dst, Reg src) {
    emit_rex(true, src, dst);
    emit8(0x21);  // AND r/m64, r64
    emit_modrm_reg(src, dst);
}

void X86_64::or_(Reg dst, Reg src) {
    emit_rex(true, src, dst);
    emit8(0x09);  // OR r/m64, r64
    emit_modrm_reg(src, dst);
}

void X86_64::test(Reg a, Reg b) {
    emit_rex(true, b, a);
    emit8(0x85);  // TEST r/m64, r64
    emit_modrm_reg(b, a);
}

void X86_64::cmp(Reg a, Reg b) {
    emit_rex(true, b, a);
    emit8(0x39);  // CMP r/m64, r64
    emit_modrm_reg(b, a);
}

void X86_64::cmp_imm(Reg a, int32_t imm) {
    emit_rex_single(true, a);
    if (imm >= -128 && imm <= 127) {
        emit8(0x83);  // CMP r/m64, imm8
        emit_modrm(0b11, 7, reg_num(a));
        emit8(static_cast<uint8_t>(imm));
    } else {
        emit8(0x81);  // CMP r/m64, imm32
        emit_modrm(0b11, 7, reg_num(a));
        emit32(imm);
    }
}

void X86_64::push(Reg reg) {
    if (needs_rex_r(reg)) {
        emit8(0x41);
    }
    emit8(0x50 + reg_num(reg));  // PUSH r64
}

void X86_64::pop(Reg reg) {
    if (needs_rex_r(reg)) {
        emit8(0x41);
    }
    emit8(0x58 + reg_num(reg));  // POP r64
}

void X86_64::call(int32_t rel) {
    emit8(0xE8);  // CALL rel32
    emit32(rel);
}

void X86_64::jmp(int32_t rel) {
    emit8(0xE9);  // JMP rel32
    emit32(rel);
}

void X86_64::jcc(Cond cc, int32_t rel) {
    emit8(0x0F);
    emit8(0x80 + static_cast<uint8_t>(cc));  // Jcc rel32
    emit32(rel);
}

void X86_64::ret() {
    emit8(0xC3);  // RET
}

void X86_64::setcc(Cond cc, Reg dst) {
    // Need REX prefix for extended registers AND for accessing SPL/BPL/SIL/DIL
    uint8_t rex = 0x40;
    if (needs_rex_r(dst)) rex |= 0x01;   // REX.B for R8-R15
    bool needs_rex_for_low_byte = (reg_num(dst) >= 4 && reg_num(dst) <= 7 && !needs_rex_r(dst));
    if (rex != 0x40 || needs_rex_for_low_byte) emit8(rex);

    emit8(0x0F);
    emit8(0x90 + static_cast<uint8_t>(cc));  // SETcc r/m8
    emit_modrm(0b11, 0, reg_num(dst));
}

void X86_64::movzx(Reg dst, Reg src8) {
    // Need REX for extended registers AND for accessing SPL/BPL/SIL/DIL
    uint8_t rex = 0x48;  // REX.W for 64-bit destination
    if (needs_rex_r(dst)) rex |= 0x04;   // REX.R
    if (needs_rex_r(src8)) rex |= 0x01;  // REX.B
    bool needs_rex_for_low_byte = (reg_num(src8) >= 4 && reg_num(src8) <= 7 && !needs_rex_r(src8));
    if (rex != 0x40 || needs_rex_for_low_byte) emit8(rex);

    emit8(0x0F);
    emit8(0xB6);  // MOVZX r64, r/m8
    emit_modrm_reg(dst, src8);
}

void X86_64::syscall() {
    emit8(0x0F);
    emit8(0x05);  // SYSCALL
}

void X86_64::movss(Reg xmm_dst, Mem src) {
    emit8(0xF3);
    // REX must come after F3 prefix for XMM8-15
    uint8_t rex = 0x40;
    if (needs_rex_r(xmm_dst)) rex |= 0x04;  // REX.R
    if (needs_rex_r(src.base)) rex |= 0x01; // REX.B
    if (rex != 0x40) emit8(rex);
    emit8(0x0F);
    emit8(0x10);  // MOVSS xmm, m32
    emit_modrm_mem(xmm_dst, src);
}

void X86_64::movss(Mem dst, Reg xmm_src) {
    emit8(0xF3);
    uint8_t rex = 0x40;
    if (needs_rex_r(xmm_src)) rex |= 0x04;
    if (needs_rex_r(dst.base)) rex |= 0x01;
    if (rex != 0x40) emit8(rex);
    emit8(0x0F);
    emit8(0x11);  // MOVSS m32, xmm
    emit_modrm_mem(xmm_src, dst);
}

void X86_64::movss(Reg xmm_dst, Reg xmm_src) {
    emit8(0xF3);
    uint8_t rex = 0x40;
    if (needs_rex_r(xmm_dst)) rex |= 0x04;
    if (needs_rex_r(xmm_src)) rex |= 0x01;
    if (rex != 0x40) emit8(rex);
    emit8(0x0F);
    emit8(0x10);  // MOVSS xmm, xmm
    emit_modrm_reg(xmm_dst, xmm_src);
}

void X86_64::movd(Reg xmm, Reg r32) {
    emit8(0x66);
    // REX must come after 66 prefix but before 0F
    uint8_t rex = 0x40;
    if (needs_rex_r(xmm)) rex |= 0x04;
    if (needs_rex_r(r32)) rex |= 0x01;
    if (rex != 0x40) emit8(rex);
    emit8(0x0F);
    emit8(0x6E);  // MOVD xmm, r/m32
    emit_modrm_reg(xmm, r32);
}

void X86_64::movd_to_reg(Reg r32, Reg xmm) {
    emit8(0x66);
    uint8_t rex = 0x40;
    if (needs_rex_r(xmm)) rex |= 0x04;
    if (needs_rex_r(r32)) rex |= 0x01;
    if (rex != 0x40) emit8(rex);
    emit8(0x0F);
    emit8(0x7E);  // MOVD r/m32, xmm
    emit_modrm_reg(xmm, r32);
}

void X86_64::addss(Reg dst, Reg src) {
    emit8(0xF3);
    uint8_t rex = 0x40;
    if (needs_rex_r(dst)) rex |= 0x04;
    if (needs_rex_r(src)) rex |= 0x01;
    if (rex != 0x40) emit8(rex);
    emit8(0x0F);
    emit8(0x58);  // ADDSS xmm, xmm
    emit_modrm_reg(dst, src);
}

void X86_64::subss(Reg dst, Reg src) {
    emit8(0xF3);
    uint8_t rex = 0x40;
    if (needs_rex_r(dst)) rex |= 0x04;
    if (needs_rex_r(src)) rex |= 0x01;
    if (rex != 0x40) emit8(rex);
    emit8(0x0F);
    emit8(0x5C);  // SUBSS xmm, xmm
    emit_modrm_reg(dst, src);
}

void X86_64::mulss(Reg dst, Reg src) {
    emit8(0xF3);
    uint8_t rex = 0x40;
    if (needs_rex_r(dst)) rex |= 0x04;
    if (needs_rex_r(src)) rex |= 0x01;
    if (rex != 0x40) emit8(rex);
    emit8(0x0F);
    emit8(0x59);  // MULSS xmm, xmm
    emit_modrm_reg(dst, src);
}

void X86_64::divss(Reg dst, Reg src) {
    emit8(0xF3);
    uint8_t rex = 0x40;
    if (needs_rex_r(dst)) rex |= 0x04;
    if (needs_rex_r(src)) rex |= 0x01;
    if (rex != 0x40) emit8(rex);
    emit8(0x0F);
    emit8(0x5E);  // DIVSS xmm, xmm
    emit_modrm_reg(dst, src);
}

void X86_64::xorps(Reg dst, Reg src) {
    uint8_t rex = 0x40;
    if (needs_rex_r(dst)) rex |= 0x04;
    if (needs_rex_r(src)) rex |= 0x01;
    if (rex != 0x40) emit8(rex);
    emit8(0x0F);
    emit8(0x57);  // XORPS xmm, xmm
    emit_modrm_reg(dst, src);
}

}  // namespace zex
