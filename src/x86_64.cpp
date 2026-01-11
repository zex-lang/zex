// Zex x86-64 machine code emitter
// Generates raw x86-64 instructions with proper encoding

#include "zex/x86_64.hpp"

namespace zex {

void X86_64::emit8(uint8_t byte) {
    code_.push_back(byte);
}

void X86_64::emit16(uint16_t value) {
    emit8(static_cast<uint8_t>(value & 0xFF));
    emit8(static_cast<uint8_t>((value >> 8) & 0xFF));
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

void X86_64::emit_rex(Reg reg, Reg rm) {
    uint8_t rex = 0x40;
    if (reg.size == OpSize::B64)
        rex |= 0x08;
    if (reg.is_ext())
        rex |= 0x04;
    if (rm.is_ext())
        rex |= 0x01;

    bool need_rex = (rex != 0x40) || (reg.size == OpSize::B64);
    bool need_for_low_byte = (reg.size == OpSize::B8 && !reg.high_byte && reg.num() >= 4 &&
                              reg.num() <= 7 && !reg.is_ext());
    if (!rm.high_byte && rm.size == OpSize::B8 && rm.num() >= 4 && rm.num() <= 7 && !rm.is_ext()) {
        need_for_low_byte = true;
    }

    if (need_rex || need_for_low_byte) {
        emit8(rex);
    }
}

void X86_64::emit_rex(Reg rm) {
    uint8_t rex = 0x40;
    if (rm.size == OpSize::B64)
        rex |= 0x08;
    if (rm.is_ext())
        rex |= 0x01;

    bool need_rex = (rex != 0x40) || (rm.size == OpSize::B64);
    bool need_for_low_byte =
        (rm.size == OpSize::B8 && !rm.high_byte && rm.num() >= 4 && rm.num() <= 7 && !rm.is_ext());

    if (need_rex || need_for_low_byte) {
        emit8(rex);
    }
}

void X86_64::emit_rex_xmm(XMM reg, Reg rm) {
    uint8_t rex = 0x40;
    if (xmm_ext(reg))
        rex |= 0x04;
    if (rm.is_ext())
        rex |= 0x01;
    if (rex != 0x40)
        emit8(rex);
}

void X86_64::emit_rex_xmm(Reg reg, XMM rm) {
    uint8_t rex = 0x40;
    if (reg.is_ext())
        rex |= 0x04;
    if (xmm_ext(rm))
        rex |= 0x01;
    if (rex != 0x40)
        emit8(rex);
}

void X86_64::emit_rex_xmm(XMM reg, XMM rm) {
    uint8_t rex = 0x40;
    if (xmm_ext(reg))
        rex |= 0x04;
    if (xmm_ext(rm))
        rex |= 0x01;
    if (rex != 0x40)
        emit8(rex);
}

void X86_64::emit_modrm(uint8_t mod, uint8_t reg, uint8_t rm) {
    emit8((mod << 6) | ((reg & 0x07) << 3) | (rm & 0x07));
}

void X86_64::emit_modrm_reg(Reg reg, Reg rm) {
    uint8_t r = reg.high_byte ? (reg.num() + 4) : reg.num();
    uint8_t m = rm.high_byte ? (rm.num() + 4) : rm.num();
    emit_modrm(0b11, r, m);
}

void X86_64::emit_modrm_mem(Reg reg, Mem mem) {
    emit_modrm_mem(reg.num(), mem);
}

void X86_64::emit_modrm_mem(uint8_t reg_num_val, Mem mem) {
    int32_t offset = mem.offset;
    uint8_t base = mem.base.num();

    bool needs_sib = (base == 4);
    bool is_rbp_r13 = (base == 5);

    if (offset == 0 && !is_rbp_r13) {
        emit_modrm(0b00, reg_num_val, needs_sib ? 4 : base);
        if (needs_sib)
            emit8((0 << 6) | (4 << 3) | base);
    } else if (offset >= -128 && offset <= 127) {
        emit_modrm(0b01, reg_num_val, needs_sib ? 4 : base);
        if (needs_sib)
            emit8((0 << 6) | (4 << 3) | base);
        emit8(static_cast<uint8_t>(offset));
    } else {
        emit_modrm(0b10, reg_num_val, needs_sib ? 4 : base);
        if (needs_sib)
            emit8((0 << 6) | (4 << 3) | base);
        emit32(offset);
    }
}

void X86_64::mov(Reg dst, Reg src) {
    if (dst.size == OpSize::B16)
        emit8(0x66);
    emit_rex(src, dst);
    emit8(dst.size == OpSize::B8 ? 0x88 : 0x89);
    emit_modrm_reg(src, dst);
}

void X86_64::mov(Reg dst, int64_t imm) {
    emit_rex(dst);
    emit8(0xB8 + dst.num());
    emit64(imm);
}

void X86_64::mov(Reg dst, int32_t imm) {
    if (dst.size == OpSize::B16)
        emit8(0x66);
    if (dst.is_ext())
        emit8(0x41);
    emit8(0xB8 + dst.num());
    if (dst.size == OpSize::B16) {
        emit16(static_cast<uint16_t>(imm));
    } else {
        emit32(imm);
    }
}

void X86_64::mov(Reg dst, Mem src) {
    if (dst.size == OpSize::B16)
        emit8(0x66);
    emit_rex(dst, src.base);
    emit8(dst.size == OpSize::B8 ? 0x8A : 0x8B);
    emit_modrm_mem(dst, src);
}

void X86_64::mov(Mem dst, Reg src) {
    if (src.size == OpSize::B16)
        emit8(0x66);
    emit_rex(src, dst.base);
    emit8(src.size == OpSize::B8 ? 0x88 : 0x89);
    emit_modrm_mem(src, dst);
}

void X86_64::lea(Reg dst, Mem src) {
    if (dst.size == OpSize::B16)
        emit8(0x66);
    emit_rex(dst, src.base);
    emit8(0x8D);
    emit_modrm_mem(dst, src);
}

void X86_64::add(Reg dst, Reg src) {
    if (dst.size == OpSize::B16)
        emit8(0x66);
    emit_rex(src, dst);
    emit8(dst.size == OpSize::B8 ? 0x00 : 0x01);
    emit_modrm_reg(src, dst);
}

void X86_64::add(Reg dst, int32_t imm) {
    if (dst.size == OpSize::B16)
        emit8(0x66);
    emit_rex(dst);
    if (imm >= -128 && imm <= 127 && dst.size != OpSize::B8) {
        emit8(0x83);
        emit_modrm(0b11, 0, dst.num());
        emit8(static_cast<uint8_t>(imm));
    } else {
        emit8(dst.size == OpSize::B8 ? 0x80 : 0x81);
        emit_modrm(0b11, 0, dst.num());
        if (dst.size == OpSize::B8) {
            emit8(static_cast<uint8_t>(imm));
        } else if (dst.size == OpSize::B16) {
            emit16(static_cast<uint16_t>(imm));
        } else {
            emit32(imm);
        }
    }
}

void X86_64::sub(Reg dst, Reg src) {
    if (dst.size == OpSize::B16)
        emit8(0x66);
    emit_rex(src, dst);
    emit8(dst.size == OpSize::B8 ? 0x28 : 0x29);
    emit_modrm_reg(src, dst);
}

void X86_64::sub(Reg dst, int32_t imm) {
    if (dst.size == OpSize::B16)
        emit8(0x66);
    emit_rex(dst);
    if (imm >= -128 && imm <= 127 && dst.size != OpSize::B8) {
        emit8(0x83);
        emit_modrm(0b11, 5, dst.num());
        emit8(static_cast<uint8_t>(imm));
    } else {
        emit8(dst.size == OpSize::B8 ? 0x80 : 0x81);
        emit_modrm(0b11, 5, dst.num());
        if (dst.size == OpSize::B8) {
            emit8(static_cast<uint8_t>(imm));
        } else if (dst.size == OpSize::B16) {
            emit16(static_cast<uint16_t>(imm));
        } else {
            emit32(imm);
        }
    }
}

void X86_64::imul(Reg dst, Reg src) {
    if (dst.size == OpSize::B16)
        emit8(0x66);
    emit_rex(dst, src);
    emit8(0x0F);
    emit8(0xAF);
    emit_modrm_reg(dst, src);
}

void X86_64::idiv(Reg src) {
    emit_rex(src);
    emit8(src.size == OpSize::B8 ? 0xF6 : 0xF7);
    emit_modrm(0b11, 7, src.num());
}

void X86_64::neg(Reg reg) {
    emit_rex(reg);
    emit8(reg.size == OpSize::B8 ? 0xF6 : 0xF7);
    emit_modrm(0b11, 3, reg.num());
}

void X86_64::cqo() {
    emit8(0x48);
    emit8(0x99);
}

void X86_64::cdq() {
    emit8(0x99);
}

void X86_64::xor_(Reg dst, Reg src) {
    if (dst.size == OpSize::B16)
        emit8(0x66);
    emit_rex(src, dst);
    emit8(dst.size == OpSize::B8 ? 0x30 : 0x31);
    emit_modrm_reg(src, dst);
}

void X86_64::and_(Reg dst, Reg src) {
    if (dst.size == OpSize::B16)
        emit8(0x66);
    emit_rex(src, dst);
    emit8(dst.size == OpSize::B8 ? 0x20 : 0x21);
    emit_modrm_reg(src, dst);
}

void X86_64::or_(Reg dst, Reg src) {
    if (dst.size == OpSize::B16)
        emit8(0x66);
    emit_rex(src, dst);
    emit8(dst.size == OpSize::B8 ? 0x08 : 0x09);
    emit_modrm_reg(src, dst);
}

void X86_64::test(Reg a, Reg b) {
    if (a.size == OpSize::B16)
        emit8(0x66);
    emit_rex(b, a);
    emit8(a.size == OpSize::B8 ? 0x84 : 0x85);
    emit_modrm_reg(b, a);
}

void X86_64::cmp(Reg a, Reg b) {
    if (a.size == OpSize::B16)
        emit8(0x66);
    emit_rex(b, a);
    emit8(a.size == OpSize::B8 ? 0x38 : 0x39);
    emit_modrm_reg(b, a);
}

void X86_64::cmp(Reg a, int32_t imm) {
    if (a.size == OpSize::B16)
        emit8(0x66);
    emit_rex(a);
    if (imm >= -128 && imm <= 127 && a.size != OpSize::B8) {
        emit8(0x83);
        emit_modrm(0b11, 7, a.num());
        emit8(static_cast<uint8_t>(imm));
    } else {
        emit8(a.size == OpSize::B8 ? 0x80 : 0x81);
        emit_modrm(0b11, 7, a.num());
        if (a.size == OpSize::B8) {
            emit8(static_cast<uint8_t>(imm));
        } else if (a.size == OpSize::B16) {
            emit16(static_cast<uint16_t>(imm));
        } else {
            emit32(imm);
        }
    }
}

void X86_64::push(Reg reg) {
    if (reg.is_ext())
        emit8(0x41);
    emit8(0x50 + reg.num());
}

void X86_64::pop(Reg reg) {
    if (reg.is_ext())
        emit8(0x41);
    emit8(0x58 + reg.num());
}

void X86_64::call(int32_t rel) {
    emit8(0xE8);
    emit32(rel);
}

void X86_64::jmp(int32_t rel) {
    emit8(0xE9);
    emit32(rel);
}

void X86_64::jcc(Cond cc, int32_t rel) {
    emit8(0x0F);
    emit8(0x80 + static_cast<uint8_t>(cc));
    emit32(rel);
}

void X86_64::ret() {
    emit8(0xC3);
}

void X86_64::setcc(Cond cc, Reg dst) {
    emit_rex(dst);
    emit8(0x0F);
    emit8(0x90 + static_cast<uint8_t>(cc));
    uint8_t rm = dst.high_byte ? (dst.num() + 4) : dst.num();
    emit_modrm(0b11, 0, rm);
}

void X86_64::movzx(Reg dst, Reg src) {
    if (dst.size == OpSize::B16)
        emit8(0x66);
    emit_rex(dst, src);
    emit8(0x0F);
    emit8(src.size == OpSize::B8 ? 0xB6 : 0xB7);
    emit_modrm_reg(dst, src);
}

void X86_64::movsx(Reg dst, Reg src) {
    if (src.size == OpSize::B32) {
        emit_rex(dst, src);
        emit8(0x63);
        emit_modrm_reg(dst, src);
    } else {
        if (dst.size == OpSize::B16)
            emit8(0x66);
        emit_rex(dst, src);
        emit8(0x0F);
        emit8(src.size == OpSize::B8 ? 0xBE : 0xBF);
        emit_modrm_reg(dst, src);
    }
}

void X86_64::syscall() {
    emit8(0x0F);
    emit8(0x05);
}

void X86_64::movss(XMM dst, Mem src) {
    emit8(0xF3);
    uint8_t rex = 0x40;
    if (xmm_ext(dst))
        rex |= 0x04;
    if (src.base.is_ext())
        rex |= 0x01;
    if (rex != 0x40)
        emit8(rex);
    emit8(0x0F);
    emit8(0x10);
    emit_modrm_mem(xmm_num(dst), src);
}

void X86_64::movss(Mem dst, XMM src) {
    emit8(0xF3);
    uint8_t rex = 0x40;
    if (xmm_ext(src))
        rex |= 0x04;
    if (dst.base.is_ext())
        rex |= 0x01;
    if (rex != 0x40)
        emit8(rex);
    emit8(0x0F);
    emit8(0x11);
    emit_modrm_mem(xmm_num(src), dst);
}

void X86_64::movss(XMM dst, XMM src) {
    emit8(0xF3);
    emit_rex_xmm(dst, src);
    emit8(0x0F);
    emit8(0x10);
    emit_modrm(0b11, xmm_num(dst), xmm_num(src));
}

void X86_64::movd(XMM xmm, Reg r) {
    emit8(0x66);
    emit_rex_xmm(xmm, r);
    emit8(0x0F);
    emit8(0x6E);
    emit_modrm(0b11, xmm_num(xmm), r.num());
}

void X86_64::movd(Reg r, XMM xmm) {
    emit8(0x66);
    emit_rex_xmm(r, xmm);
    emit8(0x0F);
    emit8(0x7E);
    emit_modrm(0b11, xmm_num(xmm), r.num());
}

void X86_64::addss(XMM dst, XMM src) {
    emit8(0xF3);
    emit_rex_xmm(dst, src);
    emit8(0x0F);
    emit8(0x58);
    emit_modrm(0b11, xmm_num(dst), xmm_num(src));
}

void X86_64::subss(XMM dst, XMM src) {
    emit8(0xF3);
    emit_rex_xmm(dst, src);
    emit8(0x0F);
    emit8(0x5C);
    emit_modrm(0b11, xmm_num(dst), xmm_num(src));
}

void X86_64::mulss(XMM dst, XMM src) {
    emit8(0xF3);
    emit_rex_xmm(dst, src);
    emit8(0x0F);
    emit8(0x59);
    emit_modrm(0b11, xmm_num(dst), xmm_num(src));
}

void X86_64::divss(XMM dst, XMM src) {
    emit8(0xF3);
    emit_rex_xmm(dst, src);
    emit8(0x0F);
    emit8(0x5E);
    emit_modrm(0b11, xmm_num(dst), xmm_num(src));
}

void X86_64::xorps(XMM dst, XMM src) {
    emit_rex_xmm(dst, src);
    emit8(0x0F);
    emit8(0x57);
    emit_modrm(0b11, xmm_num(dst), xmm_num(src));
}

void X86_64::ucomiss(XMM a, XMM b) {
    emit_rex_xmm(a, b);
    emit8(0x0F);
    emit8(0x2E);
    emit_modrm(0b11, xmm_num(a), xmm_num(b));
}

void X86_64::cvtsi2ss(XMM xmm, Reg r) {
    emit8(0xF3);
    uint8_t rex = 0x40;
    if (r.size == OpSize::B64)
        rex |= 0x08;
    if (xmm_ext(xmm))
        rex |= 0x04;
    if (r.is_ext())
        rex |= 0x01;
    if (rex != 0x40)
        emit8(rex);
    emit8(0x0F);
    emit8(0x2A);
    emit_modrm(0b11, xmm_num(xmm), r.num());
}

void X86_64::cvttss2si(Reg r, XMM xmm) {
    emit8(0xF3);
    uint8_t rex = 0x40;
    if (r.size == OpSize::B64)
        rex |= 0x08;
    if (r.is_ext())
        rex |= 0x04;
    if (xmm_ext(xmm))
        rex |= 0x01;
    if (rex != 0x40)
        emit8(rex);
    emit8(0x0F);
    emit8(0x2C);
    emit_modrm(0b11, r.num(), xmm_num(xmm));
}

}  // namespace zex
