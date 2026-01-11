// Zex x86-64 machine code emitter
// Generates raw x86-64 instructions with proper encoding

#ifndef ZEX_X86_64_HPP
#define ZEX_X86_64_HPP

#include <cstdint>
#include <vector>

namespace zex {

enum class GPR : uint8_t {
    RAX = 0,
    RCX = 1,
    RDX = 2,
    RBX = 3,
    RSP = 4,
    RBP = 5,
    RSI = 6,
    RDI = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15
};

enum class XMM : uint8_t {
    XMM0 = 0,
    XMM1 = 1,
    XMM2 = 2,
    XMM3 = 3,
    XMM4 = 4,
    XMM5 = 5,
    XMM6 = 6,
    XMM7 = 7,
    XMM8 = 8,
    XMM9 = 9,
    XMM10 = 10,
    XMM11 = 11,
    XMM12 = 12,
    XMM13 = 13,
    XMM14 = 14,
    XMM15 = 15
};

enum class OpSize : uint8_t { B8 = 1, B16 = 2, B32 = 4, B64 = 8 };

struct Reg {
    GPR gpr;
    OpSize size;
    bool high_byte;

    Reg(GPR g, OpSize s, bool hi = false) : gpr(g), size(s), high_byte(hi) {}

    static Reg rax() {
        return {GPR::RAX, OpSize::B64};
    }
    static Reg rcx() {
        return {GPR::RCX, OpSize::B64};
    }
    static Reg rdx() {
        return {GPR::RDX, OpSize::B64};
    }
    static Reg rbx() {
        return {GPR::RBX, OpSize::B64};
    }
    static Reg rsp() {
        return {GPR::RSP, OpSize::B64};
    }
    static Reg rbp() {
        return {GPR::RBP, OpSize::B64};
    }
    static Reg rsi() {
        return {GPR::RSI, OpSize::B64};
    }
    static Reg rdi() {
        return {GPR::RDI, OpSize::B64};
    }
    static Reg r8() {
        return {GPR::R8, OpSize::B64};
    }
    static Reg r9() {
        return {GPR::R9, OpSize::B64};
    }
    static Reg r10() {
        return {GPR::R10, OpSize::B64};
    }
    static Reg r11() {
        return {GPR::R11, OpSize::B64};
    }
    static Reg r12() {
        return {GPR::R12, OpSize::B64};
    }
    static Reg r13() {
        return {GPR::R13, OpSize::B64};
    }
    static Reg r14() {
        return {GPR::R14, OpSize::B64};
    }
    static Reg r15() {
        return {GPR::R15, OpSize::B64};
    }

    static Reg eax() {
        return {GPR::RAX, OpSize::B32};
    }
    static Reg ecx() {
        return {GPR::RCX, OpSize::B32};
    }
    static Reg edx() {
        return {GPR::RDX, OpSize::B32};
    }
    static Reg ebx() {
        return {GPR::RBX, OpSize::B32};
    }
    static Reg esp() {
        return {GPR::RSP, OpSize::B32};
    }
    static Reg ebp() {
        return {GPR::RBP, OpSize::B32};
    }
    static Reg esi() {
        return {GPR::RSI, OpSize::B32};
    }
    static Reg edi() {
        return {GPR::RDI, OpSize::B32};
    }
    static Reg r8d() {
        return {GPR::R8, OpSize::B32};
    }
    static Reg r9d() {
        return {GPR::R9, OpSize::B32};
    }
    static Reg r10d() {
        return {GPR::R10, OpSize::B32};
    }
    static Reg r11d() {
        return {GPR::R11, OpSize::B32};
    }
    static Reg r12d() {
        return {GPR::R12, OpSize::B32};
    }
    static Reg r13d() {
        return {GPR::R13, OpSize::B32};
    }
    static Reg r14d() {
        return {GPR::R14, OpSize::B32};
    }
    static Reg r15d() {
        return {GPR::R15, OpSize::B32};
    }

    static Reg ax() {
        return {GPR::RAX, OpSize::B16};
    }
    static Reg cx() {
        return {GPR::RCX, OpSize::B16};
    }
    static Reg dx() {
        return {GPR::RDX, OpSize::B16};
    }
    static Reg bx() {
        return {GPR::RBX, OpSize::B16};
    }
    static Reg sp() {
        return {GPR::RSP, OpSize::B16};
    }
    static Reg bp() {
        return {GPR::RBP, OpSize::B16};
    }
    static Reg si() {
        return {GPR::RSI, OpSize::B16};
    }
    static Reg di() {
        return {GPR::RDI, OpSize::B16};
    }

    static Reg al() {
        return {GPR::RAX, OpSize::B8};
    }
    static Reg cl() {
        return {GPR::RCX, OpSize::B8};
    }
    static Reg dl() {
        return {GPR::RDX, OpSize::B8};
    }
    static Reg bl() {
        return {GPR::RBX, OpSize::B8};
    }
    static Reg spl() {
        return {GPR::RSP, OpSize::B8};
    }
    static Reg bpl() {
        return {GPR::RBP, OpSize::B8};
    }
    static Reg sil() {
        return {GPR::RSI, OpSize::B8};
    }
    static Reg dil() {
        return {GPR::RDI, OpSize::B8};
    }
    static Reg r8b() {
        return {GPR::R8, OpSize::B8};
    }
    static Reg r9b() {
        return {GPR::R9, OpSize::B8};
    }
    static Reg r10b() {
        return {GPR::R10, OpSize::B8};
    }
    static Reg r11b() {
        return {GPR::R11, OpSize::B8};
    }
    static Reg r12b() {
        return {GPR::R12, OpSize::B8};
    }
    static Reg r13b() {
        return {GPR::R13, OpSize::B8};
    }
    static Reg r14b() {
        return {GPR::R14, OpSize::B8};
    }
    static Reg r15b() {
        return {GPR::R15, OpSize::B8};
    }

    static Reg ah() {
        return {GPR::RAX, OpSize::B8, true};
    }
    static Reg ch() {
        return {GPR::RCX, OpSize::B8, true};
    }
    static Reg dh() {
        return {GPR::RDX, OpSize::B8, true};
    }
    static Reg bh() {
        return {GPR::RBX, OpSize::B8, true};
    }

    uint8_t num() const {
        return static_cast<uint8_t>(gpr) & 0x07;
    }
    bool is_ext() const {
        return static_cast<uint8_t>(gpr) >= 8;
    }
};

struct Mem {
    Reg base;
    int32_t offset;

    Mem(Reg b, int32_t off = 0) : base(b), offset(off) {}
};

enum class Cond : uint8_t {
    O = 0,
    NO = 1,
    B = 2,
    AE = 3,
    E = 4,
    NE = 5,
    BE = 6,
    A = 7,
    S = 8,
    NS = 9,
    P = 10,
    NP = 11,
    L = 12,
    GE = 13,
    LE = 14,
    G = 15,
    Z = E,
    NZ = NE,
    C = B,
    NC = AE
};

class X86_64 {
   public:
    X86_64() = default;

    void mov(Reg dst, Reg src);
    void mov(Reg dst, int64_t imm);
    void mov(Reg dst, int32_t imm);
    void mov(Reg dst, Mem src);
    void mov(Mem dst, Reg src);

    void lea(Reg dst, Mem src);

    void add(Reg dst, Reg src);
    void add(Reg dst, int32_t imm);
    void sub(Reg dst, Reg src);
    void sub(Reg dst, int32_t imm);
    void imul(Reg dst, Reg src);
    void idiv(Reg src);
    void neg(Reg reg);
    void cqo();
    void cdq();

    void xor_(Reg dst, Reg src);
    void and_(Reg dst, Reg src);
    void or_(Reg dst, Reg src);
    void test(Reg a, Reg b);
    void cmp(Reg a, Reg b);
    void cmp(Reg a, int32_t imm);

    void push(Reg reg);
    void pop(Reg reg);

    void call(int32_t rel);
    void jmp(int32_t rel);
    void jcc(Cond cc, int32_t rel);
    void ret();

    void setcc(Cond cc, Reg dst);
    void movzx(Reg dst, Reg src);
    void movsx(Reg dst, Reg src);

    void syscall();

    void movss(XMM dst, Mem src);
    void movss(Mem dst, XMM src);
    void movss(XMM dst, XMM src);
    void movd(XMM xmm, Reg r);
    void movd(Reg r, XMM xmm);
    void addss(XMM dst, XMM src);
    void subss(XMM dst, XMM src);
    void mulss(XMM dst, XMM src);
    void divss(XMM dst, XMM src);
    void xorps(XMM dst, XMM src);
    void ucomiss(XMM a, XMM b);
    void cvtsi2ss(XMM xmm, Reg r);
    void cvttss2si(Reg r, XMM xmm);

    size_t current_offset() const {
        return code_.size();
    }
    void patch_rel32(size_t pos, int32_t value);
    const std::vector<uint8_t>& code() const {
        return code_;
    }
    std::vector<uint8_t>& code() {
        return code_;
    }

   private:
    std::vector<uint8_t> code_;

    void emit8(uint8_t byte);
    void emit16(uint16_t value);
    void emit32(int32_t value);
    void emit64(int64_t value);

    void emit_rex(Reg reg, Reg rm);
    void emit_rex(Reg rm);
    void emit_rex_xmm(XMM reg, Reg rm);
    void emit_rex_xmm(Reg reg, XMM rm);
    void emit_rex_xmm(XMM reg, XMM rm);

    void emit_modrm(uint8_t mod, uint8_t reg, uint8_t rm);
    void emit_modrm_reg(Reg reg, Reg rm);
    void emit_modrm_mem(Reg reg, Mem mem);
    void emit_modrm_mem(uint8_t reg_num, Mem mem);

    static uint8_t xmm_num(XMM x) {
        return static_cast<uint8_t>(x) & 0x07;
    }
    static bool xmm_ext(XMM x) {
        return static_cast<uint8_t>(x) >= 8;
    }
};

}  // namespace zex

#endif  // ZEX_X86_64_HPP
