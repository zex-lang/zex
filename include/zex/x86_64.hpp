// Zex x86_64 machine code emitter
// Generates raw x86_64 instructions with proper encoding

#ifndef ZEX_X86_64_HPP
#define ZEX_X86_64_HPP

#include <cstdint>
#include <vector>

namespace zex {

// General purpose and extended register encoding values
enum class Reg : uint8_t {
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
    R15 = 15,

    // 8 bit registers for setcc instructions
    AL = 0,
    CL = 1,
    DL = 2,
    BL = 3,

    // SSE registers for floating point
    XMM0 = 0,
    XMM1 = 1,
    XMM2 = 2,
    XMM3 = 3,
    XMM4 = 4,
    XMM5 = 5,
    XMM6 = 6,
    XMM7 = 7
};

// Memory operand with base register and displacement
struct Mem {
    Reg base;
    int32_t offset;

    Mem(Reg b, int32_t off = 0) : base(b), offset(off) {}
};

// Condition codes for conditional jumps and set instructions
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

    // Commonly used aliases
    Z = E,
    NZ = NE,
    C = B,
    NC = AE
};

// Machine code emitter for x86_64 architecture
class X86_64 {
   public:
    X86_64() = default;

    // Data movement
    void mov(Reg dst, Reg src);
    void mov(Reg dst, int64_t imm);
    void mov(Reg dst, int32_t imm);
    void mov(Reg dst, Mem src);
    void mov(Mem dst, Reg src);

    // Load effective address
    void lea(Reg dst, Mem src);

    // Integer arithmetic
    void add(Reg dst, Reg src);
    void add(Reg dst, int32_t imm);
    void sub(Reg dst, Reg src);
    void sub(Reg dst, int32_t imm);
    void imul(Reg dst, Reg src);
    void idiv(Reg src);
    void neg(Reg reg);
    void cqo();

    // Bitwise operations
    void xor_(Reg dst, Reg src);
    void and_(Reg dst, Reg src);
    void or_(Reg dst, Reg src);
    void test(Reg a, Reg b);
    void cmp(Reg a, Reg b);
    void cmp_imm(Reg a, int32_t imm);

    // Stack operations
    void push(Reg reg);
    void pop(Reg reg);

    // Control flow
    void call(int32_t rel);
    void jmp(int32_t rel);
    void jcc(Cond cc, int32_t rel);
    void ret();

    // Conditional byte set
    void setcc(Cond cc, Reg dst);
    void movzx(Reg dst, Reg src8);
    void movsx(Reg dst, Reg src);  // sign extend

    // System calls
    void syscall();

    // SSE floating point operations
    void movss(Reg xmm_dst, Mem src);
    void movss(Mem dst, Reg xmm_src);
    void movss(Reg xmm_dst, Reg xmm_src);
    void movd(Reg xmm, Reg r32);
    void movd_to_reg(Reg r32, Reg xmm);
    void addss(Reg dst, Reg src);
    void subss(Reg dst, Reg src);
    void mulss(Reg dst, Reg src);
    void divss(Reg dst, Reg src);
    void xorps(Reg dst, Reg src);
    void cvtsi2ss(Reg xmm, Reg r64);   // int to float
    void cvttss2si(Reg r64, Reg xmm);  // float to int

    // Utilities for patching and code access
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

    // Low level byte emission
    void emit8(uint8_t byte);
    void emit32(int32_t value);
    void emit64(int64_t value);

    // REX prefix generation for 64 bit and extended registers
    void emit_rex(bool w, Reg reg, Reg rm);
    void emit_rex(bool w, uint8_t reg, Reg rm);
    void emit_rex_single(bool w, Reg rm);

    // ModR/M byte encoding
    void emit_modrm(uint8_t mod, uint8_t reg, uint8_t rm);
    void emit_modrm_reg(Reg reg, Reg rm);
    void emit_modrm_mem(Reg reg, Mem mem);

    // Register number extraction
    static uint8_t reg_num(Reg r) {
        return static_cast<uint8_t>(r) & 0x07;
    }
    static bool needs_rex_r(Reg r) {
        return static_cast<uint8_t>(r) >= 8;
    }
};

}  // namespace zex

#endif  // ZEX_X86_64_HPP
