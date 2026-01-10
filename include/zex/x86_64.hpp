#ifndef ZEX_X86_64_HPP
#define ZEX_X86_64_HPP

#include <cstdint>
#include <vector>

namespace zex {

// x86-64 Register encoding
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

    // 8-bit registers (for setcc)
    AL = 0,
    CL = 1,
    DL = 2,
    BL = 3,

    // XMM registers for SSE
    XMM0 = 0,
    XMM1 = 1,
    XMM2 = 2,
    XMM3 = 3,
    XMM4 = 4,
    XMM5 = 5,
    XMM6 = 6,
    XMM7 = 7
};

// Memory operand: [base + offset]
struct Mem {
    Reg base;
    int32_t offset;

    Mem(Reg b, int32_t off = 0) : base(b), offset(off) {}
};

// Condition codes for Jcc and SETcc
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

    // Aliases
    Z = E,
    NZ = NE,
    C = B,
    NC = AE
};

class X86_64 {
   public:
    X86_64() = default;

    // === MOV instructions ===
    void mov(Reg dst, Reg src);      // mov r64, r64
    void mov(Reg dst, int64_t imm);  // mov r64, imm64
    void mov(Reg dst, int32_t imm);  // mov r32, imm32 (zero-extends)
    void mov(Reg dst, Mem src);      // mov r64, [mem]
    void mov(Mem dst, Reg src);      // mov [mem], r64

    // === LEA instruction ===
    void lea(Reg dst, Mem src);  // lea r64, [mem]

    // === Arithmetic ===
    void add(Reg dst, Reg src);      // add r64, r64
    void add(Reg dst, int32_t imm);  // add r64, imm32
    void sub(Reg dst, Reg src);      // sub r64, r64
    void sub(Reg dst, int32_t imm);  // sub r64, imm32
    void imul(Reg dst, Reg src);     // imul r64, r64
    void idiv(Reg src);              // idiv r64
    void neg(Reg reg);               // neg r64
    void cqo();                      // sign-extend rax to rdx:rax

    // === Bitwise ===
    void xor_(Reg dst, Reg src);  // xor r64, r64
    void and_(Reg dst, Reg src);  // and r64, r64
    void or_(Reg dst, Reg src);   // or r64, r64
    void test(Reg a, Reg b);      // test r64, r64
    void cmp(Reg a, Reg b);       // cmp r64, r64

    // === Stack ===
    void push(Reg reg);  // push r64
    void pop(Reg reg);   // pop r64

    // === Control flow ===
    void call(int32_t rel);          // call rel32
    void jmp(int32_t rel);           // jmp rel32
    void jcc(Cond cc, int32_t rel);  // jcc rel32
    void ret();                      // ret

    // === Conditional set ===
    void setcc(Cond cc, Reg dst);   // setcc r8
    void movzx(Reg dst, Reg src8);  // movzx r64, r8

    // === System ===
    void syscall();  // syscall

    // === SSE (float) ===
    void movss(Reg xmm_dst, Mem src);      // movss xmm, [mem]
    void movss(Mem dst, Reg xmm_src);      // movss [mem], xmm
    void movss(Reg xmm_dst, Reg xmm_src);  // movss xmm, xmm
    void movd(Reg xmm, Reg r32);           // movd xmm, r32
    void movd_to_reg(Reg r32, Reg xmm);    // movd r32, xmm
    void addss(Reg dst, Reg src);          // addss xmm, xmm
    void subss(Reg dst, Reg src);          // subss xmm, xmm
    void mulss(Reg dst, Reg src);          // mulss xmm, xmm
    void divss(Reg dst, Reg src);          // divss xmm, xmm
    void xorps(Reg dst, Reg src);          // xorps xmm, xmm

    // === Utilities ===
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

    // Encoding helpers
    void emit8(uint8_t byte);
    void emit32(int32_t value);
    void emit64(int64_t value);

    // REX prefix: automatically handles R8-R15
    void emit_rex(bool w, Reg reg, Reg rm);
    void emit_rex(bool w, uint8_t reg, Reg rm);
    void emit_rex_single(bool w, Reg rm);

    // ModR/M encoding
    void emit_modrm(uint8_t mod, uint8_t reg, uint8_t rm);
    void emit_modrm_reg(Reg reg, Reg rm);   // mod=11 (register)
    void emit_modrm_mem(Reg reg, Mem mem);  // mod=01/10 (memory)

    // Helper to get register number (0-7)
    static uint8_t reg_num(Reg r) {
        return static_cast<uint8_t>(r) & 0x07;
    }
    static bool needs_rex_r(Reg r) {
        return static_cast<uint8_t>(r) >= 8;
    }
};

}  // namespace zex

#endif  // ZEX_X86_64_HPP
