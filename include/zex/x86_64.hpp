#ifndef ZEX_X86_64_HPP
#define ZEX_X86_64_HPP

#include <cstdint>
#include <vector>

namespace zex {

// Low-level x86-64 instruction encoder
// Generates raw machine code bytes directly
class X86_64 {
   public:
    X86_64() = default;

    // Stack frame management
    void emit_push_rbp();     // push rbp
    void emit_mov_rbp_rsp();  // mov rbp, rsp
    void emit_pop_rbp();      // pop rbp
    void emit_ret();          // ret

    // Stack pointer manipulation
    void emit_sub_rsp_imm8(uint8_t imm);  // sub rsp, imm8
    void emit_add_rsp_imm8(uint8_t imm);  // add rsp, imm8

    // Move operations
    void emit_mov_rax_imm64(int64_t imm);          // mov rax, imm64
    void emit_mov_rax_rbp_offset(int32_t offset);  // mov rax, [rbp+offset]
    void emit_mov_rbp_offset_rax(int32_t offset);  // mov [rbp+offset], rax

    // Function calls
    void emit_call_rel32(int32_t offset);  // call rel32

    // System calls (for exit)
    void emit_mov_rax_imm32(int32_t imm);  // mov eax, imm32 (zero-extends to rax)
    void emit_mov_rdi_rax();               // mov rdi, rax
    void emit_syscall();                   // syscall

    // Returns current code offset (for patching later)
    size_t current_offset() const;

    // Patch a relative 32-bit offset at given position
    // Used for forward references in function calls
    void patch_rel32(size_t pos, int32_t value);

    // Get generated machine code
    const std::vector<uint8_t>& code() const {
        return code_;
    }
    std::vector<uint8_t>& code() {
        return code_;
    }

   private:
    std::vector<uint8_t> code_;

    // Helper to emit raw bytes
    void emit8(uint8_t byte);
    void emit32(int32_t value);
    void emit64(int64_t value);
};

}  // namespace zex

#endif  // ZEX_X86_64_HPP
