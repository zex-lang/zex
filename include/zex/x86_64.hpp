#ifndef ZEX_X86_64_HPP
#define ZEX_X86_64_HPP

#include <cstdint>
#include <vector>

namespace zex {

class X86_64 {
   public:
    X86_64() = default;

    void emit_push_rbp();
    void emit_mov_rbp_rsp();
    void emit_pop_rbp();
    void emit_ret();

    void emit_sub_rsp_imm8(uint8_t imm);
    void emit_add_rsp_imm8(uint8_t imm);

    void emit_mov_rax_imm64(int64_t imm);
    void emit_mov_rax_rbp_offset(int32_t offset);
    void emit_mov_rbp_offset_rax(int32_t offset);

    void emit_call_rel32(int32_t offset);

    void emit_mov_rax_imm32(int32_t imm);
    void emit_mov_rdi_rax();
    void emit_mov_rsi_rax();
    void emit_mov_rdx_rax();
    void emit_mov_rcx_rax();
    void emit_mov_r8_rax();
    void emit_mov_r9_rax();

    void emit_mov_rbp_offset_rdi(int32_t offset);
    void emit_mov_rbp_offset_rsi(int32_t offset);
    void emit_mov_rbp_offset_rdx(int32_t offset);
    void emit_mov_rbp_offset_rcx(int32_t offset);
    void emit_mov_rbp_offset_r8(int32_t offset);
    void emit_mov_rbp_offset_r9(int32_t offset);

    void emit_syscall();

    void emit_push_rax();
    void emit_pop_rcx();
    void emit_pop_rdi();
    void emit_pop_rsi();
    void emit_pop_rdx();
    void emit_pop_r8();
    void emit_pop_r9();
    void emit_add_rax_rcx();
    void emit_sub_rax_rcx();
    void emit_imul_rax_rcx();
    void emit_cqo();
    void emit_idiv_rcx();
    void emit_mov_rax_rdx();

    void emit_neg_rax();
    void emit_cmp_rax_rcx();
    void emit_sete_al();
    void emit_setne_al();
    void emit_setl_al();
    void emit_setg_al();
    void emit_setle_al();
    void emit_setge_al();
    void emit_movzx_rax_al();
    void emit_test_rax_rax();
    void emit_setz_al();

    void emit_xor_rax_rax();
    void emit_test_rcx_rcx();

    void emit_jz_rel32(int32_t offset);
    void emit_jmp_rel32(int32_t offset);

    size_t current_offset() const;
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
    void emit32(int32_t value);
    void emit64(int64_t value);
};

}  // namespace zex

#endif  // ZEX_X86_64_HPP
