#include "zex/codegen.hpp"

namespace zex {

CodeGenerator::CodeGenerator(SemanticAnalyzer& semantic)
    : semantic_(semantic), stack_size_(0), entry_offset_(0) {}

void CodeGenerator::generate(const Program& program) {
    // First, generate _start wrapper that will:
    // 1. Call main()
    // 2. Move return value to rdi
    // 3. Call exit syscall
    //
    // _start:
    //     call main
    //     mov rdi, rax      ; exit code = return value of main
    //     mov eax, 60       ; syscall number for exit
    //     syscall

    // Record _start offset (this is our entry point)
    entry_offset_ = emitter_.current_offset();

    // call main (placeholder, will be patched)
    emitter_.emit_call_rel32(0);
    size_t main_call_site = emitter_.current_offset() - 4;

    // mov rdi, rax (exit code)
    emitter_.emit_mov_rdi_rax();

    // mov eax, 60 (exit syscall)
    emitter_.emit_mov_rax_imm32(60);

    // syscall
    emitter_.emit_syscall();

    // Generate code for each function
    for (const auto& func : program.functions) {
        generate_function(*func);
    }

    // Resolve all forward references
    resolve_calls();

    // Patch the call to main
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

    for (const auto& stmt : func.body) {
        if (dynamic_cast<VarDecl*>(stmt.get())) {
            size += 8;  // Each variable is 8 bytes
        }
    }

    // Align to 16 bytes
    if (size % 16 != 0) {
        size = ((size / 16) + 1) * 16;
    }

    return size;
}

void CodeGenerator::generate_function(const Function& func) {
    // Record function offset
    function_offsets_[func.name] = emitter_.current_offset();

    // Reset local variable tracking
    local_offsets_.clear();
    stack_size_ = calculate_stack_size(func);

    // Function prologue
    emitter_.emit_push_rbp();
    emitter_.emit_mov_rbp_rsp();

    if (stack_size_ > 0) {
        emitter_.emit_sub_rsp_imm8(static_cast<uint8_t>(stack_size_));
    }

    // Track variable offsets
    int32_t offset = -8;
    for (const auto& stmt : func.body) {
        if (auto* var = dynamic_cast<VarDecl*>(stmt.get())) {
            local_offsets_[var->name] = offset;
            offset -= 8;
        }
    }

    // Generate body
    for (const auto& stmt : func.body) {
        generate_statement(stmt.get());
    }
}

void CodeGenerator::generate_statement(const Statement* stmt) {
    if (auto* var_decl = dynamic_cast<const VarDecl*>(stmt)) {
        // Evaluate initializer (result in RAX)
        generate_expression(var_decl->initializer.get());

        // Store to stack location
        int32_t offset = local_offsets_[var_decl->name];
        emitter_.emit_mov_rbp_offset_rax(offset);
    } else if (auto* ret = dynamic_cast<const ReturnStmt*>(stmt)) {
        // Evaluate return value (result in RAX)
        generate_expression(ret->value.get());

        // Function epilogue
        if (stack_size_ > 0) {
            emitter_.emit_add_rsp_imm8(static_cast<uint8_t>(stack_size_));
        }
        emitter_.emit_pop_rbp();
        emitter_.emit_ret();
    }
}

void CodeGenerator::generate_expression(const Expression* expr) {
    if (auto* lit = dynamic_cast<const IntLiteral*>(expr)) {
        emitter_.emit_mov_rax_imm64(lit->value);
    } else if (auto* ident = dynamic_cast<const Identifier*>(expr)) {
        int32_t offset = local_offsets_[ident->name];
        emitter_.emit_mov_rax_rbp_offset(offset);
    } else if (auto* call = dynamic_cast<const CallExpr*>(expr)) {
        // Record call site for later patching
        emitter_.emit_call_rel32(0);  // Placeholder offset

        CallPatch patch;
        patch.call_site = emitter_.current_offset() - 4;  // Position of the rel32
        patch.target = call->callee;
        call_patches_.push_back(patch);
    }
}

void CodeGenerator::resolve_calls() {
    for (const auto& patch : call_patches_) {
        auto it = function_offsets_.find(patch.target);
        if (it != function_offsets_.end()) {
            // Calculate relative offset
            // rel32 = target - (call_site + 4)
            // call_site is the position of rel32, so call_site + 4 is after the call instruction
            size_t target = it->second;
            size_t after_call = patch.call_site + 4;
            int32_t rel = static_cast<int32_t>(target - after_call);

            emitter_.patch_rel32(patch.call_site, rel);
        }
    }
}

}  // namespace zex
