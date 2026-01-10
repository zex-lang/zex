#ifndef ZEX_CODEGEN_HPP
#define ZEX_CODEGEN_HPP

#include <string>
#include <unordered_map>
#include <vector>

#include "zex/ast.hpp"
#include "zex/semantic.hpp"
#include "zex/x86_64.hpp"

namespace zex {

// Represents a call that needs to be patched with the actual function address
struct CallPatch {
    size_t call_site;    // Position of the rel32 in the code
    std::string target;  // Name of the function being called
};

class CodeGenerator {
   public:
    CodeGenerator(SemanticAnalyzer& semantic);

    // Generate code for entire program
    void generate(const Program& program);

    // Get the generated machine code
    const std::vector<uint8_t>& code() const {
        return emitter_.code();
    }

    // Get the offset of main() function (entry point)
    size_t entry_offset() const {
        return entry_offset_;
    }

   private:
    SemanticAnalyzer& semantic_;
    X86_64 emitter_;

    // Function name -> code offset mapping
    std::unordered_map<std::string, size_t> function_offsets_;

    // Calls that need patching
    std::vector<CallPatch> call_patches_;

    // Current function state
    std::unordered_map<std::string, int32_t> local_offsets_;
    int32_t stack_size_;

    // Entry point
    size_t entry_offset_;

    void generate_function(const Function& func);
    void generate_statement(const Statement* stmt);
    void generate_expression(const Expression* expr);

    // Resolve all forward references
    void resolve_calls();

    // Calculate stack size needed for local variables (16-byte aligned)
    int32_t calculate_stack_size(const Function& func);
};

}  // namespace zex

#endif  // ZEX_CODEGEN_HPP
