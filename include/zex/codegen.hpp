// Zex code generator
// Translates AST into x86_64 machine code

#ifndef ZEX_CODEGEN_HPP
#define ZEX_CODEGEN_HPP

#include <string>
#include <unordered_map>
#include <vector>

#include "zex/ast.hpp"
#include "zex/semantic.hpp"
#include "zex/x86_64.hpp"

namespace zex {

// Pending call instruction that needs address patching
struct CallPatch {
    size_t call_site;
    std::string target;
};

// Local variable or constant in current function scope
struct LocalVar {
    int32_t stack_offset;
    bool is_const;
    int64_t const_value;
    Type type;
};

// Interned string literal with its code offset
struct StringLiteralData {
    std::string value;
    size_t offset;
};

// Generates x86_64 machine code from the AST
class CodeGenerator {
   public:
    CodeGenerator(SemanticAnalyzer& semantic);

    // Generate code for entire program
    void generate(const Program& program);

    // Access generated machine code
    const std::vector<uint8_t>& code() const {
        return emitter_.code();
    }

    // Entry point offset in generated code
    size_t entry_offset() const {
        return entry_offset_;
    }

   private:
    SemanticAnalyzer& semantic_;
    X86_64 emitter_;

    std::unordered_map<std::string, size_t> function_offsets_;
    std::vector<CallPatch> call_patches_;
    std::vector<StringLiteralData> string_literals_;

    std::unordered_map<std::string, LocalVar> locals_;
    int32_t stack_size_;
    size_t entry_offset_;

    void generate_function(const Function& func);
    void generate_statement(const Statement* stmt);
    void generate_expression(const Expression* expr);
    void resolve_calls();
    int32_t calculate_stack_size(const Function& func);
    size_t add_string_literal(const std::string& str);
};

}  // namespace zex

#endif  // ZEX_CODEGEN_HPP
