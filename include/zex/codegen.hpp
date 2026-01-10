#ifndef ZEX_CODEGEN_HPP
#define ZEX_CODEGEN_HPP

#include <string>
#include <unordered_map>
#include <vector>

#include "zex/ast.hpp"
#include "zex/semantic.hpp"
#include "zex/x86_64.hpp"

namespace zex {

struct CallPatch {
    size_t call_site;
    std::string target;
};

struct LocalVar {
    int32_t stack_offset;
    bool is_const;
    int64_t const_value;
};

class CodeGenerator {
   public:
    CodeGenerator(SemanticAnalyzer& semantic);

    void generate(const Program& program);

    const std::vector<uint8_t>& code() const {
        return emitter_.code();
    }

    size_t entry_offset() const {
        return entry_offset_;
    }

   private:
    SemanticAnalyzer& semantic_;
    X86_64 emitter_;

    std::unordered_map<std::string, size_t> function_offsets_;
    std::vector<CallPatch> call_patches_;

    std::unordered_map<std::string, LocalVar> locals_;
    int32_t stack_size_;
    size_t entry_offset_;

    void generate_function(const Function& func);
    void generate_statement(const Statement* stmt);
    void generate_expression(const Expression* expr);
    void resolve_calls();
    int32_t calculate_stack_size(const Function& func);
};

}  // namespace zex

#endif  // ZEX_CODEGEN_HPP
