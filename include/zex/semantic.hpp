#ifndef ZEX_SEMANTIC_HPP
#define ZEX_SEMANTIC_HPP

#include <string>
#include <unordered_map>
#include <vector>

#include "zex/ast.hpp"
#include "zex/error.hpp"

namespace zex {

struct FunctionInfo {
    std::string name;
    Type return_type;
    size_t index;
};

struct VariableInfo {
    std::string name;
    Type type;
    int32_t stack_offset;
};

class SemanticAnalyzer {
   public:
    void analyze(Program& program);
    const FunctionInfo* get_function(const std::string& name) const;
    const std::vector<FunctionInfo>& get_functions() const {
        return functions_;
    }

   private:
    std::unordered_map<std::string, FunctionInfo> function_table_;
    std::vector<FunctionInfo> functions_;
    std::unordered_map<std::string, VariableInfo> local_variables_;
    int32_t next_stack_offset_;

    void register_functions(Program& program);
    void analyze_function(Function& func);
    void analyze_statement(Statement* stmt);
    void analyze_expression(Expression* expr);
};

}  // namespace zex

#endif  // ZEX_SEMANTIC_HPP
