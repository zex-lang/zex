// Zex semantic analyzer
// Validates program semantics and collects symbol information

#ifndef ZEX_SEMANTIC_HPP
#define ZEX_SEMANTIC_HPP

#include <string>
#include <unordered_map>
#include <vector>

#include "zex/ast.hpp"
#include "zex/error.hpp"

namespace zex {

// Information about a declared function
struct FunctionInfo {
    std::string name;
    std::vector<Type> param_types;
    Type return_type;
    size_t index;
};

// Information about a local variable or constant
struct VariableInfo {
    std::string name;
    Type type;
    int32_t stack_offset;
    bool is_const;
    int64_t const_value;
};

// Performs semantic analysis including name resolution and type checking
class SemanticAnalyzer {
   public:
    // Analyze entire program for semantic errors
    void analyze(Program& program);

    // Symbol table access
    const FunctionInfo* get_function(const std::string& name) const;
    const std::vector<FunctionInfo>& get_functions() const {
        return functions_;
    }
    const VariableInfo* get_variable(const std::string& name) const;

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
