// Zex error handling
// Compile time error types and messages

#ifndef ZEX_ERROR_HPP
#define ZEX_ERROR_HPP

#include <cstdint>
#include <stdexcept>
#include <string>

namespace zex {

// All compiler error codes
enum class ErrorCode {
    // File errors
    FILE_NOT_FOUND,
    FILE_WRITE_FAILED,

    // Lexer errors
    UNEXPECTED_CHARACTER,

    // Parser errors
    UNEXPECTED_TOKEN,
    EXPECTED_IDENTIFIER,
    EXPECTED_TYPE,
    EXPECTED_EXPRESSION,
    EXPECTED_STATEMENT,

    // Semantic errors
    DUPLICATE_FUNCTION,
    DUPLICATE_VARIABLE,
    UNDEFINED_FUNCTION,
    UNDEFINED_VARIABLE,
    NO_MAIN_FUNCTION,
    UNKNOWN_EXPRESSION
};

// Location in source code for error reporting
struct SourceLocation {
    uint32_t line = 0;
    uint32_t column = 0;

    SourceLocation() = default;
    SourceLocation(uint32_t ln, uint32_t col) : line(ln), column(col) {}
};

// Exception thrown for all compilation errors
class CompileError : public std::exception {
   public:
    ErrorCode code;
    SourceLocation location;
    std::string context;

    CompileError(ErrorCode c, SourceLocation loc, std::string ctx = "")
        : code(c), location(loc), context(std::move(ctx)) {}

    const char* what() const noexcept override {
        return get_message(code);
    }

    static const char* get_message(ErrorCode code) {
        switch (code) {
            case ErrorCode::FILE_NOT_FOUND:
                return "file not found";
            case ErrorCode::FILE_WRITE_FAILED:
                return "failed to write file";
            case ErrorCode::UNEXPECTED_CHARACTER:
                return "unexpected character";
            case ErrorCode::UNEXPECTED_TOKEN:
                return "unexpected token";
            case ErrorCode::EXPECTED_IDENTIFIER:
                return "expected identifier";
            case ErrorCode::EXPECTED_TYPE:
                return "expected type";
            case ErrorCode::EXPECTED_EXPRESSION:
                return "expected expression";
            case ErrorCode::EXPECTED_STATEMENT:
                return "expected statement";
            case ErrorCode::DUPLICATE_FUNCTION:
                return "duplicate function";
            case ErrorCode::DUPLICATE_VARIABLE:
                return "duplicate variable";
            case ErrorCode::UNDEFINED_FUNCTION:
                return "undefined function";
            case ErrorCode::UNDEFINED_VARIABLE:
                return "undefined variable";
            case ErrorCode::NO_MAIN_FUNCTION:
                return "no main function defined";
            case ErrorCode::UNKNOWN_EXPRESSION:
                return "unknown expression type";
        }
        return "unknown error";
    }
};

}  // namespace zex

#endif  // ZEX_ERROR_HPP
