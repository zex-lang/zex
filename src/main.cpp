#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>

#include "zex/codegen.hpp"
#include "zex/elf.hpp"
#include "zex/error.hpp"
#include "zex/lexer.hpp"
#include "zex/parser.hpp"
#include "zex/semantic.hpp"

namespace {

void print_usage(const char* program) {
    std::cerr << "usage: " << program << " <input> -o <output>\n";
}

std::string read_file(const std::string& path) {
    std::ifstream file(path);
    if (!file) {
        throw zex::CompileError(zex::ErrorCode::FILE_NOT_FOUND, {}, path);
    }
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

void print_error(const std::string& filename, const zex::CompileError& e) {
    std::cerr << filename;
    if (e.location.line > 0) {
        std::cerr << ":" << e.location.line << ":" << e.location.column;
    }
    std::cerr << ": " << e.what();
    if (!e.context.empty()) {
        std::cerr << " '" << e.context << "'";
    }
    std::cerr << "\n";
}

}  // anonymous namespace

int main(int argc, char* argv[]) {
    if (argc < 4) {
        print_usage(argv[0]);
        return 1;
    }

    std::string input_file;
    std::string output_file;

    for (int i = 1; i < argc; i++) {
        if (std::strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            output_file = argv[++i];
        } else if (input_file.empty()) {
            input_file = argv[i];
        }
    }

    if (input_file.empty() || output_file.empty()) {
        print_usage(argv[0]);
        return 1;
    }

    try {
        std::string source = read_file(input_file);

        zex::Lexer lexer(source);
        auto tokens = lexer.tokenize();

        zex::Parser parser(std::move(tokens));
        auto program = parser.parse();

        zex::SemanticAnalyzer semantic;
        semantic.analyze(*program);

        zex::CodeGenerator codegen(semantic);
        codegen.generate(*program);

        zex::ELFWriter elf;
        elf.write(output_file, codegen.code(), codegen.entry_offset());

        return 0;

    } catch (const zex::CompileError& e) {
        print_error(input_file, e);
        return 1;
    } catch (const std::exception& e) {
        std::cerr << "error: " << e.what() << "\n";
        return 1;
    }
}
