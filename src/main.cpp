// Zex compiler command line interface

#include <getopt.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>

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

void print_usage() {
    std::cerr << "usage: zex <command> [options] <file>\n\n";
    std::cerr << "commands:\n";
    std::cerr << "  build    compile source file to executable\n";
    std::cerr << "  run      compile and execute in memory\n\n";
    std::cerr << "build options:\n";
    std::cerr << "  -o <file>    output file (default: input name without extension)\n";
}

std::string read_file(const std::string& path) {
    std::ifstream file(path);
    if (!file) {
        throw zex::CompileError(zex::ErrorCode::FILE_NOT_FOUND, {}, "'" + path + "'");
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
        std::cerr << " " << e.context;
    }
    std::cerr << "\n";
}

std::string get_default_output(const std::string& input) {
    size_t dot = input.rfind('.');
    if (dot != std::string::npos) {
        return input.substr(0, dot);
    }
    return input + ".out";
}

std::vector<uint8_t> compile(const std::string& source) {
    zex::Lexer lexer(source);
    auto tokens = lexer.tokenize();

    zex::Parser parser(std::move(tokens));
    auto program = parser.parse();

    zex::SemanticAnalyzer semantic;
    semantic.analyze(*program);

    zex::CodeGenerator codegen(semantic);
    codegen.generate(*program);

    zex::ELFWriter elf;
    return elf.build(codegen.code(), codegen.entry_offset());
}

int cmd_build(int argc, char* argv[]) {
    std::string output_file;
    std::string input_file;

    optind = 2;
    int opt;
    while ((opt = getopt(argc, argv, "o:")) != -1) {
        switch (opt) {
            case 'o':
                output_file = optarg;
                break;
            default:
                print_usage();
                return 1;
        }
    }

    if (optind >= argc) {
        std::cerr << "error: no input file\n";
        return 1;
    }
    input_file = argv[optind];

    if (output_file.empty()) {
        output_file = get_default_output(input_file);
    }

    try {
        std::string source = read_file(input_file);
        auto binary = compile(source);

        std::ofstream out(output_file, std::ios::binary);
        if (!out) {
            std::cerr << "error: cannot write to '" << output_file << "'\n";
            return 1;
        }
        out.write(reinterpret_cast<const char*>(binary.data()), binary.size());
        out.close();
        chmod(output_file.c_str(), 0755);

        return 0;
    } catch (const zex::CompileError& e) {
        print_error(input_file, e);
        return 1;
    }
}

int cmd_run(int argc, char* argv[]) {
    if (argc < 3) {
        std::cerr << "error: no input file\n";
        return 1;
    }
    std::string input_file = argv[2];

    try {
        std::string source = read_file(input_file);
        auto binary = compile(source);

        // Create anonymous file in memory
        int fd = memfd_create("zex", 0);
        if (fd == -1) {
            std::cerr << "error: memfd_create failed\n";
            return 1;
        }

        // Write ELF binary to memfd
        if (write(fd, binary.data(), binary.size()) != static_cast<ssize_t>(binary.size())) {
            close(fd);
            std::cerr << "error: write failed\n";
            return 1;
        }

        // Fork and execute ELF from memfd
        pid_t pid = fork();
        if (pid == 0) {
            char* const args[] = {nullptr};
            char* const env[] = {nullptr};
            fexecve(fd, args, env);
            _exit(127);
        } else if (pid > 0) {
            close(fd);
            int status;
            waitpid(pid, &status, 0);
            if (WIFEXITED(status)) {
                return WEXITSTATUS(status);
            }
            return 1;
        } else {
            close(fd);
            std::cerr << "error: fork failed\n";
            return 1;
        }
    } catch (const zex::CompileError& e) {
        print_error(input_file, e);
        return 1;
    }
}

}  // anonymous namespace

int main(int argc, char* argv[]) {
    if (argc < 2) {
        print_usage();
        return 1;
    }

    std::string command = argv[1];

    if (command == "build") {
        return cmd_build(argc, argv);
    } else if (command == "run") {
        return cmd_run(argc, argv);
    } else if (command == "-h" || command == "--help") {
        print_usage();
        return 0;
    } else {
        std::cerr << "error: unknown command '" << command << "'\n";
        print_usage();
        return 1;
    }
}
