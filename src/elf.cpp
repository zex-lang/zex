#include "zex/elf.hpp"

#include <sys/stat.h>

#include <fstream>

#include "zex/error.hpp"

namespace zex {

void ELFWriter::write16(std::vector<uint8_t>& out, uint16_t val) {
    out.push_back(static_cast<uint8_t>(val & 0xFF));
    out.push_back(static_cast<uint8_t>((val >> 8) & 0xFF));
}

void ELFWriter::write32(std::vector<uint8_t>& out, uint32_t val) {
    out.push_back(static_cast<uint8_t>(val & 0xFF));
    out.push_back(static_cast<uint8_t>((val >> 8) & 0xFF));
    out.push_back(static_cast<uint8_t>((val >> 16) & 0xFF));
    out.push_back(static_cast<uint8_t>((val >> 24) & 0xFF));
}

void ELFWriter::write64(std::vector<uint8_t>& out, uint64_t val) {
    for (int i = 0; i < 8; i++) {
        out.push_back(static_cast<uint8_t>((val >> (i * 8)) & 0xFF));
    }
}

void ELFWriter::write_elf_header(std::vector<uint8_t>& output, uint64_t entry_addr) {
    output.push_back(ELFMAG0);
    output.push_back(ELFMAG1);
    output.push_back(ELFMAG2);
    output.push_back(ELFMAG3);
    output.push_back(ELFCLASS64);
    output.push_back(ELFDATA2LSB);
    output.push_back(EV_CURRENT);
    output.push_back(ELFOSABI_NONE);

    for (int i = 0; i < 8; i++) {
        output.push_back(0);
    }

    write16(output, ET_EXEC);
    write16(output, EM_X86_64);
    write32(output, EV_CURRENT);

    write64(output, entry_addr);
    write64(output, 64);
    write64(output, 0);

    write32(output, 0);
    write16(output, 64);
    write16(output, 56);
    write16(output, 1);
    write16(output, 0);
    write16(output, 0);
    write16(output, 0);
}

void ELFWriter::write_program_header(std::vector<uint8_t>& output, size_t code_size) {
    size_t file_size = HEADER_SIZE + code_size;
    size_t mem_size = file_size;

    write32(output, PT_LOAD);
    write32(output, PF_R | PF_X);
    write64(output, 0);
    write64(output, BASE_ADDR);
    write64(output, BASE_ADDR);
    write64(output, file_size);
    write64(output, mem_size);
    write64(output, 0x1000);
}

void ELFWriter::write(const std::string& filename, const std::vector<uint8_t>& code,
                      size_t entry_offset) {
    std::vector<uint8_t> output;
    output.reserve(HEADER_SIZE + code.size());

    uint64_t entry_addr = BASE_ADDR + HEADER_SIZE + entry_offset;

    write_elf_header(output, entry_addr);
    write_program_header(output, code.size());
    output.insert(output.end(), code.begin(), code.end());

    std::ofstream file(filename, std::ios::binary);
    if (!file) {
        throw CompileError(ErrorCode::FILE_WRITE_FAILED, {}, filename);
    }

    file.write(reinterpret_cast<const char*>(output.data()), output.size());
    file.close();

#ifdef __linux__
    chmod(filename.c_str(), 0755);
#endif
}

}  // namespace zex
