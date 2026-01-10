#ifndef ZEX_ELF_HPP
#define ZEX_ELF_HPP

#include <cstdint>
#include <string>
#include <vector>

namespace zex {

class ELFWriter {
   public:
    static constexpr uint64_t BASE_ADDR = 0x400000;
    static constexpr size_t HEADER_SIZE = 64 + 56;

    void write(const std::string& filename, const std::vector<uint8_t>& code, size_t entry_offset);

   private:
    static constexpr uint8_t ELFMAG0 = 0x7f;
    static constexpr uint8_t ELFMAG1 = 'E';
    static constexpr uint8_t ELFMAG2 = 'L';
    static constexpr uint8_t ELFMAG3 = 'F';
    static constexpr uint8_t ELFCLASS64 = 2;
    static constexpr uint8_t ELFDATA2LSB = 1;
    static constexpr uint8_t EV_CURRENT = 1;
    static constexpr uint8_t ELFOSABI_NONE = 0;
    static constexpr uint16_t ET_EXEC = 2;
    static constexpr uint16_t EM_X86_64 = 62;
    static constexpr uint32_t PT_LOAD = 1;
    static constexpr uint32_t PF_X = 1;
    static constexpr uint32_t PF_R = 4;

    void write_elf_header(std::vector<uint8_t>& output, uint64_t entry_addr);
    void write_program_header(std::vector<uint8_t>& output, size_t code_size);
    void write16(std::vector<uint8_t>& out, uint16_t val);
    void write32(std::vector<uint8_t>& out, uint32_t val);
    void write64(std::vector<uint8_t>& out, uint64_t val);
};

}  // namespace zex

#endif  // ZEX_ELF_HPP
