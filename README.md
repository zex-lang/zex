# Zex

A compiled programming language that generates native x86-64 executables directly without external assembler or linker.

## Features

- Native x86-64 code generation
- Self-contained ELF executable output
- No LLVM, no GCC, no external tools required
- Fast compilation

## Quick Start

```bash
# build
cmake -B build && cmake --build build

# compile
./build/zex hello.zx -o hello

# run
./hello
```

## Build Requirements

- CMake 3.16+
- C++17 compiler
- Linux x86-64

## License

MIT
