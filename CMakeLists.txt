cmake_minimum_required(VERSION 3.16)
project(zex C)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Compiler warnings
add_compile_options(-Wall -Wextra -Wpedantic)

# Release build by default
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Release optimization flags
set(CMAKE_C_FLAGS_RELEASE "-Ofast -flto -DNDEBUG -fdata-sections -ffunction-sections")
set(CMAKE_EXE_LINKER_FLAGS_RELEASE "-flto=auto -Wl,--gc-sections")

# Source files
set(CORE_SRC
    Core/memory.c
    Core/error.c
)

set(OBJECTS_SRC
    Objects/object.c
    Objects/tableobject.c
    Objects/stringobject.c
    Objects/intobject.c
    Objects/floatobject.c
    Objects/boolobject.c
    Objects/nullobject.c
    Objects/funcobject.c
    Objects/classobject.c
    Objects/arrayobject.c
)

set(PARSER_SRC
    Parser/token.c
    Parser/lexer.c
    Parser/ast.c
    Parser/parser.c
)

set(COMPILER_SRC
    Compiler/bytecode.c
    Compiler/symtable.c
    Compiler/compiler.c
)

set(RUNTIME_SRC
    Runtime/vm.c
    Runtime/frame.c
    Runtime/gc.c
)

set(MODULES_SRC
    Modules/builtins.c
)

set(PROGRAMS_SRC
    Programs/main.c
)

# Include directories
include_directories(
    ${CMAKE_SOURCE_DIR}
    ${CMAKE_SOURCE_DIR}/Core
    ${CMAKE_SOURCE_DIR}/Objects
    ${CMAKE_SOURCE_DIR}/Parser
    ${CMAKE_SOURCE_DIR}/Compiler
    ${CMAKE_SOURCE_DIR}/Runtime
    ${CMAKE_SOURCE_DIR}/Modules
)

# Executable
add_executable(zex
    ${CORE_SRC}
    ${OBJECTS_SRC}
    ${PARSER_SRC}
    ${COMPILER_SRC}
    ${RUNTIME_SRC}
    ${MODULES_SRC}
    ${PROGRAMS_SRC}
)

# Link math library
target_link_libraries(zex m)

# Strip in release mode
if(CMAKE_BUILD_TYPE STREQUAL "Release")
    add_custom_command(TARGET zex POST_BUILD
        COMMAND ${CMAKE_STRIP} $<TARGET_FILE:zex> || true
        COMMENT "Stripping zex executable"
    )
endif()
