/*
 * Zex Programming Language
 * Core/utf8.h - UTF-8 encoding/decoding utilities
 */

#ifndef ZEX_UTF8_H
#define ZEX_UTF8_H

#include <stdint.h>

/**
 * Encode a Unicode codepoint to UTF-8.
 * @param codepoint The Unicode codepoint (0x0 - 0x10FFFF)
 * @param out Buffer to write UTF-8 bytes (must be at least 4 bytes)
 * @return Number of bytes written (1-4), or 0 if invalid codepoint
 */
int utf8_encode(uint32_t codepoint, char* out);

/**
 * Decode a UTF-8 character to a Unicode codepoint.
 * @param str Pointer to UTF-8 string
 * @param codepoint Output for the decoded codepoint
 * @return Number of bytes consumed (1-4), or 0 if invalid
 */
int utf8_decode(const char* str, uint32_t* codepoint);

/**
 * Count the number of UTF-8 characters in a byte string.
 * @param str The UTF-8 string
 * @param byte_len Length in bytes
 * @return Number of UTF-8 characters
 */
int utf8_char_count(const char* str, int byte_len);

/**
 * Get the byte offset for a given character index.
 * @param str The UTF-8 string
 * @param byte_len Total byte length
 * @param char_idx Character index (0-based)
 * @return Byte offset, or -1 if index out of bounds
 */
int utf8_byte_offset(const char* str, int byte_len, int char_idx);

/**
 * Get the byte length of the UTF-8 character at a given position.
 * @param str Pointer to the start of the character
 * @return Number of bytes (1-4), or 1 if invalid (treat as single byte)
 */
int utf8_char_len(const char* str);

#endif /* ZEX_UTF8_H */
