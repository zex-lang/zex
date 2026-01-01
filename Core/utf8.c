/*
 * Zex Programming Language
 * Core/utf8.c - UTF-8 encoding/decoding implementation
 */

#include "utf8.h"

int utf8_encode(uint32_t codepoint, char* out) {
    if (codepoint <= 0x7F) {
        /* 1 byte: 0xxxxxxx */
        out[0] = (char)codepoint;
        return 1;
    } else if (codepoint <= 0x7FF) {
        /* 2 bytes: 110xxxxx 10xxxxxx */
        out[0] = (char)(0xC0 | (codepoint >> 6));
        out[1] = (char)(0x80 | (codepoint & 0x3F));
        return 2;
    } else if (codepoint <= 0xFFFF) {
        /* 3 bytes: 1110xxxx 10xxxxxx 10xxxxxx */
        out[0] = (char)(0xE0 | (codepoint >> 12));
        out[1] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
        out[2] = (char)(0x80 | (codepoint & 0x3F));
        return 3;
    } else if (codepoint <= 0x10FFFF) {
        /* 4 bytes: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx */
        out[0] = (char)(0xF0 | (codepoint >> 18));
        out[1] = (char)(0x80 | ((codepoint >> 12) & 0x3F));
        out[2] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
        out[3] = (char)(0x80 | (codepoint & 0x3F));
        return 4;
    }
    /* Invalid codepoint */
    return 0;
}

int utf8_decode(const char* str, uint32_t* codepoint) {
    unsigned char c = (unsigned char)str[0];
    
    if ((c & 0x80) == 0) {
        /* 1 byte: 0xxxxxxx */
        *codepoint = c;
        return 1;
    } else if ((c & 0xE0) == 0xC0) {
        /* 2 bytes: 110xxxxx 10xxxxxx */
        if (((unsigned char)str[1] & 0xC0) != 0x80) return 0;
        *codepoint = ((c & 0x1F) << 6) | (str[1] & 0x3F);
        return 2;
    } else if ((c & 0xF0) == 0xE0) {
        /* 3 bytes: 1110xxxx 10xxxxxx 10xxxxxx */
        if (((unsigned char)str[1] & 0xC0) != 0x80) return 0;
        if (((unsigned char)str[2] & 0xC0) != 0x80) return 0;
        *codepoint = ((c & 0x0F) << 12) | 
                     ((str[1] & 0x3F) << 6) | 
                     (str[2] & 0x3F);
        return 3;
    } else if ((c & 0xF8) == 0xF0) {
        /* 4 bytes: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx */
        if (((unsigned char)str[1] & 0xC0) != 0x80) return 0;
        if (((unsigned char)str[2] & 0xC0) != 0x80) return 0;
        if (((unsigned char)str[3] & 0xC0) != 0x80) return 0;
        *codepoint = ((c & 0x07) << 18) | 
                     ((str[1] & 0x3F) << 12) | 
                     ((str[2] & 0x3F) << 6) | 
                     (str[3] & 0x3F);
        return 4;
    }
    
    /* Invalid UTF-8 */
    return 0;
}

int utf8_char_len(const char* str) {
    unsigned char c = (unsigned char)str[0];
    
    if ((c & 0x80) == 0) return 1;
    if ((c & 0xE0) == 0xC0) return 2;
    if ((c & 0xF0) == 0xE0) return 3;
    if ((c & 0xF8) == 0xF0) return 4;
    
    /* Invalid byte - treat as single byte */
    return 1;
}

int utf8_char_count(const char* str, int byte_len) {
    int count = 0;
    int i = 0;
    
    while (i < byte_len) {
        i += utf8_char_len(str + i);
        count++;
    }
    
    return count;
}

int utf8_byte_offset(const char* str, int byte_len, int char_idx) {
    int offset = 0;
    int idx = 0;
    
    while (offset < byte_len && idx < char_idx) {
        offset += utf8_char_len(str + offset);
        idx++;
    }
    
    if (idx == char_idx && offset <= byte_len) {
        return offset;
    }
    
    return -1;  /* Index out of bounds */
}
