#include "Norem.h"

typedef uint64_t Int_t
typedef double Real_t
typedef uint32_t Char_t

size_t utf32_to_utf8(Char_t* str, const unsigned int code) {
    if (code <= 0x7F) {
        buffer[0] = code;
        return 1;
    }
    if (code <= 0x7FF) {
        buffer[0] = 0xC0 | (code >> 6);            /* 110xxxxx */
        buffer[1] = 0x80 | (code & 0x3F);          /* 10xxxxxx */
        return 2;
    }
    if (code <= 0xFFFF) {
        buffer[0] = 0xE0 | (code >> 12);           /* 1110xxxx */
        buffer[1] = 0x80 | ((code >> 6) & 0x3F);   /* 10xxxxxx */
        buffer[2] = 0x80 | (code & 0x3F);          /* 10xxxxxx */
        return 3;
    }
    if (code <= 0x10FFFF) {
        buffer[0] = 0xF0 | (code >> 18);           /* 11110xxx */
        buffer[1] = 0x80 | ((code >> 12) & 0x3F);  /* 10xxxxxx */
        buffer[2] = 0x80 | ((code >> 6) & 0x3F);   /* 10xxxxxx */
        buffer[3] = 0x80 | (code & 0x3F);          /* 10xxxxxx */
        return 4;
    }
    return 0;
}