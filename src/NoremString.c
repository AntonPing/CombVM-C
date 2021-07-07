#include "Norem.h"













/*
char get_bit(char c, int n) {
    return (c >> (8-n)) & 1;
}

char32_t utf8_next_char(char* str) {
    char header = *str;
    
    if(header >= 0b00000000 && header <= 0b01111111) {
        return header;// 0xxxxxxx
    } else if(header >= 0b10000000 && header <= 0b10111111) {
        return 
        // 10xxxxxx
    } else if(header >= 0b11000000 && header <= 0b11011111) {
        // 110xxxxxx
    } else if(header >= 0b11100000 && header <= 0b11101111) {
        // 1110xxxxxx
    } else if(header >= 0b11110000 && header <= 0b11110111) {
        // 11110xxxxxx
    }



size_t utf8_to_utf32(char* buffer, char32_t code) {
    if (code <= 0x7F) {
        buffer[0] = code;
        return 1;
    }
    if (code <= 0x7FF) {
        buffer[0] = 0xC0 | (code >> 6);            // 110xxxxx
        buffer[1] = 0x80 | (code & 0x3F);          // 10xxxxxx
        return 2;
    }
    if (code <= 0xFFFF) {
        buffer[0] = 0xE0 | (code >> 12);           // 1110xxxx
        buffer[1] = 0x80 | ((code >> 6) & 0x3F);   // 10xxxxxx
        buffer[2] = 0x80 | (code & 0x3F);          // 10xxxxxx
        return 3;
    }
    if (code <= 0x10FFFF) {
        buffer[0] = 0xF0 | (code >> 18);           // 11110xxx
        buffer[1] = 0x80 | ((code >> 12) & 0x3F);  // 10xxxxxx
        buffer[2] = 0x80 | ((code >> 6) & 0x3F);   // 10xxxxxx
        buffer[3] = 0x80 | (code & 0x3F);          // 10xxxxxx
        return 4;
    }
    return 0;
}


char32_t utf32_to_utf8(char32_t code) {
    int len = 0;
    while(code <= 0xFF)




}


void print_utf32_char(char32_t char) {

    #ifdef __linux__
        
    #elif _WIN32
        string port("Com3");
    #endif



}
*/