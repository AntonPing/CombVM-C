//#include "Norem.h"

#include <iostream>
#include <string>
#include <locale>
#include <codecvt>
// C++17标准正式弃用codecvt头文件和wstring_convert

int main() {
    std::u32string foo = U"你好，世界！";
    std::cout << foo << std::endl;
}