#include <cstdint>
#include <cstddef>
#include <cassert>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <limits>

// Naive comparsion
void unique_of_4(const char buf[4], bool& is_unique, size_t& advance) {
    // Assume it's not unique
    is_unique = false;

    for (int i = 1; i < 4; ++i) {
        for (int j = 0; j < i; ++j) {
            if (buf[i] == buf[j]) {
                advance = j + 1;
                return;
            }
        }
    }

    // ... unless we checked everything
    is_unique = true;
    advance = 0;
}

void unique_of_n(const char* buf, size_t buf_size, bool& is_unique, size_t& advance) {
    constexpr size_t LETTER_CNT = 26;
    constexpr size_t INVALID_LETTER_OCC = std::numeric_limits<size_t>::max();
    size_t letter_occurrences[LETTER_CNT];
    for (size_t i = 0; i < LETTER_CNT; ++i) {
        letter_occurrences[i] = INVALID_LETTER_OCC;
    }

    for (size_t i = 0; i < buf_size; ++i) {
        auto& the_letter = letter_occurrences[buf[i] - 'a'];
        if (the_letter == INVALID_LETTER_OCC) {
            the_letter = i;
        } else {
            is_unique = false;
            advance = the_letter + 1;
            return;
        }
    }

    is_unique = true;
    advance = 0;
}

void solve(const std::string& input, size_t window_size) {
    size_t window_begin = 0;
    while (window_begin + window_size <= input.length()) {
        bool is_unique;
        size_t advance;
        unique_of_n(&input[window_begin], window_size, is_unique, advance);

        if (is_unique) {
            std::cout << "Offset of datagram: " << window_begin + window_size << '\n';
            break;
        }

        window_begin += advance;
    }
}

int main() {
    std::string input;
    std::ifstream input_file("inputs/day06.txt");
    std::getline(input_file, input);

    solve(input, 4);
    solve(input, 14);
}
