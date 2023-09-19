#include <cstdint>
#include <cstddef>
#include <cassert>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <span>

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

bool unique_range(const char* buf, size_t buf_size, size_t window_size) {
    assert(window_size <= buf_size);

    uint32_t mask = 0;

    // FUCK THIS MAKES NO SENSE
    // Populate the mask with the first `window_size` chars
    size_t i = 0;
    for (; i < window_size; ++i) {
        uint32_t prev = mask;
        mask |= 1u << (buf[i] - 'a');
        uint32_t post = mask;

        if (prev == post) {
            return false;
        }
    }

    for (; i < buf_size; ++i) {
        uint32_t prev = mask;
        mask &= ~(1u << (buf[i - window_size] - 'a'));
        mask |= 1u << (buf[i] - 'a');
        uint32_t post = mask;

        if (prev == post) {
            return false;
        }
    }

    return true;
}

int main() {
    std::string input;
    std::ifstream input_file("inputs/day06.txt");
    std::getline(input_file, input);

    size_t window_begin = 0;
    while (window_begin + 4 <= input.length()) {
        bool is_unique;
        size_t advance;
        unique_of_4(&input[window_begin], is_unique, advance);

        if (is_unique) {
            std::cout << "Offset of datagram: " << window_begin + 4 << '\n';
            break;
        }

        window_begin += advance;
    }
}
