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
    uint32_t mask = 0;
    for (size_t i = 0; i < buf_size; ++i) {
        uint32_t prev = mask;
        mask |= 1u << (buf[i] - 'a');
        uint32_t post = mask;

        if (prev == post) {
            is_unique = false;
            advance = 1;
            return;
        }
    }

    is_unique = true;
    advance = 0;
}

// == <size_t>::max()   Valid result: begin index of the first unique range
// otherwise            No unique window found
size_t find_unique_window(const char* buf, size_t buf_size, size_t window_size) {
    assert(window_size <= buf_size);

    bool occurence_cnt[26];
	size_t curr_window_size = 0;

    for (size_t i = 0; i < buf_size; ++i) {
        auto& the_letter = occurence_cnt[buf[i] - 'a'];
        bool is_duplicate = the_letter >= 1;
        the_letter += 1;

        if (is_duplicate) {
            // We shift the window forward by one
            occurence_cnt[buf[i - curr_window_size] - 'a'] -= 1; // Remove first char in the currnt [beg,end)
        } else {
            the_letter += 1;
            curr_window_size += 1;

            if (curr_window_size == window_size) {
                return i - curr_window_size;
            }
        }
    }

    return std::numeric_limits<size_t>::max();
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

    //size_t res = find_unique_window(input.data(), input.size(), 4);
    //std::cout << "Offset of datagram: " << res << '\n';
}
