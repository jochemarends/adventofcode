#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>

int main() try {
    std::ifstream ifs{ "input.txt" };
    if (!ifs) throw std::runtime_error{ "failed to open input file" };

    std::vector<char> vec(std::istream_iterator<char>{ifs},
                          std::istream_iterator<char>{});

    std::size_t part1 = 4;
    for (std::size_t i = 3; i < vec.size(); ++i, ++part1) {
        std::vector<int> sub_vec{ vec.begin() + i - 3, vec.begin() + i + 1 };
        std::sort(sub_vec.begin(), sub_vec.end());
        /* will change the end in case of duplicates */
        if (std::unique(sub_vec.begin(), sub_vec.end()) != sub_vec.end()) continue;
        break;
    }

    std::size_t part2 = 14;
    for (std::size_t i = 13; i < vec.size(); ++i, ++part2) {
        std::vector<char> temp{ vec.begin() + i - 13, vec.begin() + i + 1 };
        std::sort(temp.begin(), temp.end());
        /* will change the end in case of duplicates */
        if (std::unique(temp.begin(), temp.end()) != temp.end()) continue;
        break;
    }

    std::cout << "part 1: " << part1 << '\n';
    std::cout << "part 2: " << part2 << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return 1;
}
