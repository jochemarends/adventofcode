#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>

int main() try {
    std::ifstream ifs{ "input.txt" };
    if (!ifs) throw std::runtime_error{ "failed to open input file" };

    std::vector<int> vec;
    int calories = 0;
    for (std::string line; std::getline(ifs, line);) {
        if (!line.empty()) {
            calories += std::stoi(line);
            continue;
        }
        vec.push_back(calories);
        calories = 0;
    }

    std::sort(vec.rbegin(), vec.rend());
    std::cout << "part 1: " << vec.front() << '\n';
    std::cout << "part 2: " << std::accumulate(vec.begin(), vec.begin() + 3, 0) << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return 1;
}
