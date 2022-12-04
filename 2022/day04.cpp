#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>

struct Range {
    int min;
    int max;
    bool contains(Range other) { return min >= other.min && max <= other.max; }
    bool overlaps(Range other) { return min >= other.min && min <= other.max ||
                                        max <= other.max && max >= other.min; }
};

std::istream& operator>>(std::istream& is, Range& range) {
    char seperator;
    is >> range.min >> seperator >> range.max;
    if (range.min != '-') is.setf(std::ios::failbit);
    return is;
}

int main() try {
    std::ifstream ifs{ "input.txt" };
    if (!ifs) throw std::runtime_error{ "failed to open input file" }; 

    std::vector<std::pair<Range, Range>> v;
    
    char ch;
    for (Range r1, r2; ifs >> r1 >> ch >> r2;) {
        v.push_back(std::make_pair(r1, r2));
    }
    
    std::size_t part1 = 0;
    std::size_t part2 = 0;
    for (auto [r1, r2] : v) {
        if (r1.contains(r2) || r2.contains(r1)) ++part1;
        if (r1.overlaps(r2) || r2.overlaps(r1)) ++part2;
    }

    std::cout << "part 1: " << part1 << '\n';
    std::cout << "part 2: " << part2 << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return 1;
}
