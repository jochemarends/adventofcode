#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include <numeric>

enum Choice {
    rock = 1, 
    paper, 
    scissors
};

bool operator>(Choice a, Choice b) {
    switch (a) {
    case rock:
        return b == scissors;
    case paper:
        return b == rock;
    case scissors:
        return b == paper;
    }
}

bool operator<(Choice a, Choice b) {
    switch (a) {
    case rock:
        return b == paper;
    case paper:
        return b == scissors;
    case scissors:
        return b == rock;
    }
}

const std::map<char, Choice> map{
    { 'A', rock }, { 'B', paper }, { 'C', scissors },
    { 'X', rock }, { 'Y', paper }, { 'Z', scissors },
};

int main() try {
    std::ifstream ifs{ "input.txt" };
    if (!ifs) throw std::runtime_error{ "failed to open input file" }; 

    std::vector<char> vec(std::istream_iterator<char>{ifs},
                          std::istream_iterator<char>{});

    int score = 0;
    for (std::size_t i = 1; i < vec.size(); i += 2) {
        Choice a = map.at(vec[i]);
        Choice b = map.at(vec[i - 1]);

        score += static_cast<int>(a); // your choice

        if (a > b)		score += 6;	// win
        else if (a < b) score += 0; // loss
        else			score += 3; // draw
    }

    std::cout << "part 1: " << score << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return 1;
}
