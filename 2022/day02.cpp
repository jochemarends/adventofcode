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

/* does a win from b? */
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

/* does a lose from b? */
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

    int score1 = 0;
    int score2 = 0;
    for (std::size_t i = 1; i < vec.size(); i += 2) {
        Choice a = map.at(vec[i - 1]);
        Choice b = map.at(vec[i]);

        score1 += static_cast<int>(b); // your choice

        if (a < b)      score1 += 6; // win
        else if (a > b) score1 += 0; // loss
        else		    score1 += 3; // draw

        /* part 2 */
        Choice c;
        switch (vec[i]) {
        case 'X':               // lose
            if (a > rock)       c = rock;
            else if (a > paper) c = paper;
            else                c = scissors;
            break;
        case 'Y':               // draw
            c = a;
            score2 += 3;
            break;
        case 'Z':               // win
            if (a < rock)       c = rock; 
            else if (a < paper) c = paper;
            else                c = scissors;
            score2 += 6;
            break;
        default:
            throw std::runtime_error{ "invalid input" };
        }
        score2 += static_cast<int>(c);
    }

    std::cout << "part 1: " << score1 << '\n';
    std::cout << "part 2: " << score2 << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return 1;
}
