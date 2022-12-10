#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
#include <numeric>
#include <ranges>
#include <array>

enum Opcode {
    addx, noop
};

const std::map<std::string, Opcode> str_to_opcode{
    { "addx", addx },
    { "noop", noop }
};

std::istream& operator>>(std::istream& is, Opcode& opcode) {
    std::string mnemonic;
    is >> mnemonic;
    if (!is.fail()) opcode = str_to_opcode.at(mnemonic);
    return is;
}

int main() try {
    std::ifstream ifs{ "input.txt" };
    if (!ifs) throw std::runtime_error{ "failed to open input file" };

    std::array<std::size_t, 6> targets{ 20, 60, 100, 140, 180, 220 };
    auto target = targets.begin();
    std::vector<int> vec;


    int cycles = 0;
    int x = 1;

    auto draw_pixel = [&] {
        static int col = 0;
        std::cout << (col >= x - 1 && col <= x + 1 ? '#' : '.');
        ++col;
        if (col == 40) { std::cout << '\n'; col = 0; }
    };

    for (Opcode opcode; ifs >> opcode;) {
        draw_pixel();
        if (opcode == noop) { ++cycles; continue; }

        /* have we found all the cycles we interested in? */
        //cycles += 2;
        cycles += 2;
        draw_pixel();

        if (target != targets.end() && cycles >= *target) {
            vec.push_back(*target* x);
            ++target;
        }

        /* opcode must be addx */
        int operand;
        ifs >> operand;
        x += operand;
    }
    std::cout << '\n';
    int part1 = std::accumulate(vec.begin(), vec.end(), 0);
    std::cout << "part 1: " << part1 << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return 1;
}
