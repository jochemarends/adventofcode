#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <stack>
#include <algorithm>
#include <numeric>
#include <ranges>

int main() try {
    std::ifstream ifs{ "input.txt" };
    if (!ifs) throw std::runtime_error{ "failed to open input file" };

    /* read until a blank line */
    std::vector<std::string> lines;
    for (std::string line; std::getline(ifs, line) && !line.empty();)
        lines.push_back(line);

    std::vector<std::size_t> column_positions;
    for (std::size_t i = 0; char ch : lines.back()) {
        if (isdigit(ch)) column_positions.push_back(i);
        ++i;
    }

    std::vector<std::stack<char>> v{ column_positions.size() };

    /* push the data onto the stack structures */
    for (const std::string& line : std::views::reverse(lines)) {
        for (std::size_t i = 0; i < v.size(); ++i) {
            char ch = line[column_positions[i]];
            if (std::isspace(ch)) continue;
            v[i].push(ch);
        }
    }

    auto part1{ v }, part2{ v };

    /* read the instructions */
    while (!ifs.eof()) {
        std::string s;
        std::size_t count, src_index, dst_index;
        ifs >> s >> count >> s >> src_index 
            >> s >> dst_index;

        /* convert from 1 to 0 based index */
        const auto src_iter1 = part1.begin() + src_index - 1;
        const auto dst_iter1 = part1.begin() + dst_index - 1;

        const auto src_iter2 = part2.begin() + src_index - 1;
        const auto dst_iter2 = part2.begin() + dst_index - 1;

        std::vector<char> temp;
        for (std::size_t i = 0; i < count; ++i) {
            /* part 1 */
            dst_iter1->push(src_iter1->top());
            src_iter1->pop();

            /* part 2 */
            temp.push_back(src_iter2->top());
            src_iter2->pop();
        }

        /* push the elements in reversed order */
        std::for_each(temp.rbegin(), temp.rend(), [&dst_iter2](char ch) { dst_iter2->push(ch); });
    }

    std::cout << "part 1: ";
    std::ranges::for_each(part1, [](auto& stack) { std::cout << stack.top(); });
    std::cout << "\npart 2: ";
    std::ranges::for_each(part2, [](auto& stack) { std::cout << stack.top(); });
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return 0;
}
