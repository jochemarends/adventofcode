#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>
#include <ranges>

int main() try {
    std::ifstream ifs{ "input.txt" };
    if (!ifs) throw std::runtime_error{ "failed to open input file" };

    std::vector<std::vector<int>> vec;
    for (std::string line; std::getline(ifs, line);) {
        /* read single digits */
        vec.push_back({});
        std::ranges::transform(line, std::back_inserter(vec.back()), [](char ch) { return ch - '0'; });
    }

    int part1 = 0;
    int part2 = 0;
    std::vector<int> scores;
    for (auto it1 = vec.begin(); it1 != vec.end(); ++it1) {
        const auto& row = *it1;

        /* all trees in the outer rows visible */
        if (it1 == vec.begin() || it1 == std::prev(vec.end())) {
            part1 += row.size();
            continue;
        }

        for (auto it2 = row.begin(); it2 != row.end(); ++it2) {
            int height = *it2;

            /* is the tree located at the edge? */
            if (it2 == row.begin() || it2 == std::prev(row.end())) {
                ++part1;
                continue;
            }

            bool is_visible = false;

            /* extract trees on the left */
            std::vector<int> left{ row.begin(), it2 };
            std::ranges::reverse(left);
            if (std::ranges::max(left) < height) {
                is_visible = true;
            }

            /* extract trees on the right */
            std::vector<int> right{ std::next(it2), row.end() };
            if (std::ranges::max(right) < height) {
                is_visible = true;
            }

            /* extract trees above */
            std::vector<int> above;
            /* index of column we need to extract */
            std::size_t index = std::distance(row.begin(), it2);
            std::transform(vec.begin(), it1, std::back_inserter(above), [index](const auto& row) { return row[index]; });
            std::ranges::reverse(above);
            if (std::ranges::max(above) < height) {
                is_visible = true;
            }

            /* extract trees below */
            std::vector<int> below;
            std::transform(std::next(it1), vec.end(), std::back_inserter(below), [index](const auto& row) { return row[index]; });
            if (std::ranges::max(below) < height) {
                is_visible = true;
            }

            if (is_visible) ++part1;

            /* part 2 */
            std::vector<int> distances;
            for (const auto& vec : { above, left, right, below }) {
                /* search for a tree that is higher or equal than the current one */
                auto it = std::ranges::find_if(vec, [height](int i) { return height <= i; });
                distances.push_back(std::distance(vec.begin(), it));
                /* if we have not reached the edge yet, we are looking at one more tree! */
                if (it != vec.end()) distances.back()++;
            }
            scores.push_back(std::accumulate(distances.begin(), distances.end(), 1, std::multiplies()));
        }
        part2 = std::ranges::max(scores);
    }
    std::cout << "part 1: " << part1 << '\n';
    std::cout << "part 2: " << part2 << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return 1;
}
