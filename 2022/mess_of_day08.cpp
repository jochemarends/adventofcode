#include <fstream>
#include <iostream>
#include <sstream>
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

    std::size_t part1 = 0;
    std::vector<std::size_t> part2;
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
            std::vector<int> left;
            std::copy_n(row.begin(), std::distance(row.begin(), it2), std::back_inserter(left));
            std::ranges::reverse(left);
            if (std::ranges::max(left) < height) {
                is_visible = true;
            }

            /* extract trees on the right */
            std::vector<int> right;
            std::copy(std::next(it2), row.end(), std::back_inserter(right));
            if (std::ranges::max(right) < height) {
                is_visible = true;
            }

            /* extract trees above */
            std::vector<int> above;
            std::size_t index = std::distance(row.begin(), it2);
            std::transform(vec.begin(), it1, std::back_inserter(above), [index](const auto& row) { return row[index]; });
            std::ranges::reverse(above);
            if (std::ranges::max(above) < height) {
                is_visible = true;
            }

            /* extract trees below */
            std::vector<int> below;
            std::transform(std::next(it1), vec.end(), std::back_inserter(below), [index](const auto& row) { return row[index]; });
            //std::ranges::reverse(below);
            if (std::ranges::max(below) < height) {
                is_visible = true;
            }

            if (is_visible) ++part1;

            //std::ranges::for_each(below, [](auto elem) { std::cout << elem << '\n'; });
            //std::cout << std::distance(below.begin(), std::ranges::adjacent_find(below, std::greater_equal())) + 1 << '\n';

            /* calculates the viewing dist in a given direction */
            auto view_dist = [&](const std::vector<int>& vec) -> std::size_t {
                if (vec.front() >= height) return 1;
                std::size_t n = 1;
                for (std::size_t i = 1; i < vec.size(); ++i, ++n) {
                    if (vec[i - 1] > vec[i]) break;
                }
                return n;
                return std::distance(vec.begin(), std::ranges::adjacent_find(vec, std::greater_equal())) + 1;
            };

            /* part 2 */
            std::vector<std::size_t> dists(4);
            dists[0] = view_dist(above);
            dists[1] = view_dist(left);
            dists[2] = view_dist(right);
            dists[3] = view_dist(below);

            part2.push_back(std::accumulate(dists.begin(), dists.end(), 1, std::multiplies()));
            std::ranges::for_each(dists, [](auto elem) { std::cout << elem << '\n'; });
            std::cout << '\n';
        }
    }
    std::cout << "part 1: " << part1 << '\n';
    std::cout << "part 2: " << std::ranges::max(part2) << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return 1;
}
