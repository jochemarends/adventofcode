#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <format>
#include <fstream>
#include <functional>
#include <iostream>
#include <iterator>
#include <print>
#include <ranges>
#include <set>
#include <vector>

template<std::output_iterator<int> Iter1, std::output_iterator<int> Iter2>
void parse(std::istream& is, Iter1 left, Iter2 right) {
    auto data = std::views::istream<int>(is) | std::ranges::to<std::vector>();
    std::ranges::copy(data | std::views::stride(2), left);
    std::ranges::copy(data | std::views::drop(1) | std::views::stride(2), right);
}

int part1(std::ranges::input_range auto&& left, std::ranges::input_range auto&& right) {
    std::multiset<int> sorted_left{std::ranges::begin(left), std::ranges::end(left)};
    std::multiset<int> sorted_right{std::ranges::begin(right), std::ranges::end(right)};
    auto distances = std::views::zip_transform([](auto a, auto b) {
        return std::abs(a - b);
    }, sorted_left, sorted_right);
    return std::ranges::fold_right(distances, 0, std::plus{});
}

int part2(std::ranges::input_range auto&& left, std::ranges::input_range auto&& right) {
    auto freqs = std::views::transform(left, std::bind_front(std::ranges::count, right));
    auto scores = std::views::zip_transform(std::multiplies{}, left, freqs);
    return std::ranges::fold_right(scores, 0, std::plus{});
}

int main() {
    std::ifstream ifs{"./input.txt"};
    if (!ifs) {
        std::println(stderr, "error: failed to read input file");
        return EXIT_FAILURE;
    }

    std::vector<int> left{}, right{};
    parse(ifs, std::back_inserter(left), std::back_inserter(right));

    std::println("part 1: {}", part1(left, right));
    std::println("part 2: {}", part2(left, right));
}
