#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <ranges>
#include <variant>
#include <compare>

struct packet {
    std::vector<std::variant<packet, int>> data;
};

std::strong_ordering operator<=>(const packet& a, int b) {
    if (a.data.empty()) return std::strong_ordering::less;

    return std::visit([&](const auto& data){
        if (std::is_eq(data <=> b) && a.data.size() > 1) {
            return std::strong_ordering::greater;
        }
        return data <=> b;
    }, a.data.front());
}

std::strong_ordering operator<=>(int a, const packet& b) {
    if (b.data.empty()) return std::strong_ordering::greater;

    return std::visit([&](const auto& data){
        if (std::is_eq(a <=> data) && b.data.size() > 1) {
            return std::strong_ordering::less;
        }
        return a <=> data;
    }, b.data.front());
}

std::strong_ordering operator<=>(const packet& a, const packet& b) {
    if (a.data.empty() || b.data.empty()) {
        return a.data.size() <=> b.data.size();
    }

    auto compare = [](const auto& c, const auto& d) -> std::strong_ordering {
        return std::visit([&](const auto& a, const auto& b){
            return a <=> b;
        }, c, d);
    };

    return std::lexicographical_compare_three_way(
        a.data.begin(), a.data.end(),
        b.data.begin(), b.data.end(), compare
    );
}

std::istream& operator>>(std::istream& is, packet& p) {
    if ((is >> std::ws).get() != '[') is.setstate(std::ios_base::failbit);
    p.data.clear();

    while (is.good() && is.peek() != ']') {
        if (is.peek() == '[') {
            p.data.emplace_back(packet{});
        } else {
            p.data.emplace_back(int{});
        }
        std::visit([&is](auto& data) { is >> data; }, p.data.back());
        if (is.peek() == ',') is.get();
    }

    if (is.good() && is.get() != ']') is.setstate(std::ios_base::failbit);
    return is;
}

std::ostream& operator<<(std::ostream& os, const packet& p) {
    os << '[';

    if (!p.data.empty()) {
        for (std::size_t idx{0}; idx < p.data.size() - 1; ++idx) {
            std::visit([&](const auto& data) { os << data; }, p.data[idx]);
            os << ',';
        }
        std::visit([&](const auto& data){ os << data; }, p.data.back());
    }

    return os << ']';
}

int main() try {
    std::ifstream ifs{"input.txt"};
    if (!ifs) {
        throw std::runtime_error{"error: failed to open input file"};
    }

    std::vector<packet> v{std::istream_iterator<packet>{ifs}, std::istream_iterator<packet>{}};

    std::size_t part1{0};
    for (std::size_t idx{1}, pair_idx{1}; idx < v.size(); idx += 2, ++pair_idx) {
        packet a = v[idx - 1];
        packet b = v[idx];
        if (a < b) part1 += pair_idx;
    }

    std::istringstream iss{"[[2]] [[6]]"};
    packet dividers[2];
    std::copy(std::istream_iterator<packet>{iss}, std::istream_iterator<packet>{}, std::begin(dividers));
    std::ranges::copy(dividers, std::back_inserter(v));

    std::sort(v.begin(), v.end());

    auto it1 = std::lower_bound(v.begin(), v.end(), dividers[0]);
    auto it2 = std::lower_bound(v.begin(), v.end(), dividers[1]);
    std::size_t part2 = (std::distance(v.begin(), it1) + 1) * (std::distance(v.begin(), it2) + 1);

    std::cout << "part 1: " << part1 << '\n';
    std::cout << "part 2: " << part2 << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return EXIT_FAILURE;
}
