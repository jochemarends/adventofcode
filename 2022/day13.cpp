#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <ranges>
#include <variant>
#include <algorithm>

struct packet {
    std::vector<std::variant<packet, int>> data;
};

std::strong_ordering operator<=>(const packet& a, int b) {
    if (a.data.empty()) return std::strong_ordering::less;

    return std::visit([&](const auto& data){
        return data <=> b;
    }, a.data.front());
}

std::strong_ordering operator<=>(int a, const packet& b) {
    if (b.data.empty()) return std::strong_ordering::greater;

    return std::visit([&](const auto& data){
        return a <=> data;
    }, b.data.front());
}

#include <compare>

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


    std::vector<std::pair<packet, packet>> vec;
    for (packet a, b; ifs >> a >> b;) {
        vec.push_back(std::make_pair(a, b));
    }

    std::size_t part1{};
    for (std::size_t idx{0}; idx < vec.size(); ++idx) {
        if (vec[idx].first < vec[idx].second) {
            part1 += idx + 1;
        }
    }

    std::cout << "part 1: " << part1 << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return EXIT_FAILURE;
}
