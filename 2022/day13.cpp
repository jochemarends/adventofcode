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

std::strong_ordering operator<=>(const packet& a, const packet& b) {
    if (a.data.empty() || b.data.empty()) {
        return a.data.size() <=> b.data.size();
    }

    return std::visit([&](const auto& c, const auto& d){
        return c <=> d;
    }, a.data.front(), b.data.front());
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

void is_sorted(const packet& a, const packet& b) {
    auto it1 = a.data.begin();
    auto it2 = b.data.begin();

}

int main() try {
    std::ifstream ifs{"input.txt"};
    if (!ifs) {
        throw std::runtime_error{"error: failed to open input file"};
    }

    std::vector<packet> vec{std::istream_iterator<packet>{ifs}, std::istream_iterator<packet>{}};
    std::cout << vec[0] << '\n';
    std::cout << vec[1] << '\n';
    std::cout << !std::ranges::lexicographical_compare(vec[1].data, vec[0].data) << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return EXIT_FAILURE;
}
