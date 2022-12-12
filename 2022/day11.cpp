#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>
#include <functional>
#include <ranges>
#include <regex>

using num_t = std::size_t;

struct Monkey {
    std::vector<num_t> items;
    std::function<num_t(num_t)> operation;
    int test;
    std::pair<num_t, num_t> adjacents;
    num_t inspect_count = 0;
};

std::istream& operator>>(std::istream& is, Monkey& monkey) {
    std::smatch matches;
    std::string str;

    monkey = Monkey{};

    auto next = [&]() {
        static const std::regex pat{ R"((?:old ([+*]) (\d+|old))|\d+)" };
        //static const std::regex pat{ R"(([+*]\s)?(\d+|old))" };
        return std::regex_search(str, matches, pat);
    };

    std::getline(is, str);
    std::getline(is, str);
    while (next()) {
        monkey.items.push_back(std::stoi(matches[0]));
        str = matches.suffix();
    }

    std::getline(is, str);
    next();
    char op = matches[1].str().front();
    if (matches[2].str() == "old") {
        switch (op) {
        case '+':
            monkey.operation = [](std::size_t old) { return old + old; };
            break;
        case '*':
            monkey.operation = [](std::size_t old) { return old * old; };
        }
    }
    else {
        int num = std::stoi(matches[2]);
        switch (op) {
        case '+':
            monkey.operation = [num](num_t old) { return old + num; };
            break;
        case '*':
            monkey.operation = [num](num_t old) { return old * num; };
        }
    }

    std::getline(is, str);
    next();
    monkey.test = std::stoi(matches[0]);

    std::getline(is, str);
    next();
    monkey.adjacents.first = std::stoi(matches[0]);

    std::getline(is, str);
    next();
    monkey.adjacents.second = std::stoi(matches[0]);
    is.ignore('\n');
    return is;
}

int main() try {
    std::ifstream ifs{ "input.txt" };
    if (!ifs) throw std::runtime_error{ "failed to open input file" };

    std::vector<Monkey> monkeys;
    for (Monkey monkey; ifs;) {
        monkey = Monkey{};
        ifs >> monkey;
        monkeys.push_back(monkey);
    }

    std::vector<Monkey> vec1{ monkeys };
    for (std::size_t i = 0; i < 20; ++i) {
        for (Monkey& monkey : vec1) {
            for (auto iter = monkey.items.begin(); iter != monkey.items.end();) {
                int item = *iter;
                int value = monkey.operation(item) / 3;
                if (value % monkey.test == 0) {
                    vec1[monkey.adjacents.first].items.push_back(value);
                }
                else {
                    vec1[monkey.adjacents.second].items.push_back(value);
                }
                ++monkey.inspect_count;
                iter = monkey.items.erase(iter);
            }
        }
    }

    auto it = std::ranges::max_element(vec1, [](auto& a, auto& b) { return a.inspect_count < b.inspect_count; });
    num_t a = it->inspect_count;
    vec1.erase(it);
    it = std::ranges::max_element(vec1, [](auto& a, auto& b) { return a.inspect_count < b.inspect_count; });
    num_t b = it->inspect_count;
    std::size_t part1 = a * b;

    num_t lcm = std::lcm(monkeys[0].test, monkeys[1].test);
    for (std::size_t i = 1; i < monkeys.size(); ++i) {
        lcm = std::lcm(lcm, monkeys[i].test);
    }

    for (std::size_t i = 0; i < 10'000; ++i) {
        for (Monkey& monkey : monkeys) {
            for (auto iter = monkey.items.begin(); iter != monkey.items.end();) {
                num_t item = *iter;
                int value = monkey.operation(item) % lcm;
                if (value % monkey.test == 0) {
                    monkeys[monkey.adjacents.first].items.push_back(value);
                }
                else {
                    monkeys[monkey.adjacents.second].items.push_back(value);
                }
                ++monkey.inspect_count;
                iter = monkey.items.erase(iter);
            }
        }
    }

    it = std::ranges::max_element(monkeys, [](auto& a, auto& b) { return a.inspect_count < b.inspect_count; });
    a = it->inspect_count;
    monkeys.erase(it);
    it = std::ranges::max_element(monkeys, [](auto& a, auto& b) { return a.inspect_count < b.inspect_count; });
    b = it->inspect_count;
    num_t part2 = a * b;

    std::cout << "part 1: " << part1 << '\n';
    std::cout << "part 2: " << part2 << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return 1;
}
