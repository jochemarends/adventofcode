#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <set>
#include <map>
#include <algorithm>
#include <numeric>
#include <ranges>

enum Direction {
    up      = 'U',
    left    = 'L',
    right   = 'R',
    down    = 'D'
};

using Point = std::pair<int, int>;

const std::map<Direction, Point> dir_to_point{
    { up,       { 0, 1 } },
    { left,     {-1, 0 } },
    { right,    { 1, 0 } },
    { down,     { 0,-1 } }
};


Point& operator+=(Point& a, Point b) {
    a.first += b.first; a.second += b.second;
    return a;
}

Point& operator-=(Point& a, Point b) {
    a.first -= b.first; a.second -= b.second;
    return a;
}

Point operator-(Point a, Point b) {
    return Point{ a.first - b.first, a.second - b.second };
}

Point abs(Point p) { 
    return Point{ std::abs(p.first), std::abs(p.second) };
}

double dist(Point a, Point b) {
    auto [dx, dy] = b - a;
    return std::sqrt(dx * dx + dy * dy);
}

template<std::size_t N>
void update(std::span<Point, N> rope, Direction dir) {
    rope.front() += dir_to_point.at(dir);

    for (auto it = std::next(rope.begin()); it != rope.end(); ++it) {
        Point& head = *std::prev(it);
        Point& tail = *it;

        auto  [dx, dy] = head - tail;
        auto& [tx, ty] = tail;

        /* ropes are touching if the distance between them is not higher than 2.0 */
        bool is_touching = std::sqrt(dx * dx + dy * dy) <= 2.0;

        /* add normalized direction if needed */
        if (std::abs(dx) > 1 || !is_touching) {
            tx += std::max(-1, std::min(dx, 1));
        }
        if (std::abs(dy) > 1 || !is_touching) {
            ty += std::max(-1, std::min(dy, 1));
        }
    }
}

void update(Point& head, Point& tail, Direction dir) {
    head += dir_to_point.at(dir);
    auto  [dx, dy] = head - tail;
    auto& [tx, ty] = tail;

    /* ropes are touching if the distance between them is not higher than 2.0 */
    bool is_touching = std::sqrt(dx * dx + dy * dy) <= 2.0;

    /* add normalized direction if needed */
    if (std::abs(dx) > 1 || !is_touching) {
        tx += std::max(-1, std::min(dx, 1));
    }
    if (std::abs(dy) > 1 || !is_touching) {
        ty += std::max(-1, std::min(dy, 1));
    }
}

template<std::size_t N>
struct Solution {
    std::set<Point> visited;
    Point array[N];
    std::span<Point, N> rope{ array };
};

int main() try {
    std::ifstream ifs{ "input.txt" };
    if (!ifs) throw std::runtime_error{ "failed to open input file" };

    std::vector<std::pair<Direction, int>> vec;
    char ch;
    int i;
    while (ifs >> ch >> i) {
        vec.push_back(std::make_pair(static_cast<Direction>(ch), i));
    }

    Solution<01> part1;
    Solution<10> part2;

    for (auto [dir, count] : vec) {
        for (std::size_t i = 0; i < count; ++i) {
            update(part1.rope, dir);
            part1.visited.insert(part1.rope.back());
            update(part2.rope, dir);
            part2.visited.insert(part2.rope.back());
        }
    }
    std::cout << "part1: " << part1.visited.size() << '\n';
    std::cout << "part2: " << part2.visited.size() << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return 1;
}
