#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <queue>
#include <algorithm>
#include <ranges>

struct point {
    std::size_t x, y;
};

bool operator==(point a, point b) {
    return a.x == b.x && a.y == b.y;
}

std::ostream& operator<<(std::ostream& os, point p) {
    return os << '(' << p.x << ',' << p.y << ')';
}

int main() try {
    std::ifstream ifs{"input.txt"};
    if (!ifs) {
        throw std::runtime_error{"error: failed to open input file"};
    }

    point start_point{}, end_point{};

    std::vector<std::string> map;
    for (std::string line; std::getline(ifs, line);) {
        if (auto it = std::ranges::find(line, 'S'); it != std::ranges::end(line)) {
            start_point.x = map.size();
            start_point.y = std::distance(line.begin(), it);
        }

        if (auto it = std::ranges::find(line, 'E'); it != std::ranges::end(line)) {
            end_point.x = map.size();
            end_point.y = std::distance(line.begin(), it);
        }

        map.push_back(line);
    }

    auto get_height = [&map](point p) {
        char ch = map[p.x][p.y];
        if (ch == 'S') return 'a';
        if (ch == 'E') return 'z';
        return ch;
    };

    auto is_on_map = [&map](point p) {
        if (p.x > map.size() - 1) return false;
        return (p.y < map[p.x].size() - 1);
    };

    auto adjacent_points = [&](point p) {
        point points[]{
            {p.x + 1, p.y}, {p.x - 1, p.y},
            {p.x, p.y + 1}, {p.x, p.y - 1}
        };
        std::vector<point> vec;
        std::ranges::copy_if(points, std::back_inserter(vec), is_on_map);
        return vec;
    };

    std::queue<std::pair<point, std::size_t>> queue({{start_point, 0}});
    auto visited = map;
    std::ranges::fill(std::views::join(visited), false);
    visited[start_point.x][start_point.y] = true;

    std::size_t part1{};
    while (!queue.empty()) {
        auto [curr_point, n] = queue.front();
        queue.pop();

        auto adjacent = adjacent_points(curr_point);

        if (curr_point == end_point) {
            part1 = n;
            break;
        }

        auto visit = [&](point p) {
            visited[p.x][p.y] = true;
            queue.emplace(p, n + 1);
        };

        auto is_reachable = [&](point next_point) {
            char curr = get_height(curr_point);
            char next = get_height(next_point);
            return next - curr < 2;
        };

        for (auto point : std::views::filter(adjacent, is_reachable)) {
            if (!visited[point.x][point.y]) {
                visit(point);
            }
        }
    }

    queue = decltype(queue)({{end_point, 0}});
    std::ranges::fill(std::views::join(visited), false);
    visited[end_point.x][end_point.y] = true;

    point lowest_point{end_point};
    std::size_t part2{};
    while (!queue.empty()) {
        auto [curr_point, n] = queue.front();
        queue.pop();

        if (get_value(curr_point) < get_value(lowest_point)) {
            lowest_point = curr_point;
            part2 = n;
        }

        auto adjacent = adjacent_points(curr_point);

        auto visit = [&](point p) {
            visited[p.x][p.y] = true;
            queue.emplace(p, n + 1);
        };

        auto is_reachable = [&](point prev_point) {
            char curr = get_height(curr_point);
            char prev = get_height(prev_point);
            return curr - prev < 2;
        };

        for (auto point : std::views::filter(adjacent, is_reachable)) {
            if (!visited[point.x][point.y]) {
                visit(point);
            }
        }
    }

    std::cout << "start:  " << start_point << '\n';
    std::cout << "end:    " << end_point << '\n';
    std::cout << "part 1: " << part1 << '\n';
    std::cout << "part 2: " << part2 << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return EXIT_FAILURE;
}
