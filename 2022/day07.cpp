#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <functional>
#include <set>
#include <algorithm>
#include <numeric>
#include <ranges>

struct Dir;

using File = std::pair<std::string, std::size_t>;
using Dirs = std::vector<std::shared_ptr<Dir>>;
using Files = std::vector<File>;

struct Dir {
    std::string name;
    std::shared_ptr<Dir> parent;
    Files files;
    Dirs dirs;
};

std::size_t nesting(std::shared_ptr<Dir> dir) {
    std::size_t nesting = 0;
    for (; dir->parent != nullptr; ++nesting) dir = dir->parent;
    return nesting;
}

std::ostream& operator<<(std::ostream& os, const File& file) {
    const auto& [name, size] = file;
    return os << "- " << name << " (file, size=" << size << ")\n";
}

std::ostream& operator<<(std::ostream& os, std::shared_ptr<Dir> dir) {
    std::string padding(nesting(dir), '\t');
    os << padding << "- " << dir->name << " (dir)\n";
    std::ranges::for_each(dir->dirs, [&](auto dir) { os << dir; });
    for (const auto& file : dir->files) {
        std::cout << padding << '\t' << file;
    }
    return os;
}

auto root = std::make_shared<Dir>("/");

void change_dir(std::shared_ptr<Dir>& dir, const std::string& name) {
    if (name == "/") dir = root;
    else if (name == "..") dir = dir->parent;
    else {
        for (auto d : dir->dirs) {
            if (d->name == name) {
                dir = d;
            }
        }
    }
}

std::size_t size(std::shared_ptr<Dir> dir) {
    std::size_t result = 0;
    for (const auto& dir : dir->dirs) result += size(dir);
    for (const auto& [name, size] : dir->files) result += size;
    return result;
}

std::size_t part1(std::shared_ptr<Dir> dir) {
    std::size_t result = 0;
    for (auto d : dir->dirs) {
        std::size_t s = size(d);
        if (s <= 100'000) result += s;
        result += part1(d);
    }
    return result;
}

std::size_t part2() {
    const std::size_t used = size(root);
    const std::size_t free = 70'000'000 - used;
    const std::size_t to_delete = 30'000'000 - free;
    
    std::set<std::size_t> set;
    std::function<void(std::shared_ptr<Dir>)> for_each_dir = [&](auto dir) {
        set.insert(size(dir));
        for (auto d : dir->dirs) {
            for_each_dir(d);
        }
    };
    for_each_dir(root);

    for (auto size : set) {
        if (size >= to_delete) return size;
    }
    return -1;
}

int main() try {
    std::ifstream ifs{ "input.txt" };
    if (!ifs) throw std::runtime_error{ "failed to open input file" };

    std::shared_ptr<Dir> curr_dir = root;

    for (std::string s; ifs >> s;) {
        /* command */
        if (s == "$") {
            ifs >> s;
            if (s == "cd") {
                ifs >> s;
                change_dir(curr_dir, s);
            }
            else {
                std::cout << curr_dir;
            }
        }
        /* create dir/file */
        else if (s == "dir") {
            ifs >> s;
            curr_dir->dirs.push_back(std::make_shared<Dir>(s));
            curr_dir->dirs.back()->parent = curr_dir;
        }
        else {
            std::size_t size = std::stoi(s);
            ifs >> s;
            curr_dir->files.push_back(std::make_pair(s, size));
        }
    }

    std::cout << "part 1: " << part1(root) << '\n';
    std::cout << "part 2: " << part2() << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return 1;
}
