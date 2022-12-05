#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <stack>
#include <algorithm>
#include <numeric>

using Column = std::stack<char>;

int main() try {
	std::ifstream ifs{ "input.txt" };
	if (!ifs) throw std::runtime_error{ "failed to open input file" };

	std::vector<std::string> lines;
	for (std::string line; std::getline(ifs, line);) {
		if (line.empty()) break;
		lines.push_back(line);
	}

	std::vector<Column> v{ (lines.back().size() + 1) / 4 };

	std::for_each(lines.rbegin() + 1, lines.rend(),
		[&v](const std::string& line) {
			for (size_t i = 0; i < v.size(); ++i) {
				char ch = line[1 + 4 * i];
				if (!std::isspace(ch)) {
					v[i].push(ch);
				}
			}
		});

	while (true) {
		std::string s;
		int count, from, to;
		ifs >> s >> count >> s >> from >> s >> to;
		if (!ifs) break;
		
		std::vector<char> vec;
		for (std::size_t i = 0; i < count; ++i) {
			vec.push_back(v[from - 1].top());
			v[from - 1].pop();
		}


		std::reverse(vec.begin(), vec.end());
		for (auto ch : vec) {
			v[to - 1].push(ch);
		}
	}

	for (auto col : v) {
		if (col.size() == 0) continue;
		std::cout << col.top();
	}
}
catch (std::exception& e) {
	std::cerr << e.what() << '\n';
	return 0;
}
