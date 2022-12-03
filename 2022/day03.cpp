#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <numeric>

int main() try {
    std::ifstream ifs{ "input.txt" };
    if (!ifs) throw std::runtime_error{ "failed to open input file" }; 

    std::vector<std::string> lines(std::istream_iterator<std::string>{ifs},
                                   std::istream_iterator<std::string>{});
    
    /* part 1 */
    std::vector<int> v1;
    for (const std::string& line : lines) {
        const std::string half1 = line.substr(0, line.size() / 2);
        const std::string half2 = line.substr(line.size() / 2);

        for (char ch : half1) {
            if (half2.find(ch) != std::string::npos) {
                v1.push_back(std::islower(ch) ? ch - 'a' + 1 : ch - 'A' + 27);
                break;
            }
        }
    }

    /* part 2 */
    std::vector<int> v2;
    for (std::size_t i = 2; i < lines.size(); i += 3) {
        const std::string& line1 = lines[i - 2];
        const std::string& line2 = lines[i - 1];
        const std::string& line3 = lines[i];

        for (char ch : line1) {
            if (line2.find(ch) == std::string::npos) continue;
            if (line3.find(ch) == std::string::npos) continue;
            v2.push_back(std::islower(ch) ? ch - 'a' + 1 : ch - 'A' + 27);
            break;
        }
    }

    std::cout << "part 1: " << std::accumulate(v1.begin(), v1.end(), 0) << '\n';
    std::cout << "part 2: " << std::accumulate(v2.begin(), v2.end(), 0) << '\n';
}
catch (std::exception& e) {
    std::cerr << e.what() << '\n';
    return 1;
}
 
