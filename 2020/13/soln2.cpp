#include <iostream>
#include <vector>
#include <cmath>

int main(int argc, char** argv)
{
    std::vector<std::pair<unsigned int, int>> pure;
    pure.push_back(std::make_pair(911,0));
    pure.push_back(std::make_pair(827,31));
    pure.push_back(std::make_pair(41,-41));
    pure.push_back(std::make_pair(37,-6));
    pure.push_back(std::make_pair(29,29));
    pure.push_back(std::make_pair(23,23));
    pure.push_back(std::make_pair(19,50));
    pure.push_back(std::make_pair(17,14));
    pure.push_back(std::make_pair(13,13));
    int offset = 41;
    unsigned int increment = 911;
    // pure.push_back(std::make_pair(59,0));
    // pure.push_back(std::make_pair(31,2));
    // pure.push_back(std::make_pair(19,3));
    // pure.push_back(std::make_pair(13,-3));
    // pure.push_back(std::make_pair(7,-4));
    // int offset = 4;
    // unsigned int increment = 59;

    unsigned int print_count = 0;
    unsigned long timestamp = 911 * (std::floor(100000000000000/911) + 1);
    bool success = false;

    while (!success)
    {
        if (print_count++ % 10000000 == 0)
            std::cerr << timestamp << std::endl;

        // check for stop conditions
        success = true;
        for (const auto [t,m] : pure)
        {
            double val = static_cast<double>(timestamp + m) / t;
            // if ((timestamp + m) % t != 0)
            if ((val - std::floor(val)) > 0.00001)
            {
                success = false;
                break;
            }
        }

        if (!success)
            timestamp += increment;
    }
    timestamp -= offset;
    std::cerr << "Success: " << timestamp << std::endl;

}
