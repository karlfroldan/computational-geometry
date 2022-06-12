#include <iostream>
#include <string>

#include "../geolib/types.hpp"

int main(int argc, char* argv[]) {

    double a, b;

    while (std::cin >> a >> b) {
        Rational p(a), q(b);

        std::cout << p << ' ' << q << '\n';
    }


    return EXIT_SUCCESS;
}