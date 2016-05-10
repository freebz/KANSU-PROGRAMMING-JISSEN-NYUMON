// sample.cpp
// $ g++ -std=c++0x powerof2.cpp sample.cpp -o sample
// $ ./sample
// 3 is invalid
//
#include <iostream>
#include "PowerOf2.hpp"

int main() {
  boost::optional< PowerOf2 > x = makePowerOf2(3);
  if (x) std::cout << exponentPowerOf2(*x) << std::endl;
  else   std::cout << "3 is invalid" << std::endl;
  return 0;
}
