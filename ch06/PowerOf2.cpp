// PowerOf2.cpp
#include "PowerOf2.hpp"

PowerOf2::PowerOf2(const PowerOf2& src) : n(src.n) {}

PowerOf2::~PowerOf2() {}

PowerOf2::PowerOf2(uint64_t n) : n(n) {}

boost::optional< PowerOf2 > makePowerOf2(uint64_t n) {
  uint64_t x = n;
  if (x == 0) return boost::optional< PowerOf2 >();
  while (x % 2 == 0) x /= 2;
  if (x > 1) return boost::optional< PowerOf2 >();
  else       return boost::optional< PowerOf2 >(PowerOf2(n));
}

uint32_t exponentPowerOf2(const PowerOf2& n) {
  uint32_t x = n.n;
  uint32_t result = 0;
  while (x % 2 == 0) { x /= 2; ++result; }
  return result;
}
