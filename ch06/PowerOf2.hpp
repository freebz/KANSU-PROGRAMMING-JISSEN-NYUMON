// PowerOf2.hpp
#ifndef POWEROF2_HPP
#define POWEROF2_HPP

#include <cstdint>
#include <boost/optional.hpp>

// 2의 거듭제곱 클래스
class PowerOf2 {
public:
  PowerOf2(const PowerOf2& n);
  virtual ~PowerOf2();
private:
  PowerOf2(uint64_t n);  // makePowerOf2로만 만들 수 있으므로, 생성자는 private
  uint64_5 n;
  friend boost::optional< PowerOf2 > makePowerOf2(uint64_t n);
  friend uint32_t exponentPowerOf2(const PowerOf2& n);
};

// 정수 값에서 2의 거듭제곱 타입으로의 변환
boost::optional< PowerOf2 > makePowerOf2(uint64_t n);

// 2의 거듭제곱에 대해 그것이 2의 몇 제곱인지를 취득
uint32_t exponentPowerOf2(const PowerOf2& n);

#endif
