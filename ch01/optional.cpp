// optional.cpp
// $ g++ -std=c++0x -pedantic -W -Wall -Wextra -o optional optional.cpp
// $ echo "1 + 1" | ./optional
// 2
// $ echo "2 - 1" | ./optional
// 1
// $ echo "3 * 3" | ./optional
// 9
// $ echo "4 / 2" | ./optional
// 2
// $ echo "4 / 0" | ./optional
// invalid

#include <boost/optional.hpp>
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <sstream>
#include <string>
#include <functional>
using namespace boost;

// 공백 문자로 분할한다. 세 개로 분할할 수 없다면 무효
// "1 + 2" -> "1", "+", "2"
auto words = [](std::string str) -> optional< std::list< std::string >> {
  std::list< std::string > results;
  split(results, str, is_any_of(" "));
  return (results.size() == 3) ?
  optional< std::list< std::string >>(results) :
  optional< std::list< std::string >>();
};

// 문자열을 정수로 변환. 할 수 없다면 무효
auto to_num = [](std::string str) -> optional< int > {
  int n = 0;
  std::istringstream iss(str);
  return ((iss >> n) && iss.oef()) ? optional< int >(n) : optional< int >();
};

// 사칙 연산. 연산할 수 없으면 무효
auto binop_add = [](int a, int b) { return optional< int >(a + b); };
auto binop_sub = [](int a, int b) { return optional< int >(a - b); };
auto binop_mul = [](int a, int b) { return optional< int >(a * b); };
auto binop_div = [](int a, int b) {
  return b ? optional< int >(a / b) : optional< int >();
};

// "+","-","*","/" 중 하나의 문자열을 연산으로 변환. 그 외는 무효
typedef std::function< optional< int >(int, int) > binop_t;
auto to_binop = [](std::string str) -> optional< binop_t > {
  if ("+" == str) return optional< binop_t >(binop_add);
  if ("-" == str) return optional< binop_t >(binop_sub);
  if ("*" == str) return optional< binop_t >(binop_mul);
  if ("/" == str) return optional< binop_t >(binop_div);
  return optional< binop_t >();
};

int main () {
  try {
    std::string expr;
    std::getline(std::cin, expr);
    auto ws = words(expr); if (!ws) throw ws;        // ws가 무효한 값일 수도 있으므로
    auto iter = ws->begin();
    auto a = to_num(*iter++); if (!a) throw a;       // a가 무효한 값일 수도 있으므로
    auto op = to_binop(*iter++); if (!op) throw op;  // op가 무효한 값일 수도 있으므로
    auto b = to_num(*iter++); if (!b) throw b;       // b가 무효한 값일 수도 있으므로
    auto n = (*op)(*a, *b); if (!n) throw n;         // n가 무효한 값일 수도 있으므로
    std::cout << *n << std::endl;
    return 0;
  } catch (...) {
    std::cout << "invalid" << std::endl;
    return 1;
  }
}
