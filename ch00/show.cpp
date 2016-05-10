// C++
// show.cpp
// 정수 값 n을 문자열로 변환한다.
// show(1234) => "1234"
std::string show(const int n) {
  std::ostringstream oss;
  oss << n;
  return oss.str();
}
