#include <iostream>

class I {
public:
  virtual ~I() {}
  virtual int toInt() const = 0;
};

class A : public I {
public:
  virtual ~A() {}
  virtual int toInt() const { return 1; };
};

class B : public I {
public:
  virtual ~B() {}
  virtual int toInt() const { return 2; };
};

int add(const I& x, const I& y) {
  return x.toInt() + y.toInt();
}

int main () {
  A a;
  B b;
  std::cout << add(a, a) << std::endl;
  std::cout << add(b, b) << std::endl;
  std::cout << add(a, b) << std::endl;  // 컴파일 오류로 하고 싶다.
  return 0;
}
