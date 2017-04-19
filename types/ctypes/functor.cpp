// A C++ functor

class A {
public:
    void b(int c) {}
};

struct wrapper {
  A* pA;
  void (A::*pF)(int);
  void operator()(int c) { (pA->*pF)(c); }
  wrapper(A* pA, void(A::*pF)(int)) : pA(pA), pF(pF) {}
};

int main () {
  A a1;
  A a2;

  wrapper w1(&a1, &A::b);
  wrapper w2(&a2, &A::b);

  w1(3);
  w2(7);
}