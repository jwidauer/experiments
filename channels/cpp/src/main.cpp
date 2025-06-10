#include "spsc.hpp"

struct Foo {
  double x;
  int y;
  char z;
};

auto main() -> int {
  moodycamel::SPSC<Foo, 8> queue;

  return 0;
}
