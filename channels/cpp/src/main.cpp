#include "spsc.hpp"
#include "uninitialized_array.hpp"

struct Foo {
  // Make Foo nontrivial to test the uninitialized array
  Foo() : x(0), y(0.0), z('a') {}

  int x;
  double y;
  char z;
};

auto main() -> int {
  moodycamel::SPSC<int, 10> queue;

  UninitializedArray<Foo, 10> arr;
  auto f = arr[0];
  [[maybe_unused]] auto end = arr.end();

  return 0;
}
