#include <latch>
#include <thread>

#include "spsc.hpp"

struct Foo {
  int y;
};

namespace {

void test() {
  moodycamel::SPSC<Foo, 8> queue;

  std::latch start{1};

  std::jthread producer([&]() {
    start.wait();
    for (int i = 0; i < 8; ++i) {
      while (!queue.try_enqueue({.y = i})) {
        std::this_thread::yield();  // Wait until there is space
      }
    }
  });
  std::jthread consumer([&]() {
    start.wait();
    for (int i = 0; i < 8; ++i) {
      Foo f;
      while (!queue.try_dequeue(f)) {
        std::this_thread::yield();  // Wait until there is an item
      }
      assert(f.y == i);
    }
  });
  start.count_down();
}

}  // namespace

auto main() -> int {
  test();
  return 0;
}
