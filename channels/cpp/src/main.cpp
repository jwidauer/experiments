#include <latch>
#include <thread>

#include "channel.hpp"
#include "spsc.hpp"

struct Foo {
  std::size_t y;
};

namespace {
void test_st() {
  moodycamel::SPSC<Foo, 9> queue;

  constexpr auto cap = queue.max_capacity();

  for (std::size_t i = 0; i < cap; ++i) {
    assert(queue.try_enqueue({.y = i}));
  }
  assert(!queue.try_enqueue({.y = cap}));

  Foo f;
  assert(queue.try_dequeue(f));
  assert(f.y == 0);
  assert(queue.try_enqueue({.y = cap}));

  assert(!queue.try_enqueue({.y = cap}));

  for (std::size_t i = 0; i < cap; ++i) {
    assert(queue.try_dequeue(f));
    assert(f.y == (i + 1));
  }
  assert(!queue.try_dequeue(f));
}

void test_mt() {
  moodycamel::SPSC<Foo, 9> queue;

  std::latch start{1};

  std::jthread producer([&]() {
    start.wait();
    for (std::size_t i = 0; i < queue.max_capacity(); ++i) {
      while (!queue.try_enqueue({.y = i})) {
        std::this_thread::yield();  // Wait until there is space
      }
    }
  });
  std::jthread consumer([&]() {
    start.wait();
    for (std::size_t i = 0; i < queue.max_capacity(); ++i) {
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
  test_st();
  test_mt();

  auto [side1, side2] = channel::create<Foo, 9>();

  side1.send({.y = 42});
  auto opt = side2.recv();
  assert(opt.has_value());
  assert(opt->y == 42);

  return 0;
}
