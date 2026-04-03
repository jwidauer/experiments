#include "task.hpp"

namespace {

// Simulated async operation
auto async_compute(int x) -> Task<int> {
  // In real code, this would suspend and resume when I/O completes
  co_return x * 2;
}

// Composition
auto compute_sum(int a, int b) -> Task<int> {
  int x = co_await async_compute(a);
  int y = co_await async_compute(b);
  co_return x + y;
}

// Sequential operations
// auto do_work() -> Task<void> {
//   auto result = co_await compute_sum(10, 20);
//   std::println("Result: {}", result);
//   co_return;
// }
//
// // Error handling
// auto may_fail(bool should_fail) -> Task<int> {
//   if (should_fail) throw std::runtime_error("Operation failed");
//
//   co_return 42;
// }
//
// auto handle_errors() -> Task<void> {
//   try {
//     auto result = co_await may_fail(true);
//     std::println("Got: {}", result);
//   } catch (const std::exception& e) {
//     std::println("Error: {}", e.what());
//   }
// }

}  // namespace

auto main() -> int {
  [[maybe_unused]] auto t = compute_sum(10, 20).sync_wait();
  // std::println("Synchronous result: {}", result);
  return 0;
}
