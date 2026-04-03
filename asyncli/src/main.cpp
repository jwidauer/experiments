#include <asyncli/event_loop.hpp>
#include <cstdlib>
#include <ctr/aligned_storage.hpp>

auto main() -> int {
  auto loop = asyncli::EventLoop::create();
  if (!loop) return EXIT_FAILURE;

  return 0;
}
