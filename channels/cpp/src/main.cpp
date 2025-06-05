#include <string>

#include "mpsc.hpp"
#include "spsc.hpp"

auto main() -> int {
  moodycamel::SPSC<int, 10> queue;

  std::string s{"test"};
  return 0;
}
