#include <iostream>

#define PROJECT_NAME "containers"

auto main(int argc, char** argv) -> int {
  if (argc != 1) {
    std::cout << argv[0] << "takes no arguments.\n";
    return 1;
  }
  std::cout << "This is project " << PROJECT_NAME << ".\n";
  return 0;
}
