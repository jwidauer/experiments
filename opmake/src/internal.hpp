#pragma once

#include <cstddef>
#include <iostream>

template <std::size_t N>
void configure() {
  // Configuration logic for size N
  std::cout << "Configuring with size: " << N << "\n";
}
