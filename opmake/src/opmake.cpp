#include "opmake/opmake.hpp"

#include <iostream>

#include "internal.hpp"

namespace opmake {

void build() {
  configure<42>();

  std::cout << "Building with Opmake!\n";
}

}  // namespace opmake
