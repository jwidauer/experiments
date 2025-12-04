#include <functional>
#include <memory>
#include <print>
#include <vector>

#include "uninitialized_array.hpp"

namespace {

consteval auto make_data() -> UninitializedArray<int, 10> {
  UninitializedArray<int, 10> arr;
  for (std::size_t i = 0; i < arr.size(); ++i) {
    arr[i] = static_cast<int>(i);
  }
  return arr;
}

struct S {
  constexpr S(int x, float y) : x(x), y(y) {}

  constexpr ~S() = default;

  int x;
  float y;
};

constexpr auto make_s_arr() -> UninitializedArray<S, 10> {
  UninitializedArray<S, 10> arr;
  for (std::size_t i = 0; i < arr.size(); ++i) {
    std::construct_at(std::addressof(arr[i]), i, i + 1);
  }
  return arr;
}

template <class R, class Proj = std::identity>
void print_range(const R& range, Proj proj = {}) {
  std::print("[");
  for (const auto& elem : range) {
    std::print("{},", std::invoke(proj, elem));
  }
  std::println("]");
}

}  // namespace

auto main(int /*argc*/, char** /*argv*/) -> int {
  constexpr auto arr = make_data();
  auto s_arr = make_s_arr();

  std::vector<S> vec;
  vec.reserve(arr.size());

  std::print("s_arr: ");
  print_range(s_arr, &S::x);

  return 0;
}
