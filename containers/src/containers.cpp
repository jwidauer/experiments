#include <functional>
#include <memory>
#include <print>

#include "function.hpp"
#include "multi_object_vector.hpp"
#include "static_vector.hpp"

namespace {

consteval auto make_data() -> UninitializedArray<int, 10> {
  UninitializedArray<int, 10> arr;
  for (std::size_t i = 0; i < arr.size(); ++i) {
    *arr[i] = static_cast<int>(i);
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
    std::construct_at(arr[i], i, i + 1);
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

auto main() -> int {
  constexpr Function<int(int, int), 1> add{[](int a, int b) -> int { return a + b; }};

  auto vec = MultiObjectVector<10, int, float, S>{};

  for (int i = 0; i < 5; ++i) vec.try_push_back(i, i + 0.5F, S{i, i + 1.0F});

  auto i = vec.at<int>(2);
  static_assert(std::is_same_v<decltype(i), tl::optional<int&>>);

  auto f = vec.at<1>(3);
  static_assert(std::is_same_v<decltype(f), tl::optional<float&>>);

  ctr::StaticVector<int, 5> static_vec;
  for (int i = 0; i < 5; ++i) {
    static_vec.try_push_back(i * 10);
  }

  return 0;
}
