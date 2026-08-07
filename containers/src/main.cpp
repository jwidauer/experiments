#include <cstddef>
#include <functional>
#include <memory>
#include <print>
#include <type_traits>

#include "ctr/alloc.hpp"
#include "ctr/function.hpp"
#include "ctr/id.hpp"
#include "ctr/soa_vec.hpp"
#include "ctr/static_vector.hpp"
#include "ctr/uninitialized_array.hpp"
#include "tl/optional.hpp"

namespace {

consteval auto make_data() -> ctr::UninitializedArray<int, 10> {
  ctr::UninitializedArray<int, 10> arr;
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
  bool b{false};
};

constexpr auto make_s_arr() -> ctr::UninitializedArray<S, 10> {
  ctr::UninitializedArray<S, 10> arr;
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

struct TagA {};
struct TagB {};

}  // namespace

auto main() -> int {
  ctr::Smallocator<sizeof(void*), 10> alloc;
  using Storage = ctr::AllocatedStorage<decltype(alloc)>;
  ctr::Function<int(int, int), Storage> add{[](int a, int b) -> int { return a + b; }, alloc};
  ctr::Function<int(int, int), ctr::InlineStorage<1>> sub{};

  auto func = ctr::make_function<int(int, int)>([](int a, int b) -> int { return a * b; });

  auto vec = ctr::SoaVec<10, int, float, S>{};

  for (int i = 0; i < 5; ++i) vec.try_push_back(i, i + 0.5F, S{i, i + 1.0F});

  auto i = vec.at<int>(2);
  static_assert(std::is_same_v<decltype(i), tl::optional<int&>>);

  auto f = vec.at<1>(3);
  static_assert(std::is_same_v<decltype(f), tl::optional<float&>>);

  ctr::StaticVector<int, 5> static_vec;
  for (int i = 0; i < 5; ++i) {
    static_vec.try_push_back(i * 10);
  }

  using AId = ctr::Id<TagA>;
  using BId = ctr::Id<TagB>;

  auto a1 = AId::create();
  auto a2 = AId::create();

  auto b1 = BId::create();
  auto b2 = BId::create();

  std::println("AId: {}, {}", a1, a2);
  std::println("BId: {}, {}", b1, b2);

  return 0;
}
