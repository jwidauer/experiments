#include <doctest/doctest.h>

#include <ctr/soa_vec.hpp>

TEST_SUITE_BEGIN("[soa_vec]");

TEST_CASE("soa_vec basic functionality") {
  ctr::SoaVec<5, int, float> vec;

  // Test push_back and size
  vec.push_back(1, 1.0F);
  vec.push_back(2, 2.0F);
  vec.push_back(3, 3.0F);
  CHECK(vec.size() == 3);

  // Test element access
  CHECK(vec.at<0>(0) == 1);
  CHECK(vec.at<1>(0) == 1.0F);
  CHECK(vec.at<0>(1) == 2);
  CHECK(vec.at<1>(1) == 2.0F);
  CHECK(vec.at<0>(2) == 3);
  CHECK(vec.at<1>(2) == 3.0F);

  CHECK(vec.at<int>(0) == 1);
  CHECK(vec.at<float>(0) == 1.0F);
  CHECK(vec.at<int>(1) == 2.0F);
  CHECK(vec.at<float>(1) == 2.0F);
  CHECK(vec.at<int>(2) == 3);
  CHECK(vec.at<float>(2) == 3.0F);

  // Test clear
  vec.clear();
  CHECK(vec.size() == 0);
}

TEST_CASE("soa_vec insert and remove") {
  ctr::SoaVec<5, int, float> vec;

  // Test insert
  vec.push_back(1, 1.0F);
  vec.push_back(3, 3.0F);
  vec.try_insert(1, 2, 2.0F);  // Insert at index 1
  CHECK(vec.size() == 3);
  CHECK(vec.at<0>(1) == 2);
  CHECK(vec.at<1>(1) == 2.0F);

  // Test remove
  vec.try_erase(1);  // Remove element at index 1
  CHECK(vec.size() == 2);
  CHECK(vec.at<0>(1) == 3);
  CHECK(vec.at<1>(1) == 3.0F);
}

TEST_CASE("soa_vec find and contains") {
  ctr::SoaVec<5, int, float> vec;

  vec.push_back(1, 1.0F);
  vec.push_back(2, 2.0F);
  vec.push_back(3, 3.0F);

  // Test find
  auto index = vec.find<int>(2);
  CHECK(index.has_value());
  CHECK(*index == 1);

  index = vec.find<float>(3.0F);
  CHECK(index.has_value());
  CHECK(*index == 2);

  index = vec.find<int>(4);
  CHECK(!index.has_value());

  // Test contains
  CHECK(vec.contains<int>(1));
  CHECK(vec.contains<float>(2.0F));
  CHECK(!vec.contains<int>(4));
}

TEST_CASE("soa_vec full and empty") {
  ctr::SoaVec<3, int, float> vec;

  CHECK(vec.empty());
  CHECK(!vec.full());

  vec.push_back(1, 1.0F);
  vec.push_back(2, 2.0F);
  vec.push_back(3, 3.0F);

  CHECK(!vec.empty());
  CHECK(vec.full());

  vec.try_pop_back();
  CHECK(!vec.full());
}

TEST_CASE("soa_vec try_push_back and try_insert") {
  ctr::SoaVec<3, int, float> vec;

  // Test try_push_back
  auto res = vec.try_push_back(1, 1.0F);
  CHECK(res.has_value());
  CHECK(*res == 0);

  res = vec.try_push_back(2, 2.0F);
  CHECK(res.has_value());
  CHECK(*res == 1);

  res = vec.try_push_back(3, 3.0F);
  CHECK(res.has_value());
  CHECK(*res == 2);

  res = vec.try_push_back(4, 4.0F);  // Should fail
  CHECK(!res.has_value());

  // Test try_insert
  res = vec.try_insert(1, 5, 5.0F);  // Should fail because full
  CHECK(!res.has_value());

  vec.try_pop_back();  // Remove one element

  res = vec.try_insert(1, 5, 5.0F);  // Should succeed now
  CHECK(res.has_value());
  CHECK(*res == 1);
}

TEST_SUITE_END();
