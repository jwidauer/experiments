#include <array>
#include <bitset>
#include <cstring>
#include <iostream>
#include <string_view>

template <typename T, size_t sz = sizeof(T)>
void print_bin(std::string_view name, T *e) {
  std::cout << name << ": size: " << sizeof(T) << " val: '" << *e << "'\n  ";
  std::cout << std::bitset<sz * 8>(*e) << "\n  ";
  auto p = (uint8_t *)e;
  for (size_t i = 0; i < sz; ++i) {
    std::cout << std::bitset<8>(p[i]) << " ";
  }
  std::cout << "\n";
}

uint64_t swap_uint64(uint64_t val) {
  val = ((val << 8) & 0xFF00FF00FF00FF00ULL) |
        ((val >> 8) & 0x00FF00FF00FF00FFULL);
  val = ((val << 16) & 0xFFFF0000FFFF0000ULL) |
        ((val >> 16) & 0x0000FFFF0000FFFFULL);
  return (val << 32) | (val >> 32);
}

uint32_t getbits(const uint8_t *buff, int pos, int len) {
  // Example:
  // pos: 11, len: 27
  // 01100010 01101111 01101111 01100010 01110011
  //             ^----------------------------^
  // result: 011 11011011 11011000 10011100

  const uint8_t *p = buff + (pos / 8);
  int n_bytes = (len + 7) / 8;

  uint64_t res = 0;
  // for (int i = 0; i < n_bytes; ++i) {
  //   res <<= 8;
  //   res |= p[i];
  // }
  // Do the above in a single line using memcpy
  memcpy((uint8_t *)&res + (sizeof(res) - n_bytes), p, n_bytes);
  print_bin("res", &res);

  res = swap_uint64(res);
  print_bin("res", &res);

  res >>= 8 - ((pos + len) % 8);

  res &= (1 << len) - 1;

  return (uint32_t)res;
}

// Type your code here, or load an example.
int main() {
  std::array<uint8_t, 8> c{'b', 'o', 'o', 'b', 's', 'a', 'r', 'e'};
  print_bin<uint64_t, 8>("orig", (uint64_t *)c.data());

  auto res = getbits(c.data(), 11, 27);

  print_bin("res", &res);

  uint32_t expected = 0b011110110111101100010011100;

  print_bin("expected", &expected);

  return 0;
}
