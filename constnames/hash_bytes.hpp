// This file defines Hash_bytes, a primitive used for defining hash
// functions. Based on public domain MurmurHashUnaligned2, by Austin
// Appleby.  http://murmurhash.googlepages.com/

// This file also defines _Fnv_hash_bytes, another primitive with
// exactly the same interface but using a different hash algorithm,
// Fowler / Noll / Vo (FNV) Hash (type FNV-1a). The Murmur hash
// function apears to be better in both speed and hash quality, and
// FNV is provided primarily for backward compatibility.

#include <cstdint>
#include <print>

namespace detail {

template <typename T>
inline auto unaligned_load(const char* p) -> T {
  T result;
  __builtin_memcpy(&result, p, sizeof(result));
  return result;
}

// Loads n bytes, where 1 <= n < 8.
inline auto load_bytes(const char* p, int n) -> std::size_t {
  std::size_t result = 0;
  --n;
  do result = (result << 8) + static_cast<unsigned char>(p[n]);
  while (--n >= 0);
  return result;
}

inline auto shift_mix(std::size_t v) -> std::size_t { return v ^ (v >> 47); }

}  // namespace detail

// Implementation of Murmur hash for 32-bit size_t.
inline auto hash_bytes32(const void* ptr, uint32_t len, uint32_t seed) -> uint32_t {
  const uint32_t m = 0x5bd1e995;
  uint32_t hash = seed ^ len;
  const char* buf = static_cast<const char*>(ptr);

  // Mix 4 bytes at a time into the hash.
  while (len >= 4) {
    auto k = detail::unaligned_load<uint32_t>(buf);
    k *= m;
    k ^= k >> 24;
    k *= m;
    hash *= m;
    hash ^= k;
    buf += 4;
    len -= 4;
  }

  uint32_t k;
  // Handle the last few bytes of the input array.
  switch (len) {
    case 3:
      k = static_cast<unsigned char>(buf[2]);
      hash ^= k << 16;
      [[gnu::fallthrough]];
    case 2:
      k = static_cast<unsigned char>(buf[1]);
      hash ^= k << 8;
      [[gnu::fallthrough]];
    case 1:
      k = static_cast<unsigned char>(buf[0]);
      hash ^= k;
      hash *= m;
  };

  // Do a few final mixes of the hash.
  hash ^= hash >> 13;
  hash *= m;
  hash ^= hash >> 15;
  return hash;
}

// Implementation of Murmur hash for 64-bit size_t.
inline auto hash_bytes64(const void* ptr, uint64_t len, uint64_t seed) -> uint64_t {
  static const uint64_t kMul = ((static_cast<uint64_t>(0xc6a4a793UL)) << 32UL) + static_cast<uint64_t>(0x5bd1e995UL);
  const char* const buf = static_cast<const char*>(ptr);

  // Remove the bytes not divisible by the sizeof(uint64_t).  This
  // allows the main loop to process the data as 64-bit integers.
  const uint64_t len_aligned = len & ~static_cast<uint64_t>(0x7);
  const char* const end = buf + len_aligned;

  uint64_t hash = seed ^ (len * kMul);
  for (const char* p = buf; p != end; p += 8) {
    const uint64_t data = detail::shift_mix(detail::unaligned_load<uint64_t>(p) * kMul) * kMul;
    hash ^= data;
    hash *= kMul;
  }
  // 0x7 == 0b0111
  if ((len & 0x7) != 0) {
    const uint64_t data = detail::load_bytes(end, len & 0x7);
    hash ^= data;
    hash *= kMul;
  }
  hash = detail::shift_mix(hash) * kMul;
  hash = detail::shift_mix(hash);
  return hash;
}

inline auto hash_bytes(const void* ptr, std::size_t len, std::size_t seed = 0) -> std::size_t {
  if constexpr (sizeof(std::size_t) == 4) {
    return hash_bytes32(ptr, static_cast<uint32_t>(len), static_cast<uint32_t>(seed));
  } else {
    return hash_bytes64(ptr, static_cast<uint64_t>(len), static_cast<uint64_t>(seed));
  }
}
