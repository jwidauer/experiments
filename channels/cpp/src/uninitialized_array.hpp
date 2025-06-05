#ifndef INCLUDE_UNINITIALIZED_ARRAY_HPP_
#define INCLUDE_UNINITIALIZED_ARRAY_HPP_

#include <array>
#include <optional>

template <std::size_t Len, std::size_t Align>
struct AlignedStorage {
  constexpr auto data [[nodiscard]] () -> std::byte* { return data_.data(); }
  constexpr auto data [[nodiscard]] () const -> const std::byte* { return data_.data(); }

 private:
  alignas(Align) std::array<std::byte, Len> data_;
};

template <typename T, std::size_t N>
class UninitializedArray {
 public:
  using value_type = T;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;
  using reference = value_type&;
  using const_reference = const value_type&;
  using pointer = value_type*;
  using const_pointer = const value_type*;

  constexpr auto at [[nodiscard]] (std::size_t idx) -> std::optional<T*> {
    return idx < N ? data() + idx : std::nullopt;
  }
  constexpr auto at [[nodiscard]] (std::size_t idx) const -> std::optional<const T*> {
    return idx < N ? data() + idx : std::nullopt;
  }

  constexpr auto operator[] [[nodiscard]] (std::size_t idx) -> T* { return data() + idx; }
  constexpr auto operator[] [[nodiscard]] (std::size_t idx) const -> const T* { return data() + idx; }

  constexpr auto begin [[nodiscard]] () -> iterator { return data(); }

  constexpr auto size [[nodiscard]] () const -> std::size_t { return N; }
  constexpr auto data [[nodiscard]] () -> T* {
    if constexpr (is_sufficiently_trivial) {
      return storage_.data();
    } else {
      return reinterpret_cast<T*>(storage_.data());
    }
  }
  auto data [[nodiscard]] () const -> const T* {
    if constexpr (is_sufficiently_trivial) {
      return storage_.data();
    } else {
      return reinterpret_cast<T*>(storage_.data());
    }
  }

 private:
  static constexpr bool is_sufficiently_trivial =
      std::is_trivially_default_constructible_v<T> && std::is_trivially_destructible_v<T>;

  using storage_t =
      std::conditional_t<is_sufficiently_trivial, std::array<T, N>, AlignedStorage<sizeof(T) * N, alignof(T)>>;

  [[no_unique_address]] storage_t storage_;
};

#endif  // INCLUDE_UNINITIALIZED_ARRAY_HPP_
