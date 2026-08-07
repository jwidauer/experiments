#pragma once

#include <algorithm>
#include <array>
#include <bitset>
#include <climits>
#include <concepts>
#include <cstddef>
#include <functional>
#include <iterator>
#include <limits>
#include <ranges>
#include <tl/optional.hpp>
#include <utility>

#include "ctr/maybe_uninit.hpp"
#include "ctr/type_traits.hpp"

namespace ctr {

template <class Key, class Value, std::size_t N, class Equal = std::equal_to<Key>>
struct InplaceUnorderedMap {
 private:
 public:
  template <bool IsConst>
  struct Iterator;

  // NOLINTBEGIN(readability-identifier-naming) STL compatibility
  using key_type = Key;
  using mapped_type = Value;
  using reference = std::pair<const key_type&, mapped_type&>;
  using const_reference = std::pair<const key_type&, const mapped_type&>;
  using size_type = std::size_t;
  using difference_type = std::ptrdiff_t;
  using iterator = Iterator<false>;
  using const_iterator = Iterator<true>;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;
  // NOLINTEND(readability-identifier-naming)

  // Iterators
  constexpr auto begin [[nodiscard]] () -> iterator { return iterator{this, 0}; }
  constexpr auto begin [[nodiscard]] () const -> const_iterator { return const_iterator{this, 0}; }
  constexpr auto cbegin [[nodiscard]] () const -> const_iterator { return begin(); }

  constexpr auto end [[nodiscard]] () -> iterator { return iterator{this, size()}; }
  constexpr auto end [[nodiscard]] () const -> const_iterator { return const_iterator{this, size()}; }
  constexpr auto cend [[nodiscard]] () const -> const_iterator { return end(); }

  // Capacity
  constexpr auto empty [[nodiscard]] () const -> bool { return taken_slots_.none(); }
  constexpr auto size [[nodiscard]] () const -> size_type { return taken_slots_.count(); }
  constexpr auto max_size [[nodiscard]] () const -> size_type { return taken_slots_.size(); }
  constexpr auto full [[nodiscard]] () const -> bool { return taken_slots_.all(); }

  // Element access
  constexpr auto operator[](const Key& k) -> mapped_type& { return this->operator[]<Key>(k); }
  constexpr auto operator[](Key&& k) -> mapped_type& { return this->operator[]<Key>(std::move(k)); }
  template <class K>
    requires std::same_as<K, Key> || TransparentComparator<Equal>
  constexpr auto operator[](K&& k) -> mapped_type& {
    return try_emplace(std::forward<K>(k)).first->second;
  }

  // Modifiers
  constexpr void clear() {
    for (const auto& idx : taken_indices()) {
      std::destroy_at(keys_[idx].data());
      std::destroy_at(values_[idx].data());
    }
    taken_slots_.reset();
  }

  template <class... Args>
  constexpr auto try_emplace(const key_type& k, Args&&... args) -> std::pair<iterator, bool> {
    return try_emplace<key_type>(k, std::forward<Args>(args)...);
  }
  template <class... Args>
  constexpr auto try_emplace(key_type&& k, Args&&... args) -> std::pair<iterator, bool> {
    return try_emplace<key_type>(std::move(k), std::forward<Args>(args)...);
  }
  template <class K, class... Args>
    requires(std::same_as<K, key_type> || (TransparentComparator<Equal> && std::constructible_from<key_type, K>))
  constexpr auto try_emplace(K&& k, Args&&... args) -> std::pair<iterator, bool> {
    if (full()) return {end(), false};  // Map is full

    if (const auto it = find(k); it != end()) return {it, false};  // Key already exists

    // Key does not exist, find the first available index to insert the new key-value pair
    const auto new_index = first_unused_idx();

    std::construct_at(keys_[new_index].data(), std::forward<K>(k));
    std::construct_at(values_[new_index].data(), std::forward<Args>(args)...);
    taken_slots_.set(new_index);

    return {iterator{this, new_index}, true};
  }

  // Lookup
  template <class Self>
  constexpr auto find [[nodiscard]] (this Self& self, const key_type& k) -> Iterator<std::is_const_v<Self>> {
    return self.template find<key_type>(k);
  }
  template <class Self, class K>
    requires(std::same_as<K, key_type> || TransparentComparator<Equal>)
  constexpr auto find [[nodiscard]] (this Self& self, const K& k) -> Iterator<std::is_const_v<Self>> {
    const auto is_equal = [&](auto idx) -> bool { return self.equal_(*self.keys_[idx], k); };
    const auto idx_it = std::ranges::find_if(self.taken_indices(), is_equal);

    return idx_it != indices.end() ? iterator{&self, *idx_it} : self.end();
  }

  constexpr auto contains [[nodiscard]] (const key_type& k) const -> bool { return contains<key_type>(k); }
  template <class K>
    requires(std::same_as<K, key_type> || TransparentComparator<Equal>)
  constexpr auto contains [[nodiscard]] (const K& k) const -> bool {
    return find(k) != end();
  }

 private:
  template <class T>
  static consteval auto bit_size [[nodiscard]] () -> std::size_t {
    return std::numeric_limits<T>::digits;
  }

  constexpr auto taken_indices [[nodiscard]] () const {
    const auto is_set = [this](auto idx) -> bool { return taken_slots_[idx]; };
    return indices | std::views::filter(is_set);
  }

  constexpr auto first_unused_idx [[nodiscard]] () const -> std::size_t {
    if constexpr (N <= bit_size<unsigned long>()) {  // NOLINT(google-runtime-int) std::bitset uses ulong
      return std::countr_one(taken_slots_.to_ulong());
    } else if constexpr (N <= bit_size<unsigned long long>()) {  // NOLINT(google-runtime-int) std::bitset uses ullong
      return std::countr_one(taken_slots_.to_ullong());
    } else {
      auto idx_iter = std::ranges::find_if(indices, [this](auto idx) -> bool { return !taken_slots_[idx]; });
      assert(idx_iter != indices.end());  // There should be at least one available index
      return *idx_iter;
    }
  }

  static constexpr auto indices = std::views::iota(0UZ, N);

  std::array<MaybeUninit<Key>, N> keys_;
  std::array<MaybeUninit<Value>, N> values_;

  std::bitset<N> taken_slots_;
  [[no_unique_address]] Equal equal_;
};

}  // namespace ctr
