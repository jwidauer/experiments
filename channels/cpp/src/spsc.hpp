// ©2020 Cameron Desrochers.
// Distributed under the simplified BSD license (see the license file that
// should have come with this header).

// Provides a C++11 implementation of a single-producer, single-consumer wait-free concurrent
// circular buffer (fixed-size queue).

#pragma once

#include <atomic>
#include <cmath>
#include <memory>
#include <utility>

#include "uninitialized_array.hpp"

namespace moodycamel {

template <typename T, std::size_t N>
class SPSC {
 public:
  using value_type = T;  // NOLINT(readability-identifier-naming)

  static constexpr std::size_t capacity =
      std::bit_ceil(N);                              // round up to next power of two for efficient modulo operations
  static constexpr std::size_t mask = capacity - 1;  // mask for circular buffer (power of two minus one)

  SPSC() = default;

  SPSC(SPSC&& other) noexcept : slots_{0}, items_{0} { swap(other); }

  SPSC(SPSC const&) = delete;

  // Note: The queue should not be accessed concurrently while it's
  // being deleted. It's up to the user to synchronize this.
  ~SPSC() {
    for (std::size_t i = 0, n = size(); i != n; ++i) std::destroy_at(at(next_item_ + i));
  }

  auto operator=(SPSC&& other) noexcept -> SPSC& {
    swap(other);
    return *this;
  }

  auto operator=(SPSC const&) -> SPSC& = delete;

  // Enqueues a single item (by copying it).
  // Fails if not enough room to enqueue.
  // Thread-safe when called by producer thread.
  // No exception guarantee (state will be corrupted) if constructor of T throws.
  auto try_enqueue(T const& item) -> bool {
    if (slots_.load(std::memory_order::relaxed) <= 0) return false;
    slots_.fetch_sub(1, std::memory_order::acquire);

    inner_enqueue(item);
    return true;
  }

  // Enqueues a single item (by moving it, if possible).
  // Fails if not enough room to enqueue.
  // Thread-safe when called by producer thread.
  // No exception guarantee (state will be corrupted) if constructor of T throws.
  auto try_enqueue(T&& item) -> bool {
    if (slots_.load(std::memory_order::relaxed) <= 0) return false;
    slots_.fetch_sub(1, std::memory_order::acquire);

    inner_enqueue(std::move(item));
    return true;
  }

  // Attempts to dequeue a single item.
  // Returns false if the buffer is empty.
  // Thread-safe when called by consumer thread.
  // No exception guarantee (state will be corrupted) if assignment operator of U throws.
  template <typename U>
  auto try_dequeue(U& item) -> bool {
    if (items_.load(std::memory_order::relaxed) <= 0) return false;
    items_.fetch_sub(1, std::memory_order::acquire);

    inner_dequeue(item);
    return true;
  }

  // Returns a pointer to the next element in the queue (the one that would
  // be removed next by a call to `try_dequeue` or `try_pop`). If the queue
  // appears empty at the time the method is called, returns nullptr instead.
  // Thread-safe when called by consumer thread.
  auto peek() -> T* {
    if (size() == 0U) return nullptr;
    return at(next_item_);
  }

  // Pops the next element from the queue, if there is one.
  // Thread-safe when called by consumer thread.
  auto try_pop() -> bool {
    if (items_.load(std::memory_order::relaxed) <= 0) return false;
    items_.fetch_sub(1, std::memory_order::acquire);

    inner_pop();
    return true;
  }

  // Returns a (possibly outdated) snapshot of the total number of elements currently in the buffer.
  // Thread-safe.
  auto size [[nodiscard]] () const -> std::size_t {
    auto size = items_.load(std::memory_order::relaxed);
    return size > 0 ? static_cast<std::size_t>(size) : 0;
  }

  // Returns the maximum number of elements that this circular buffer can hold at once.
  // Thread-safe.
  consteval auto max_capacity [[nodiscard]] () const -> std::size_t { return N; }

 private:
  constexpr auto idx [[nodiscard]] (this auto& self, std::size_t idx) -> std::size_t { return idx & self.mask; }

  constexpr auto at [[nodiscard]] (this auto& self, std::size_t idx) -> decltype(auto) {
    return self.data_.data() + self.idx(idx);
  }

  template <typename U>
  void inner_enqueue(U&& item) {
    auto i = next_slot_++;
    std::construct_at<T>(at(i), std::forward<U>(item));
    items_.fetch_add(1, std::memory_order::release);
  }

  template <typename U>
  void inner_dequeue(U& item) {
    auto i = next_item_++;
    T* element = at(i);
    item = std::move(*element);
    std::destroy_at(element);
    slots_.fetch_add(1, std::memory_order::release);
  }

  void inner_pop() {
    auto i = next_item_++;
    std::destroy_at(at(i));
    slots_.fetch_add(1, std::memory_order::release);
  }

  std::atomic<ssize_t> slots_{N};           // number of slots currently free
  std::atomic<ssize_t> items_{0};           // number of elements currently enqueued
  std::size_t next_slot_{};                 // index of next free slot to enqueue into
  std::size_t next_item_{};                 // index of next element to dequeue from
  UninitializedArray<T, capacity> data_{};  // circular buffer memory aligned to element alignment
};

}  // namespace moodycamel
