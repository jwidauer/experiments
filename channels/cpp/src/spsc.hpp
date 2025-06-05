// ©2020 Cameron Desrochers.
// Distributed under the simplified BSD license (see the license file that
// should have come with this header).

// Provides a C++11 implementation of a single-producer, single-consumer wait-free concurrent
// circular buffer (fixed-size queue).

#pragma once

#include <atomic>
#include <memory>
#include <new>
#include <utility>

#ifndef MOODYCAMEL_CACHE_LINE_SIZE
#define MOODYCAMEL_CACHE_LINE_SIZE std::hardware_destructive_interference_size
#endif

namespace moodycamel {

template <typename T, std::size_t N>
class SPSC {
 public:
  using value_type = T;

  explicit SPSC() : maxcap_{N}, slots_{static_cast<ssize_t>(N)}, items_{0} {
    // Round capacity up to power of two to compute modulo mask.
    auto capacity = std::bit_ceil(N);
    mask_ = capacity++;
    rawData_ = static_cast<char*>(std::malloc((capacity * sizeof(T)) + std::alignment_of_v<T> - 1));
    data_ = align_for<T>(rawData_);
  }

  SPSC(SPSC&& other) noexcept : slots_{0}, items_{0} { swap(other); }

  SPSC(SPSC const&) = delete;

  // Note: The queue should not be accessed concurrently while it's
  // being deleted. It's up to the user to synchronize this.
  ~SPSC() {
    for (std::size_t i = 0, n = size(); i != n; ++i) std::destroy_at(at((next_item_ + i) & mask_));
    std::free(rawData_);
  }

  auto operator=(SPSC&& other) noexcept -> SPSC& {
    swap(other);
    return *this;
  }

  auto operator=(SPSC const&) -> SPSC& = delete;

  // Swaps the contents of this buffer with the contents of another.
  // Not thread-safe.
  void swap(SPSC& other) noexcept {
    std::swap(maxcap_, other.maxcap_);
    std::swap(mask_, other.mask_);
    std::swap(rawData_, other.rawData_);
    std::swap(data_, other.data_);
    slots_ = other.slots_.exchange(slots_);
    items_ = other.items_.exchange(items_);
    std::swap(next_slot_, other.next_slot_);
    std::swap(next_item_, other.next_item_);
  }

  // Enqueues a single item (by copying it).
  // Fails if not enough room to enqueue.
  // Thread-safe when called by producer thread.
  // No exception guarantee (state will be corrupted) if constructor of T throws.
  auto try_enqueue(T const& item) -> bool {
    if (slots_.load(std::memory_order::relaxed) <= 0) return false;
    slots_.fetch_add(-1, std::memory_order::acquire);

    inner_enqueue(item);
    return true;
  }

  // Enqueues a single item (by moving it, if possible).
  // Fails if not enough room to enqueue.
  // Thread-safe when called by producer thread.
  // No exception guarantee (state will be corrupted) if constructor of T throws.
  auto try_enqueue(T&& item) -> bool {
    if (slots_.load(std::memory_order::relaxed) <= 0) return false;
    slots_.fetch_add(-1, std::memory_order::acquire);

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
    items_.fetch_add(-1, std::memory_order::acquire);

    inner_dequeue(item);
    return true;
  }

  // Returns a pointer to the next element in the queue (the one that would
  // be removed next by a call to `try_dequeue` or `try_pop`). If the queue
  // appears empty at the time the method is called, returns nullptr instead.
  // Thread-safe when called by consumer thread.
  auto peek() -> T* {
    if (size() == 0U) return nullptr;
    return at(next_item_ & mask_);
  }

  // Pops the next element from the queue, if there is one.
  // Thread-safe when called by consumer thread.
  auto try_pop() -> bool {
    if (items_.load(std::memory_order::relaxed) <= 0) return false;
    items_.fetch_add(-1, std::memory_order::acquire);

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
  auto max_capacity [[nodiscard]] () const -> std::size_t { return maxcap_; }

 private:
  auto at [[nodiscard]] (std::size_t idx) -> T* { return reinterpret_cast<T*>(data_) + idx; }
  auto at [[nodiscard]] (std::size_t idx) const -> const T* { return reinterpret_cast<T*>(data_) + idx; }

  template <typename U>
  void inner_enqueue(U&& item) {
    auto i = next_slot_++;
    std::construct_at<T>(at(i & mask_), std::forward<U>(item));
    items_.fetch_add(1, std::memory_order::release);
  }

  template <typename U>
  void inner_dequeue(U& item) {
    auto i = next_item_++;
    T* element = at(i & mask_);
    item = std::move(*element);
    std::destroy_at(element);
    slots_.fetch_add(1, std::memory_order::release);
  }

  void inner_pop() {
    auto i = next_item_++;
    std::destroy_at(at(i & mask_));
    slots_.fetch_add(1, std::memory_order::release);
  }

  template <typename U>
  static auto align_for(char* ptr) -> char* {
    const std::size_t alignment = std::alignment_of_v<U>;
    return ptr + ((alignment - (reinterpret_cast<std::uintptr_t>(ptr) % alignment)) % alignment);
  }

  static constexpr std::size_t chache_line_size = std::hardware_destructive_interference_size;

  std::size_t maxcap_{};                               // actual (non-power-of-two) capacity
  std::size_t mask_{};                                 // circular buffer capacity mask (for cheap modulo)
  char* rawData_{};                                    // raw circular buffer memory
  char* data_{};                                       // circular buffer memory aligned to element alignment
  std::atomic<ssize_t> slots_;                         // number of slots currently free
  std::atomic<ssize_t> items_;                         // number of elements currently enqueued
  alignas(chache_line_size) std::size_t next_slot_{};  // index of next free slot to enqueue into
  alignas(chache_line_size) std::size_t next_item_{};  // index of next element to dequeue from
};

}  // namespace moodycamel
