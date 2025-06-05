// Large parts of this file is borrowed from the public domain code below.
// from https://github.com/mstump/queues

// C++ implementation of Dmitry Vyukov's non-intrusive
// lock free unbound MPSC queue
// http://www.1024cores.net/home/
// lock-free-algorithms/queues/non-intrusive-mpsc-node-based-queue

#ifndef UTIL_MPSC_H_
#define UTIL_MPSC_H_

#include <atomic>
#include <cassert>
#include <memory>
#include <type_traits>

/**
 * Multiple Producer Single Consumer Lockless Q
 */
template <typename T>
class MpscQueue {
 public:
  struct BufferNode {
    T data;
    std::atomic<BufferNode*> next;
  };

  MpscQueue() {
    auto* al_st = new buffer_node_aligned_t;
    auto* node = std::construct_at<BufferNode>(al_st);
    head_.store(node);
    tail_.store(node);

    node->next.store(nullptr, std::memory_order_relaxed);
  }

  MpscQueue(const MpscQueue&) = delete;
  auto operator=(const MpscQueue&) -> MpscQueue& = delete;

  ~MpscQueue() {
    T output;
    while (this->dequeue(&output)) {
    }
    BufferNode* front = head_.load(std::memory_order_relaxed);
    std::destroy_at(front);

    delete front;
  }

  void enqueue(const T& input) {
    auto* al_st = new buffer_node_aligned_t;
    auto* node = std::construct_at<BufferNode>(al_st, input, nullptr);

    BufferNode* prev_head = head_.exchange(node, std::memory_order_acq_rel);
    prev_head->next.store(node, std::memory_order_release);
  }

  auto dequeue(T* output) -> bool {
    BufferNode* tail = tail_.load(std::memory_order_relaxed);
    BufferNode* next = tail->next.load(std::memory_order_acquire);

    if (next == nullptr) {
      return false;
    }

    *output = next->data;
    tail_.store(next, std::memory_order_release);

    std::destroy_at(tail);

    delete tail;
    return true;
  }

  // you can only use pop_all if the queue is SPSC
  auto pop_all() -> BufferNode* {
    // nobody else can move the tail pointer.
    BufferNode* tptr = tail_.load(std::memory_order_relaxed);
    BufferNode* next = tptr->next.exchange(nullptr, std::memory_order_acquire);
    head_.exchange(tptr, std::memory_order_acquire);

    // there is a race condition here
    return next;
  }

 private:
  using buffer_node_aligned_t = std::aligned_storage_t<sizeof(BufferNode), alignof(BufferNode)>;

  std::atomic<BufferNode*> head_;
  std::atomic<BufferNode*> tail_;
};

#endif  // UTIL_MPSC_H_
