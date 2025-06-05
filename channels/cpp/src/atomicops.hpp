// ©2013-2016 Cameron Desrochers.
// Distributed under the simplified BSD license (see the license file that
// should have come with this header).
// Uses Jeff Preshing's semaphore implementation (under the terms of its
// separate zlib license, embedded below).

#pragma once

// Provides portable (VC++2010+, Intel ICC 13, GCC 4.7+, and anything C++11 compliant) implementation
// of low-level memory barriers, plus a few semi-portable utility macros (for inlining and alignment).
// Also has a basic atomic type (limited to hardware-supported atomics with no memory ordering guarantees).
// Uses the AE_* prefix for macros (historical reasons), and the "moodycamel" namespace for symbols.

#include <cassert>
#include <cerrno>
#include <cstdint>
#include <ctime>
#include <type_traits>

// Platform detection
#if defined(__INTEL_COMPILER)
#define AE_ICC
#elif defined(_MSC_VER)
#define AE_VCPP
#elif defined(__GNUC__)
#define AE_GCC
#endif

#if defined(_M_IA64) || defined(__ia64__)
#define AE_ARCH_IA64
#elif defined(_WIN64) || defined(__amd64__) || defined(_M_X64) || defined(__x86_64__)
#define AE_ARCH_X64
#elif defined(_M_IX86) || defined(__i386__)
#define AE_ARCH_X86
#elif defined(_M_PPC) || defined(__powerpc__)
#define AE_ARCH_PPC
#else
#define AE_ARCH_UNKNOWN
#endif

// AE_UNUSED
#define AE_UNUSED(x) ((void)x)

// AE_NO_TSAN/AE_TSAN_ANNOTATE_*
// For GCC
#if defined(__SANITIZE_THREAD__)
#define AE_TSAN_IS_ENABLED
#endif
// For clang
#if defined(__has_feature)
#if __has_feature(thread_sanitizer) && !defined(AE_TSAN_IS_ENABLED)
#define AE_TSAN_IS_ENABLED
#endif
#endif

#ifdef AE_TSAN_IS_ENABLED
#if __cplusplus >= 201703L  // inline variables require C++17
namespace moodycamel {
inline int ae_tsan_global;
}
#define AE_TSAN_ANNOTATE_RELEASE() AnnotateHappensBefore(__FILE__, __LINE__, (void*)(&::moodycamel::ae_tsan_global))
#define AE_TSAN_ANNOTATE_ACQUIRE() AnnotateHappensAfter(__FILE__, __LINE__, (void*)(&::moodycamel::ae_tsan_global))
extern "C" void AnnotateHappensBefore(const char*, int, void*);
extern "C" void AnnotateHappensAfter(const char*, int, void*);
#else  // when we can't work with tsan, attempt to disable its warnings
#define AE_NO_TSAN __attribute__((no_sanitize("thread")))
#endif
#endif

#ifndef AE_NO_TSAN
#define AE_NO_TSAN
#endif

#ifndef AE_TSAN_ANNOTATE_RELEASE
#define AE_TSAN_ANNOTATE_RELEASE()
#define AE_TSAN_ANNOTATE_ACQUIRE()
#endif

// AE_FORCEINLINE
#if defined(AE_VCPP) || defined(AE_ICC)
#define AE_FORCEINLINE __forceinline
#elif defined(AE_GCC)
// #define AE_FORCEINLINE __attribute__((always_inline))
#define AE_FORCEINLINE inline
#else
#define AE_FORCEINLINE inline
#endif

// AE_ALIGN
#if defined(AE_VCPP) || defined(AE_ICC)
#define AE_ALIGN(x) __declspec(align(x))
#elif defined(AE_GCC)
#define AE_ALIGN(x) __attribute__((aligned(x)))
#else
// Assume GCC compliant syntax...
#define AE_ALIGN(x) __attribute__((aligned(x)))
#endif

// Portable atomic fences implemented below:

namespace moodycamel {

enum memory_order {
  memory_order_relaxed,
  memory_order_acquire,
  memory_order_release,
  memory_order_acq_rel,
  memory_order_seq_cst,

  // memory_order_sync: Forces a full sync:
  // #LoadLoad, #LoadStore, #StoreStore, and most significantly, #StoreLoad
  memory_order_sync = memory_order_seq_cst
};

}  // end namespace moodycamel

// Use standard library of atomics
#include <atomic>

namespace moodycamel {

AE_FORCEINLINE void compiler_fence(memory_order order) AE_NO_TSAN {
  switch (order) {
    case memory_order_relaxed:
      break;
    case memory_order_acquire:
      std::atomic_signal_fence(std::memory_order_acquire);
      break;
    case memory_order_release:
      std::atomic_signal_fence(std::memory_order_release);
      break;
    case memory_order_acq_rel:
      std::atomic_signal_fence(std::memory_order_acq_rel);
      break;
    case memory_order_seq_cst:
      std::atomic_signal_fence(std::memory_order_seq_cst);
      break;
    default:
      assert(false);
  }
}

AE_FORCEINLINE void fence(memory_order order) AE_NO_TSAN {
  switch (order) {
    case memory_order_relaxed:
      break;
    case memory_order_acquire:
      AE_TSAN_ANNOTATE_ACQUIRE();
      std::atomic_thread_fence(std::memory_order_acquire);
      break;
    case memory_order_release:
      AE_TSAN_ANNOTATE_RELEASE();
      std::atomic_thread_fence(std::memory_order_release);
      break;
    case memory_order_acq_rel:
      AE_TSAN_ANNOTATE_ACQUIRE();
      AE_TSAN_ANNOTATE_RELEASE();
      std::atomic_thread_fence(std::memory_order_acq_rel);
      break;
    case memory_order_seq_cst:
      AE_TSAN_ANNOTATE_ACQUIRE();
      AE_TSAN_ANNOTATE_RELEASE();
      std::atomic_thread_fence(std::memory_order_seq_cst);
      break;
    default:
      assert(false);
  }
}

}  // end namespace moodycamel

#if !defined(AE_VCPP) || (_MSC_VER >= 1700 && !defined(__cplusplus_cli))
#define AE_USE_STD_ATOMIC_FOR_WEAK_ATOMIC
#endif

#include <atomic>
#include <utility>

// WARNING: *NOT* A REPLACEMENT FOR std::atomic. READ CAREFULLY:
// Provides basic support for atomic variables -- no memory ordering guarantees are provided.
// The guarantee of atomicity is only made for types that already have atomic load and store guarantees
// at the hardware level -- on most platforms this generally means aligned pointers and integers (only).
namespace moodycamel {
template <typename T>
class weak_atomic {
 public:
  AE_NO_TSAN weak_atomic() : value_() {}

  template <typename U>
  AE_NO_TSAN weak_atomic(U&& x) : value_(std::forward<U>(x)) {}

  AE_NO_TSAN weak_atomic(weak_atomic const& other) : value_(other.load()) {}

  AE_NO_TSAN weak_atomic(weak_atomic&& other) noexcept : value_(std::move(other.load())) {}

  AE_FORCEINLINE explicit operator T() const AE_NO_TSAN { return load(); }

  template <typename U>
  AE_FORCEINLINE auto operator=(U&& x) -> weak_atomic& AE_NO_TSAN {
    value_.store(std::forward<U>(x), std::memory_order_relaxed);
    return *this;
  }

  AE_FORCEINLINE auto operator=(weak_atomic const& other) -> weak_atomic& AE_NO_TSAN {
    value_.store(other.value_.load(std::memory_order_relaxed), std::memory_order_relaxed);
    return *this;
  }

  [[nodiscard]] AE_FORCEINLINE auto load() const -> T AE_NO_TSAN { return value_.load(std::memory_order_relaxed); }

  AE_FORCEINLINE auto fetch_add_acquire(T increment) -> T AE_NO_TSAN {
    return value_.fetch_add(increment, std::memory_order_acquire);
  }

  AE_FORCEINLINE auto fetch_add_release(T increment) -> T AE_NO_TSAN {
    return value_.fetch_add(increment, std::memory_order_release);
  }

 private:
#ifndef AE_USE_STD_ATOMIC_FOR_WEAK_ATOMIC
  // No std::atomic support, but still need to circumvent compiler optimizations.
  // `volatile` will make memory access slow, but is guaranteed to be reliable.
  volatile T value;
#else
  std::atomic<T> value_;
#endif
};

}  // end namespace moodycamel

// Portable single-producer, single-consumer semaphore below:

#include <semaphore.h>

// Code in the spsc_sema namespace below is an adaptation of Jeff Preshing's
// portable + lightweight semaphore implementations, originally from
// https://github.com/preshing/cpp11-on-multicore/blob/master/common/sema.h
// LICENSE:
// Copyright (c) 2015 Jeff Preshing
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgement in the product documentation would be
//    appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.
namespace moodycamel::spsc_sema {
//---------------------------------------------------------
// Semaphore (POSIX, Linux)
//---------------------------------------------------------
class Semaphore {
 private:
  sem_t m_sema_{};

 public:
  Semaphore(const Semaphore& other) = delete;
  auto operator=(const Semaphore& other) -> Semaphore& = delete;

  AE_NO_TSAN explicit Semaphore(int initial_count = 0) {
    assert(initial_count >= 0);
    int rc = sem_init(&m_sema_, 0, static_cast<unsigned int>(initial_count));
    assert(rc == 0);
    AE_UNUSED(rc);
  }

  AE_NO_TSAN ~Semaphore() { sem_destroy(&m_sema_); }

  auto wait() -> bool AE_NO_TSAN {
    // http://stackoverflow.com/questions/2013181/gdb-causes-sem-wait-to-fail-with-eintr-error
    int rc;
    do {
      rc = sem_wait(&m_sema_);
    } while (rc == -1 && errno == EINTR);
    return rc == 0;
  }

  auto try_wait() -> bool AE_NO_TSAN {
    int rc;
    do {
      rc = sem_trywait(&m_sema_);
    } while (rc == -1 && errno == EINTR);
    return rc == 0;
  }

  auto timed_wait(std::uint64_t usecs) -> bool AE_NO_TSAN {
    struct timespec ts;
    constexpr std::uint64_t usecs_in_1_sec = 1000000;
    constexpr int nsecs_in_1_sec = 1000000000;
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec += static_cast<time_t>(usecs / usecs_in_1_sec);
    ts.tv_nsec += static_cast<long>(usecs % usecs_in_1_sec) * 1000;
    // sem_timedwait bombs if you have more than 1e9 in tv_nsec
    // so we have to clean things up before passing it in
    if (ts.tv_nsec >= nsecs_in_1_sec) {
      ts.tv_nsec -= nsecs_in_1_sec;
      ++ts.tv_sec;
    }

    int rc;
    do {
      rc = sem_timedwait(&m_sema_, &ts);
    } while (rc == -1 && errno == EINTR);
    return rc == 0;
  }

  void signal() AE_NO_TSAN {
    while (sem_post(&m_sema_) == -1) {
    }
  }

  void signal(int count) AE_NO_TSAN {
    while (count-- > 0) {
      while (sem_post(&m_sema_) == -1);
    }
  }
};

//---------------------------------------------------------
// LightweightSemaphore
//---------------------------------------------------------
class LightweightSemaphore {
 public:
  using ssize_t = std::make_signed_t<std::size_t>;

 private:
  std::atomic<ssize_t> m_count_;
  Semaphore m_sema_;

  auto wait_with_partial_spinning(std::int64_t timeout_usecs = -1) -> bool AE_NO_TSAN {
    ssize_t old_count;
    // Is there a better way to set the initial spin count?
    // If we lower it to 1000, testBenaphore becomes 15x slower on my Core i7-5930K Windows PC,
    // as threads start hitting the kernel semaphore.
    int spin = 1024;
    while (--spin >= 0) {
      if (m_count_.load(std::memory_order::relaxed) > 0) {
        m_count_.fetch_add(-1, std::memory_order::acquire);
        return true;
      }
      compiler_fence(memory_order_acquire);  // Prevent the compiler from collapsing the loop.
    }
    old_count = m_count_.fetch_add(-1, std::memory_order::acquire);
    if (old_count > 0) return true;
    if (timeout_usecs < 0) {
      if (m_sema_.wait()) return true;
    }
    if (timeout_usecs > 0 && m_sema_.timed_wait(static_cast<uint64_t>(timeout_usecs))) return true;
    // At this point, we've timed out waiting for the semaphore, but the
    // count is still decremented indicating we may still be waiting on
    // it. So we have to re-adjust the count, but only if the semaphore
    // wasn't signaled enough times for us too since then. If it was, we
    // need to release the semaphore too.
    while (true) {
      old_count = m_count_.fetch_add(1, std::memory_order::release);
      if (old_count < 0) return false;  // successfully restored things to the way they were
      // Oh, the producer thread just signaled the semaphore after all. Try again:
      old_count = m_count_.fetch_add(-1, std::memory_order::acquire);
      if (old_count > 0 && m_sema_.try_wait()) return true;
    }
  }

 public:
  AE_NO_TSAN explicit LightweightSemaphore(ssize_t initial_count = 0) noexcept : m_count_{initial_count} {
    assert(initial_count >= 0);
  }

  auto try_wait() -> bool AE_NO_TSAN {
    if (m_count_.load(std::memory_order::relaxed) > 0) {
      m_count_.fetch_add(-1, std::memory_order::acquire);
      return true;
    }
    return false;
  }

  auto wait() -> bool AE_NO_TSAN { return try_wait() || wait_with_partial_spinning(); }

  auto wait(std::int64_t timeout_usecs) -> bool AE_NO_TSAN {
    return try_wait() || wait_with_partial_spinning(timeout_usecs);
  }

  void signal(ssize_t count = 1) AE_NO_TSAN {
    assert(count >= 0);
    ssize_t old_count = m_count_.fetch_add(count, std::memory_order::release);
    assert(old_count >= -1);
    if (old_count < 0) {
      m_sema_.signal(1);
    }
  }

  [[nodiscard]] auto available_approx() const -> std::size_t AE_NO_TSAN {
    ssize_t count = m_count_.load(std::memory_order::relaxed);
    return count > 0 ? static_cast<std::size_t>(count) : 0;
  }
};

}  // namespace moodycamel::spsc_sema
