#pragma once

#include <coroutine>
#include <exception>
#include <utility>
#include <variant>

template <typename T>
class Task {
 public:
  // NOLINTNEXTLINE(readability-identifier-naming) - matches promise_type naming
  struct promise_type {
    // Store either the result or an exception
    std::variant<std::monostate, T, std::exception_ptr> result;
    std::coroutine_handle<> continuation;  // Who's waiting for us

    constexpr auto get_return_object() -> Task {
      return Task{std::coroutine_handle<promise_type>::from_promise(*this)};
    }

    // Suspend immediately - lazy start
    constexpr auto initial_suspend() noexcept -> std::suspend_always { return {}; }

    // Custom final_suspend for symmetric transfer
    constexpr auto final_suspend() noexcept {
      struct FinalAwaiter {
        auto await_ready() noexcept -> bool { return false; }

        auto await_suspend(std::coroutine_handle<promise_type> h) noexcept -> std::coroutine_handle<> {
          // Resume whoever is waiting for us
          if (h.promise().continuation) return h.promise().continuation;

          return std::noop_coroutine();
        }

        void await_resume() noexcept {}
      };
      return FinalAwaiter{};
    }

    // Store the return value
    template <typename U>
      requires std::convertible_to<U&&, T>
    constexpr void return_value(U&& value) {
      result.template emplace<1>(std::forward<U>(value));
    }

    constexpr void unhandled_exception() { result.template emplace<2>(std::current_exception()); }

    // Get the result, rethrowing any exception
    constexpr auto get_result() & -> T& {
      if (result.index() == 2) {
        std::rethrow_exception(std::get<2>(result));
      }
      return std::get<1>(result);
    }

    constexpr auto get_result() && -> T&& {
      if (result.index() == 2) {
        std::rethrow_exception(std::get<2>(result));
      }
      return std::move(std::get<1>(result));
    }
  };

 private:
  std::coroutine_handle<promise_type> handle_;

 public:
  constexpr Task() noexcept : handle_{nullptr} {}

  constexpr explicit Task(std::coroutine_handle<promise_type> h) noexcept : handle_{h} {}

  Task(const Task&) = delete;
  auto operator=(const Task&) -> Task& = delete;

  // Move-only
  constexpr Task(Task&& other) noexcept : handle_{std::exchange(other.handle_, nullptr)} {}

  constexpr auto operator=(Task&& other) noexcept -> Task& {
    if (this != &other) {
      if (handle_) handle_.destroy();
      handle_ = std::exchange(other.handle_, nullptr);
    }
    return *this;
  }

  constexpr ~Task() {
    if (handle_) handle_.destroy();
  }

  // Check if valid
  constexpr explicit operator bool() const noexcept { return handle_ != nullptr; }

  [[nodiscard]] constexpr auto done() const noexcept -> bool { return !handle_ || handle_.done(); }
  constexpr void resume() {
    if (handle_) handle_.resume();
  }

  // Awaiter returned when you co_await a task
  struct Awaiter {
    std::coroutine_handle<promise_type> handle;

    constexpr auto await_ready() noexcept -> bool {
      return false;  // Always suspend to start the task
    }

    // Symmetric transfer: resume the task, store continuation
    constexpr auto await_suspend(std::coroutine_handle<> continuation) noexcept -> std::coroutine_handle<> {
      handle.promise().continuation = continuation;
      return handle;  // Start/resume the task
    }

    // Get the result when we resume
    constexpr auto await_resume() -> T { return std::move(handle.promise()).get_result(); }
  };

  // Support co_await on task
  constexpr auto operator co_await() && noexcept -> Awaiter { return Awaiter{handle_}; }

  // For non-coroutine contexts: block until complete
  constexpr auto sync_wait() -> T {
    // Simple spin wait - in production, use proper synchronization
    // Resume until done
    while (!done()) resume();
    return std::move(handle_.promise()).get_result();
  }
};

// Specialization for task<void>
template <>
class Task<void> {
 public:
  // NOLINTNEXTLINE(readability-identifier-naming) - matches promise_type naming
  struct promise_type {
    std::exception_ptr exception;
    std::coroutine_handle<> continuation;

    constexpr auto get_return_object() -> Task {
      return Task{std::coroutine_handle<promise_type>::from_promise(*this)};
    }

    constexpr auto initial_suspend() noexcept -> std::suspend_always { return {}; }

    constexpr auto final_suspend() noexcept {
      struct FinalAwaiter {
        constexpr auto await_ready() noexcept -> bool { return false; }
        constexpr auto await_suspend(std::coroutine_handle<promise_type> h) noexcept -> std::coroutine_handle<> {
          if (h.promise().continuation) return h.promise().continuation;
          return std::noop_coroutine();
        }
        constexpr void await_resume() noexcept {}
      };
      return FinalAwaiter{};
    }

    constexpr void return_void() noexcept {}

    constexpr void unhandled_exception() { exception = std::current_exception(); }

    constexpr void get_result() const {
      if (exception) std::rethrow_exception(exception);
    }
  };

 private:
  std::coroutine_handle<promise_type> handle_;

 public:
  constexpr Task() noexcept : handle_{nullptr} {}

  constexpr explicit Task(std::coroutine_handle<promise_type> h) noexcept : handle_{h} {}

  Task(const Task&) = delete;
  auto operator=(const Task&) -> Task& = delete;

  // Move-only
  constexpr Task(Task&& other) noexcept : handle_{std::exchange(other.handle_, nullptr)} {}

  constexpr auto operator=(Task&& other) noexcept -> Task& {
    if (this != &other) {
      if (handle_) handle_.destroy();
      handle_ = std::exchange(other.handle_, nullptr);
    }
    return *this;
  }

  constexpr ~Task() {
    if (handle_) handle_.destroy();
  }

  // Check if valid
  constexpr explicit operator bool() const noexcept { return handle_ != nullptr; }

  [[nodiscard]] constexpr auto done() const noexcept -> bool { return !handle_ || handle_.done(); }
  constexpr void resume() {
    if (handle_) handle_.resume();
  }

  // Awaiter returned when you co_await a task
  struct Awaiter {
    std::coroutine_handle<promise_type> handle;

    constexpr auto await_ready() noexcept -> bool {
      return false;  // Always suspend to start the task
    }

    // Symmetric transfer: resume the task, store continuation
    [[nodiscard]] constexpr auto await_suspend(std::coroutine_handle<> continuation) const noexcept
        -> std::coroutine_handle<> {
      handle.promise().continuation = continuation;
      return handle;  // Start/resume the task
    }

    // Get the result when we resume
    constexpr void await_resume() const { handle.promise().get_result(); }
  };

  // Support co_await on task
  constexpr auto operator co_await() && noexcept -> Awaiter { return Awaiter{handle_}; }

  // For non-coroutine contexts: block until complete
  constexpr void sync_wait() {
    // Simple spin wait - in production, use proper synchronization
    // Resume until done
    while (!done()) resume();
    handle_.promise().get_result();
  }
};
