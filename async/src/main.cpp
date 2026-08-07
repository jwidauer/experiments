#include <condition_variable>
#include <deque>
#include <functional>
#include <mutex>
#include <print>
#include <thread>
#include <utility>

namespace {

namespace async {

class Executor {
 public:
  void post(std::function<void()> task) {
    {
      std::lock_guard lock{mutex_};
      tasks_.push_back(std::move(task));
    }
    ready_.notify_one();
  }

  void run() {
    std::unique_lock lock(mutex_);

    auto has_work = [&] -> bool { return !tasks_.empty(); };

    for (;;) {
      ready_.wait(lock, has_work);

      if (!has_work()) return;

      auto task = std::move(tasks_.front());
      tasks_.pop_front();

      lock.unlock();
      task();
      lock.lock();

      if (tasks_.empty()) {
        ready_.notify_all();
      }
    }
  }

 private:
  std::mutex mutex_;
  std::condition_variable ready_;
  std::deque<std::function<void()>> tasks_;
};

template <typename T>
class Pipe {
 private:
  struct PendingWrite {
    T value;
    Executor* executor;
    std::function<void()> continuation;
  };

  struct PendingRead {
    Executor* executor;
    std::function<void(T)> continuation;
  };

 public:
  template <typename Fn>
  void write(Executor& executor, T value, Fn&& continuation) {
    auto on_write = std::function<void()>(std::forward<Fn>(continuation));

    std::unique_lock lock{mutex_};
    if (pending_reads_.empty()) {
      pending_writes_.emplace_back(std::move(value), &executor, std::move(on_write));
      return;
    }

    auto read = std::move(pending_reads_.front());
    pending_reads_.pop_front();
    lock.unlock();

    read.executor->post([continuation = std::move(read.continuation), value = std::move(value)]() mutable -> auto {
      continuation(std::move(value));
    });
    executor.post([continuation = std::move(on_write)]() mutable -> auto { continuation(); });
  }

  template <typename Fn>
  void read(Executor& executor, Fn&& continuation) {
    auto on_read = std::function<void(T)>(std::forward<Fn>(continuation));

    std::unique_lock lock{mutex_};
    if (pending_writes_.empty()) {
      pending_reads_.emplace_back(&executor, std::move(on_read));
      return;
    }

    auto write = std::move(pending_writes_.front());
    pending_writes_.pop_front();
    lock.unlock();

    executor.post([continuation = std::move(on_read), value = std::move(write.value)]() mutable -> auto {
      continuation(std::move(value));
    });
    write.executor->post([continuation = std::move(write.continuation)]() mutable -> auto { continuation(); });
  }

 private:
  std::mutex mutex_;
  std::deque<PendingWrite> pending_writes_;
  std::deque<PendingRead> pending_reads_;
};

template <typename T>
class Writer {
 public:
  Writer(Executor& executor, Pipe<T>& pipe) : executor_{executor}, pipe_{pipe} {}

  template <typename Fn>
  void write(T value, Fn&& continuation) {
    pipe_.write(executor_, std::move(value), std::forward<Fn>(continuation));
  }

 private:
  Executor& executor_;
  Pipe<T>& pipe_;
};

template <typename T>
class Reader {
 public:
  Reader(Executor& executor, Pipe<T>& pipe) : executor_{executor}, pipe_{pipe} {}

  template <typename Fn>
  void read(Fn&& continuation) {
    pipe_.read(executor_, std::forward<Fn>(continuation));
  }

 private:
  Executor& executor_;
  Pipe<T>& pipe_;
};

}  // namespace async

constexpr int max_count = 10;

void produce(async::Pipe<int>& pipe) {
  async::Executor executor;
  async::Writer writer(executor, pipe);

  int count = 0;
  auto post_write = [&](this const auto& self) -> void {
    if (count == max_count) return;

    writer.write(count++, self);
  };

  writer.write(count++, post_write);
  executor.run();
}

void consume(async::Pipe<int>& pipe) {
  async::Executor executor;
  async::Reader reader(executor, pipe);

  auto post_read = [&](this const auto& self, int value) -> void {
    std::println("{}", value);

    if (value == (max_count - 1)) return;

    reader.read(self);
  };

  reader.read(post_read);
  executor.run();
}

}  // namespace

auto main(int /*argc*/, char** /*argv*/) -> int {
  async::Pipe<int> pipe;

  auto producer = std::jthread(produce, std::ref(pipe));
  auto consumer = std::jthread(consume, std::ref(pipe));

  return 0;
}
