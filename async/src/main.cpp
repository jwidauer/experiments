#include <cstddef>
#include <thread>

namespace {

namespace async {

struct Executor {};

template <typename T>
struct Pipe {};

template <typename T>
struct Writer {
  Writer(Executor& executor, Pipe<T>& pipe) : executor_{executor}, pipe_{pipe} {}

 private:
  Executor& executor_;
  Pipe<T>& pipe_;
};

template <typename T>
struct Reader {
  Reader(Executor& executor, Pipe<T>& pipe) : executor_{executor}, pipe_{pipe} {}

 private:
  Executor& executor_;
  Pipe<T>& pipe_;
};

}  // namespace async

constexpr std::size_t max_count = 10;

void produce(async::Pipe<int>& pipe) {
  async::Executor executor;

  async::Writer writer(executor, pipe);

  std::size_t count = 0;

  auto post_write = [&](this const auto& self) -> auto {
    if (!(count < max_count)) return;
    writer.write(count++, self);
  };

  writer.write(count++, post_write);

  executor.run();
}

void consume(async::Pipe<int>& pipe) {
  async::Executor executor;

  async::Reader reader(executor, pipe);

  auto post_read = [&](this const auto& self, auto i) -> auto {
    if (i == max_count) return;
    reader.read(self);
  };

  reader.read(post_read);

  executor.run();
}

}  // namespace

auto main(int /*argc*/, char** /*argv*/) -> int {
  async::Pipe pipe;

  auto t1 = std::jthread(produce, std::ref(pipe));
  auto t2 = std::jthread(consume, std::ref(pipe));

  return 0;
}
