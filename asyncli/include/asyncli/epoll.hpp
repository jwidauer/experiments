#pragma once

#include <sys/epoll.h>

#include <chrono>
#include <cstddef>
#include <span>

#include "events.hpp"
#include "file_descriptor.hpp"
#include "functional.hpp"
#include "result.hpp"
#include "syscall.hpp"

namespace asyncli {

struct Epoll {
  [[nodiscard]] static auto create() -> Result<Epoll> {
    return syscall<epoll_create1>(0).transform(construct<Epoll>());
  }

  [[nodiscard]] auto add(const FileDescriptor& fd, Events events) -> Result<void> {
    epoll_event event{};
    event.events = events.to_int();
    event.data.fd = fd.val();
    return syscall<epoll_ctl>(fd_.val(), EPOLL_CTL_ADD, fd.val(), &event).transform(Ignore{});
  }

  [[nodiscard]] auto modify(const FileDescriptor& fd, Events events) -> Result<void> {
    epoll_event event{};
    event.events = events.to_int();
    event.data.fd = fd.val();
    return syscall<epoll_ctl>(fd_.val(), EPOLL_CTL_MOD, fd.val(), &event).transform(Ignore{});
  }

  [[nodiscard]] auto remove(const FileDescriptor& fd) -> Result<void> {
    return syscall<epoll_ctl>(fd_.val(), EPOLL_CTL_DEL, fd.val(), nullptr).transform(Ignore{});
  }

  [[nodiscard]] auto wait(std::span<epoll_event> events, std::chrono::milliseconds timeout) -> Result<std::size_t> {
    auto as_size = [](int n) -> std::size_t { return static_cast<std::size_t>(n); };
    return syscall<epoll_wait>(fd_.val(), events.data(), events.size(), timeout.count()).transform(as_size);
  }

 private:
  friend struct Construct<Epoll>;

  constexpr explicit Epoll(int fd) : fd_{fd} {}

  FileDescriptor fd_;
};

}  // namespace asyncli
