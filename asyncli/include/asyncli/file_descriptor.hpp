#pragma once

#include <unistd.h>

#include <cassert>
#include <cstddef>
#include <functional>
#include <utility>

namespace asyncli {

struct FileDescriptor {
  constexpr explicit FileDescriptor(int fd) : fd_(fd) { assert(fd != -1); }

  FileDescriptor(const FileDescriptor&) = default;
  auto operator=(const FileDescriptor&) -> FileDescriptor& = default;

  constexpr FileDescriptor(FileDescriptor&& other) noexcept : fd_{std::exchange(other.fd_, -1)} {}
  constexpr auto operator=(FileDescriptor&& other) noexcept -> FileDescriptor& {
    if (this != &other) {
      close();
      fd_ = std::exchange(other.fd_, -1);
    }
    return *this;
  }

  constexpr ~FileDescriptor() { close(); }

  [[nodiscard]] constexpr auto val() const -> int { return fd_; }

  constexpr void close() {
    if (fd_ == -1) return;
    ::close(fd_);
    fd_ = -1;
  }

  [[nodiscard]] friend constexpr auto operator==(const FileDescriptor&, const FileDescriptor&) -> bool = default;

 private:
  int fd_;
};

}  // namespace asyncli

template <>
struct std::hash<asyncli::FileDescriptor> {
  constexpr auto operator()(const asyncli::FileDescriptor& fd) const -> std::size_t {
    return std::hash<int>{}(fd.val());
  }
};
