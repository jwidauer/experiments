#pragma once

#include <algorithm>
#include <functional>
#include <magic_enum/magic_enum_containers.hpp>
#include <unordered_map>
#include <utility>

#include "epoll.hpp"
#include "events.hpp"
#include "file_descriptor.hpp"
#include "functional.hpp"
#include "macros.hpp"
#include "magic_enum/magic_enum.hpp"
#include "result.hpp"

namespace asyncli {

struct EventLoop {
  using Handler = std::function<void(FileDescriptor, Event)>;

  [[nodiscard]] static auto create() -> Result<EventLoop> { return Epoll::create().transform(construct<EventLoop>()); }

  [[nodiscard]] auto add_handler(FileDescriptor fd, Event event, Handler handler) -> Result<void> {
    auto& handlers = handlers_[fd];
    if (std::ranges::none_of(handlers, &Handler::operator bool)) {
      ASYNCLI_RETURN_IF_ERROR(epoll_.add(std::move(fd), Events{event}));
    } else {
      auto or_if_set = [&](Events acc, Event event) -> Events {
        return acc | (handlers[event] ? Events{event} : Events{});
      };
      auto existing_events = std::ranges::fold_left(magic_enum::enum_values<Event>(), Events{}, or_if_set);
      ASYNCLI_RETURN_IF_ERROR(epoll_.modify(std::move(fd), existing_events | Events{event}));
    }
    handlers[event] = std::move(handler);
    return {};
  }

  [[nodiscard]] auto run() -> Result<void>;

 private:
  friend struct Construct<EventLoop>;

  explicit EventLoop(Epoll&& epoll) : epoll_{std::move(epoll)} {}

  using Handlers = magic_enum::containers::array<Event, Handler>;

  std::unordered_map<FileDescriptor, Handlers> handlers_;
  Epoll epoll_;
};

}  // namespace asyncli
