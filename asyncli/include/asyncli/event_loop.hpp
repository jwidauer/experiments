#pragma once

#include <sys/epoll.h>

#include <algorithm>
#include <array>
#include <chrono>
#include <functional>
#include <magic_enum/magic_enum_containers.hpp>
#include <span>
#include <unordered_map>
#include <utility>

#include "epoll.hpp"
#include "events.hpp"
#include "file_descriptor.hpp"
#include "functional.hpp"
#include "key.hpp"
#include "macros.hpp"
#include "magic_enum/magic_enum.hpp"
#include "result.hpp"

namespace asyncli {

struct EventLoop {
  constexpr static auto max_events = 64;
  constexpr static auto default_timeout = std::chrono::milliseconds{10};

  using Handler = std::function<void(FileDescriptor, Event)>;

  [[nodiscard]] static auto create() -> Result<EventLoop> { return Epoll::create().transform(construct<EventLoop>()); }

  [[nodiscard]] auto add_handler(const FileDescriptor& fd, Event event, Handler handler) -> Result<void> {
    auto& handlers = handlers_[fd];

    if (std::ranges::none_of(handlers, &Handler::operator bool)) {
      // No existing handlers for this file descriptor, so we need to add it to epoll.
      ASYNCLI_TRY(epoll_.add(fd, Events{event}));
    } else {
      // There are existing handlers, so we need to modify the epoll events to include the new event.
      auto or_if_set = [&](Events acc, Event event) -> Events {
        return acc | (handlers[event] ? Events{event} : Events{});
      };
      auto existing_events = std::ranges::fold_left(magic_enum::enum_values<Event>(), Events{}, or_if_set);
      ASYNCLI_TRY(epoll_.modify(fd, existing_events | Events{event}));
    }

    handlers[event] = std::move(handler);
    return {};
  }

  [[nodiscard]] auto run_once() -> Result<void> {
    std::array<epoll_event, max_events> events;
    const auto num_events = ASYNCLI_TRY(epoll_.wait(events, default_timeout));

    const auto event_span = std::span{events}.first(num_events);

    for (const auto& event : event_span) {
      const auto events = Events::from_int(event.events);
      if (!events) continue;  // No valid events, skip it.

      const auto fd = FileDescriptor{event.data.fd};
      const auto iter = handlers_.find(fd);
      if (iter == handlers_.end()) continue;  // No handlers for this file descriptor, skip it.

      const auto& handlers = iter->second;

      for (const auto event : events.value()) {
        const auto& handler = handlers[event];
        if (handler) handler(fd, event);
      }
    }

    return {};
  }

 private:
  friend struct Construct<EventLoop>;

  explicit EventLoop(Epoll&& epoll) : epoll_{std::move(epoll)} {}

  using Handlers = magic_enum::containers::array<Event, Handler>;

  std::unordered_map<Key, Handlers> handlers_;
  Epoll epoll_;
};

}  // namespace asyncli
