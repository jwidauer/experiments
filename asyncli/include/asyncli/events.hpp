#pragma once

#include <sys/epoll.h>

#include <cstdint>
#include <magic_enum/magic_enum_containers.hpp>
#include <magic_enum/magic_enum_flags.hpp>
#include <optional>

namespace asyncli {

enum struct Event : uint32_t {  // NOLINT(performance-enum-size) needs to be uint32_t to match EPOLL_EVENTS
  Readable = EPOLLIN,
  Exceptional = EPOLLPRI,  // There is urgent data to read (e.g., out-of-band data on TCP socket; see recv(2)).
  Writable = EPOLLOUT,
  Error = EPOLLERR,
  HangUp = EPOLLHUP,
  ReadHangUp = EPOLLRDHUP,
};

struct Events {
  Events() = default;

  explicit Events(Event event) : events_{event} {}

  Events(const Events&) = default;
  auto operator=(const Events&) -> Events& = default;
  Events(Events&&) = default;
  auto operator=(Events&&) -> Events& = default;

  [[nodiscard]] static constexpr auto from_int(uint32_t events) -> std::optional<Events> {
    return magic_enum::enum_flags_cast<Event>(events).transform([](Event e) -> auto { return Events{e}; });
  }

  [[nodiscard]] constexpr auto test(Event event) const -> bool { return events_.test(event); }
  [[nodiscard]] constexpr auto to_int() const -> uint32_t { return static_cast<uint32_t>(events_.to_ulong({})); }

  friend auto operator|(Events lhs, Events rhs) -> Events { return Events{lhs.events_ | rhs.events_}; }
  friend auto operator&(Events lhs, Events rhs) -> Events { return Events{lhs.events_ & rhs.events_}; }
  friend auto operator~(Events events) -> Events { return Events{~events.events_}; }

 private:
  using Bitset = magic_enum::containers::bitset<Event>;
  explicit Events(Bitset events) : events_{events} {}

  Bitset events_;
};

}  // namespace asyncli
