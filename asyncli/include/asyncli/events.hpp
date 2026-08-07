#pragma once

#include <sys/epoll.h>

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <magic_enum/magic_enum_containers.hpp>
#include <magic_enum/magic_enum_flags.hpp>
#include <optional>
#include <span>
#include <type_traits>
#include <utility>

#include "magic_enum/magic_enum.hpp"

namespace asyncli {

enum struct Event : uint32_t {  // NOLINT(performance-enum-size) needs to be uint32_t to match EPOLL_EVENTS
  Readable = EPOLLIN,
  Exceptional = EPOLLPRI,  // There is urgent data to read (e.g., out-of-band data on TCP socket; see recv(2)).
  Writable = EPOLLOUT,
  Error = EPOLLERR,
  HangUp = EPOLLHUP,
  ReadHangUp = EPOLLRDHUP,
};

}

template <>
struct std::hash<asyncli::Event> {
  constexpr auto operator()(const asyncli::Event& event) const -> std::size_t {
    using Type = std::underlying_type_t<asyncli::Event>;
    return std::hash<Type>{}(std::to_underlying(event));
  }
};

namespace asyncli {

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

  // An iterator over the set events in this Events object. Iteration order is the same as the order of the Event enum
  // values.
  struct ValueIterator {
    using iterator_category = std::input_iterator_tag;
    using value_type = Event;
    using difference_type = std::ptrdiff_t;
    using pointer = const Event*;
    using reference = const Event&;

    ValueIterator() = default;
    explicit ValueIterator(const Events* events) : events_{events} {}

    [[nodiscard]] constexpr auto operator*() const -> value_type { return current_; }

    constexpr auto operator++() -> ValueIterator& {
      advance_to_next_set_event();
      return *this;
    }

    [[nodiscard]] constexpr auto operator++(int) -> ValueIterator {
      ValueIterator cp = *this;
      ++*this;
      return cp;
    }

    [[nodiscard]] friend auto operator==(const ValueIterator& lhs, const ValueIterator& rhs) -> bool {
      return lhs.events_ == rhs.events_ && lhs.current_ == rhs.current_;
    }
    [[nodiscard]] friend auto operator!=(const ValueIterator& lhs, const ValueIterator& rhs) -> bool {
      return !(lhs == rhs);
    }

   private:
    static constexpr auto values = magic_enum::enum_values<Event>();

    constexpr void advance_to_next_set_event() {
      const auto current_idx = magic_enum::enum_index(current_).value();
      const auto next_set_event = first_set_event(current_idx + 1);
      if (next_set_event) {
        current_ = *next_set_event;
        return;
      }

      // No more set events, move to the end.
      events_ = nullptr;
    }

    [[nodiscard]] constexpr auto first_set_event(std::size_t offset = 0) const -> std::optional<Event> {
      if (events_ == nullptr || offset >= values.size()) return std::nullopt;

      const auto events_to_check = std::span{values}.subspan(offset);
      const auto iter =
          std::ranges::find_if(events_to_check, [this](auto event) -> bool { return events_->test(event); });
      return iter != events_to_check.end() ? std::optional{*iter} : std::nullopt;
    }

    const Events* events_ = nullptr;
    Event current_ = first_set_event().value_or(values.front());
  };

  [[nodiscard]] constexpr auto begin() const -> ValueIterator { return ValueIterator{this}; }
  [[nodiscard]] constexpr auto end() const -> ValueIterator { return ValueIterator{}; }

 private:
  using Bitset = magic_enum::containers::bitset<Event>;
  explicit Events(Bitset events) : events_{events} {}

  Bitset events_;
};

}  // namespace asyncli
