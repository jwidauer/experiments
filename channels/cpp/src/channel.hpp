#ifndef INCLUDE_CHANNEL_HPP
#define INCLUDE_CHANNEL_HPP

#include "spsc.hpp"

namespace channel {
namespace detail {

template <typename T, std::size_t N>
struct Channel {
  moodycamel::SPSC<T, N> queue1;
  moodycamel::SPSC<T, N> queue2;
};

template <typename T, std::size_t N>
struct Sender {
  explicit Sender(std::shared_ptr<moodycamel::SPSC<T, N>> q) : queue_{std::move(q)} {}

  auto send(const T& item) -> bool { return queue_->try_enqueue(item); }

  auto send(T&& item) -> bool { return queue_->try_enqueue(std::move(item)); }

 private:
  std::shared_ptr<moodycamel::SPSC<T, N>> queue_;
};

template <typename T, std::size_t N>
struct Receiver {
  explicit Receiver(std::shared_ptr<moodycamel::SPSC<T, N>> q) : queue_{std::move(q)} {}

  auto recv(T& item) -> bool { return queue_->try_dequeue(item); }

  auto recv() -> std::optional<T> {
    T item;
    if (!queue_->try_dequeue(item)) {
      return std::nullopt;
    }
    return std::optional<T>{std::in_place, std::move(item)};
  }

 private:
  std::shared_ptr<moodycamel::SPSC<T, N>> queue_;
};

template <typename T, std::size_t N>
struct ChannelSide {
  ChannelSide(Receiver<T, N>&& rx, Sender<T, N>&& tx) : receiver_{std::move(rx)}, sender_{std::move(tx)} {}

  auto send(const T& item) -> bool { return sender_.send(item); }
  auto send(T&& item) -> bool { return sender_.send(std::move(item)); }

  auto recv(T& item) -> bool { return receiver_.recv(item); }
  auto recv() -> std::optional<T> { return receiver_.recv(); }

 private:
  Receiver<T, N> receiver_;
  Sender<T, N> sender_;
};

}  // namespace detail

template <typename T, std::size_t N>
auto create() -> std::pair<detail::ChannelSide<T, N>, detail::ChannelSide<T, N>> {
  auto channel = std::make_shared<detail::Channel<T, N>>();

  std::shared_ptr<moodycamel::SPSC<T, N>> q1{channel, &channel->queue1};
  std::shared_ptr<moodycamel::SPSC<T, N>> q2{channel, &channel->queue2};

  detail::Sender<T, N> sender1(q1);
  detail::Receiver<T, N> receiver1(q2);

  detail::Sender<T, N> sender2(std::move(q2));
  detail::Receiver<T, N> receiver2(std::move(q1));

  return {detail::ChannelSide<T, N>(std::move(receiver1), std::move(sender1)),
          detail::ChannelSide<T, N>(std::move(receiver2), std::move(sender2))};
}

}  // namespace channel

#endif  // INCLUDE_CHANNEL_HPP
