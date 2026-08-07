#pragma once

#include <cstddef>
#include <format>
#include <functional>
#include <utility>

namespace ctr {

template <class Tag>
struct Id {
  [[nodiscard]] static constexpr auto create() -> Id<Tag> {
    static constinit auto counter = 0UZ;
    return Id<Tag>{counter++};
  }

  constexpr Id() = delete;

  constexpr Id(const Id&) = default;
  constexpr auto operator=(const Id&) -> Id& = default;

  constexpr Id(Id&&) = default;
  constexpr auto operator=(Id&&) -> Id& = default;

  friend constexpr void swap(Id& lhs, Id& rhs) noexcept { std::swap(lhs.val_, rhs.val_); }

  friend constexpr auto operator<=>(const Id<Tag>& lhs, const Id<Tag>& rhs) -> bool = default;
  friend struct std::hash<Id<Tag>>;
  friend struct std::formatter<Id<Tag>>;

 private:
  explicit constexpr Id(std::size_t val) : val_(val) {}

  std::size_t val_;
};

}  // namespace ctr

template <class Tag>
struct std::hash<ctr::Id<Tag>> {
  [[nodiscard]] constexpr auto operator()(const ctr::Id<Tag>& id) const -> std::size_t {
    return std::hash<std::size_t>{}(id.val_);
  }
};

template <class Tag>
struct std::formatter<ctr::Id<Tag>> : std::formatter<std::size_t> {
  template <class FormatContext>
  auto format(const ctr::Id<Tag>& id, FormatContext& ctx) const {
    auto out = std::format_to(ctx.out(), "Id(");
    out = std::formatter<std::size_t>::format(id.val_, ctx);
    return std::format_to(out, ")");
  }
};
