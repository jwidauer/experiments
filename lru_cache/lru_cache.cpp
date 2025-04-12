#include <concepts>
#include <cstddef>
#include <iostream>
#include <list>
#include <optional>
#include <unordered_map>

template <typename T>
concept Hashable = requires(T a) {
  {
    std::hash<T>{}(a)
  } -> std::same_as<std::size_t>;
};

template <Hashable Key, typename Value>
class LruCache {
 public:
  using key_type = Key;
  using value_type = Value;

  explicit LruCache(std::size_t capacity) : capacity_{capacity} {}

  auto get(const Key& key) -> std::optional<Value>
  {
    if (auto it = cache_.find(key); it != cache_.end()) {
      lru_.splice(lru_.begin(), lru_, it->second.it);
      return it->second.value;
    }
    return std::nullopt;
  }

  void put(const Key& key, const Value& value)
  {
    if (auto it = cache_.find(key); it != cache_.end()) {
      it->second.value = value;
      lru_.splice(lru_.begin(), lru_, it->second.it);
      return;
    }
    if (cache_.size() == capacity_) {
      cache_.erase(lru_.back());
      lru_.pop_back();
    }
    lru_.emplace_front(key);
    cache_.insert({key, {value, lru_.begin()}});
  }

 private:
  using lru_list = std::list<Key>;
  struct CacheEntry {
    Value value;
    lru_list::iterator it;
  };

  std::size_t capacity_;
  std::unordered_map<Key, CacheEntry> cache_;
  lru_list lru_;
};

auto main() -> int
{
  LruCache<int, std::string> cache{2};
  cache.put(1, "one");
  cache.put(2, "two");
  std::cout << cache.get(1).value() << '\n';

  cache.put(3, "three");
  std::cout << cache.get(2).value_or("not found") << '\n';

  cache.put(4, "four");
  std::cout << cache.get(1).value_or("not found") << '\n';

  return 0;
}
