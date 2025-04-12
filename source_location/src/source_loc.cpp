#include <cstdint>
#include <iostream>

#define FN [[nodiscard]] constexpr auto

struct SourceLocation {
  consteval SourceLocation(const char* file_name, const char* function_name,
                           uint32_t line)
      : file_{file_name}, function_{function_name}, line_{line} {}

  FN FileName() const -> const char* { return file_; }
  FN FunctionName() const -> const char* { return function_; }
  FN Line() const -> uint32_t { return line_; }

 private:
  const char* file_;
  const char* function_;
  uint32_t line_;
};

template <class OStream>
OStream& operator<<(OStream& os, const SourceLocation& loc) {
  return os << loc.FileName() << ':' << loc.Line() << ' ' << loc.FunctionName();
}

#define SOURCE_LOCATION() \
  SourceLocation { __FILE_NAME__, __PRETTY_FUNCTION__, __LINE__ }

int main() {
  auto loc = SOURCE_LOCATION();
  auto loc2 = loc;

  std::cout << "loc:\t\t" << loc << '\n';
  std::cout << "loc2:\t\t" << loc2 << '\n';

  return 0;
}
