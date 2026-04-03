project(
  'coro',
  'cpp',
  version: '0.1',
  meson_version: '>= 1.3.0',
  default_options: ['warning_level=3', 'cpp_std=c++26'],
)

dependencies = []

exe = executable(
  'main',
  'src/main.cpp',
  install: true,
  dependencies: dependencies,
)

test('basic', exe)
