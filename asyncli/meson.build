project(
  'asyncli',
  'cpp',
  version: '0.1',
  meson_version: '>= 1.3.0',
  default_options: ['warning_level=3', 'cpp_std=c++26'],
)

subdir('include')

exe = executable(
  'asyncli',
  'src/main.cpp',
  install: true,
  dependencies: [asyncli_dep],
)

test('basic', exe)
