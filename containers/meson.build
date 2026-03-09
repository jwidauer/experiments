project(
  'containers',
  'cpp',
  version: '0.1',
  meson_version: '>= 1.3.0',
  default_options: ['warning_level=3', 'cpp_std=c++26'],
)

subdir('include')

exe = executable(
  'containers',
  'src/main.cpp',
  install: true,
  dependencies: [ctr_dep],
  cpp_args: ['-Wcast-align=strict'],
)

test('basic', exe)
