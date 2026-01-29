project(
  'containers',
  'cpp',
  version: '0.1',
  meson_version: '>= 1.3.0',
  default_options: ['warning_level=3', 'cpp_std=c++26'],
)

dependencies = [
  dependency('tl-optional', required: true),
]

exe = executable(
  'containers',
  'src/containers.cpp',
  install: true,
  dependencies: dependencies,
  cpp_args: ['-Wcast-align=strict'],
)

test('basic', exe)
