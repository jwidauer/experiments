project('minilog', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3',
                     'cpp_std=c++17'])

deps = [
  dependency('spdlog'),
  dependency('nlohmann_json'),
]

exe = executable('minilog', 'minilog.cpp',
  dependencies : deps,
  install : true)

test('basic', exe)
