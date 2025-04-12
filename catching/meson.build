project('catching', 'cpp',
  version : '0.1',
  default_options : ['warning_level=3',
                     'cpp_std=c++23'])

exe = executable('catching', ['catching.cpp', 'other.cpp'],
  cpp_args : ['-static'],
  install : true)

test('basic', exe)
