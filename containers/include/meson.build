inc_dir = include_directories('.')

# Make this library usable from the system's
# package manager.
headers = [
  'ctr/alloc.hpp',
  'ctr/contiguous_container.hpp',
  'ctr/function.hpp',
  'ctr/multi_object_vector.hpp',
  'ctr/normal_iterator.hpp',
  'ctr/ref.hpp',
  'ctr/static_vector.hpp',
  'ctr/type_traits.hpp',
  'ctr/uninitialized_array.hpp',
  'ctr/util.hpp',
]

install_headers(headers, preserve_path: true)

dependencies = [
  dependency('tl-optional', required: true),
]

# Make this library usable as a Meson subproject.
ctr_dep = declare_dependency(
  include_directories: inc_dir,
  dependencies: dependencies,
)
