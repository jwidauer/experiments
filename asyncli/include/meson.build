inc_dir = include_directories('.')

# Make this library usable from the system's
# package manager.
headers = []

install_headers(headers, preserve_path: true)

dependencies = [
  dependency('containers', required: true),
  dependency('magic_enum', required: true),
]

compile_args = ['-Wno-gnu-statement-expression']

# Make this library usable as a Meson subproject.
asyncli_dep = declare_dependency(
  include_directories: inc_dir,
  dependencies: dependencies,
  compile_args: compile_args,
)
