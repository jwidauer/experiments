doctest_dep = dependency('doctest')

sources = ['main.cpp', 'soa_vec_test.cpp']

test_exe = executable(
    'ctr_test',
    sources,
    dependencies: [ctr_dep, doctest_dep],
)

test('ctr_test', test_exe)
