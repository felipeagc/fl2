project('fl2', 'c', 'cpp')

compiler_lib_sources = [
	'src/position.c',
	'src/str_builder.c',
	'src/bump_alloc.c',

	'src/scanner.c',
	# 'src/parser.c',
]

compiler_exe_sources = [
	'src/main.c'
]

compiler_lib = library('fl', compiler_lib_sources)
compiler_exe = executable('fl', compiler_exe_sources, link_with: [compiler_lib])

test_sources = [
	'src/tests.c'
]
tests_exe = executable('fl_tests', test_sources, link_with: compiler_lib)
test('fl tests', tests_exe)

