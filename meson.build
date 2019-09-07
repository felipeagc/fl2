project('fl2', 'c', 'cpp')

cc = meson.get_compiler('c')
m_dep = cc.find_library('m', required: false)

compiler_lib_sources = [
	'src/position.c',
	'src/str_builder.c',
	'src/bump_alloc.c',
	'src/table.c',
	'src/token.c',
	'src/scope.c',

	'src/context.c',
	'src/scanner.c',
	'src/parser.c',
	'src/analyzer.c',
]

compiler_exe_sources = [
	'src/main.c'
]

compiler_lib = library('fl', compiler_lib_sources)
compiler_dep = declare_dependency(link_with: compiler_lib, dependencies: [m_dep])

compiler_exe = executable('fl', compiler_exe_sources, dependencies: [compiler_dep])

test_sources = [
	'src/tests.c'
]
tests_exe = executable('fl_tests', test_sources, link_with: compiler_lib)
test('fl tests', tests_exe)

