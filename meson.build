project('fl2', 'c', 'cpp')

llvm_dep = dependency('llvm', version: '>= 8.0')

cc = meson.get_compiler('c')
m_dep = cc.find_library('m', required: false)

compiler_lib_sources = [
	'src/position.c',
	'src/str_builder.c',
	'src/bump_alloc.c',
	'src/table.c',
	'src/token.c',
	'src/scope.c',
	'src/filesystem.c',
	'src/result.c',

	'src/context.c',
	'src/type.c',
	'src/expr.c',
	'src/scanner.c',
	'src/parser.c',
	'src/analyzer.c',
	'src/codegen_llvm.c',
]

compiler_exe_sources = [
	'src/main.c'
]

compiler_deps = [
	llvm_dep,
	m_dep
]

compiler_inc_dirs = include_directories('src')
compiler_lib = static_library('fl', compiler_lib_sources, dependencies: compiler_deps)
compiler_dep = declare_dependency(link_with: compiler_lib, dependencies: compiler_deps, include_directories: compiler_inc_dirs)

compiler_exe = executable('fl', compiler_exe_sources, dependencies: [compiler_dep], install: true)

test_script = find_program(meson.source_root() + '/tests/test.sh')
test('fl tests',
	test_script,
	args: [compiler_exe.full_path(),
				meson.source_root() + '/tests'])

