cmake_minimum_required(VERSION 3.16)
project(insider-scheme)

option(ASAN "Enable address sanitizer" OFF)
option(UBSAN "Enable UB sanitizer" OFF)

if (${CMAKE_CXX_COMPILER_ID} MATCHES "GNU|Clang")
  set(compile_flags
          -Wall
          -Wextra
          -pedantic
          -fno-omit-frame-pointer
          -Wsuggest-override
          -Wextra-semi
          $<$<CXX_COMPILER_ID:GNU>:-Wno-stringop-overflow>
          $<$<CXX_COMPILER_ID:GNU>:-Wno-restrict>
          $<$<CXX_COMPILER_ID:GNU>:-Wno-maybe-uninitialized>
          $<$<CXX_COMPILER_ID:GNU>:-Wno-dangling-reference>
          $<$<CXX_COMPILER_ID:GNU>:-Wno-array-bounds>
          $<$<CXX_COMPILER_ID:GNU>:-fconcepts-diagnostics-depth=4>
          $<$<CONFIG:DEBUG>:-ggdb3>
          $<$<CONFIG:RELWITHDEBINFO>:-O3 -ggdb3 -UNDEBUG>
          $<$<CONFIG:RELEASE>:-O3 -DNDEBUG>
          $<$<CONFIG:PROFILE>:-O3 -DNDEBUG -ggdb3>
          $<$<BOOL:${ASAN}>:-fsanitize=address>
          $<$<BOOL:${UBSAN}>:-fsanitize=undefined>)

  set(link_flags
          $<$<BOOL:${ASAN}>:-fsanitize=address>
          $<$<BOOL:${UBSAN}>:-fsanitize=undefined>)
else()
  set(compile_flags
          /W4
          /wd4324
          /wd4146
          /wd4456
          /wd4457
          /wd4458
          /wd4459
          /wd4996
          /wd4702
          /MP
          /utf-8
          /bigobj
          /DUNICODE
          /D_UNICODE
          $<$<CONFIG:RELEASE>:NDEBUG>)
endif()

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(scheme_sources
        src/compiler/analyse_free_variables_pass.cpp
        src/compiler/analyse_free_variables_pass.hpp
        src/compiler/analyse_variables_pass.cpp
        src/compiler/analyse_variables_pass.hpp
        src/compiler/analyser.cpp
        src/compiler/analyser.hpp
        src/compiler/ast.cpp
        src/compiler/ast.hpp
        src/compiler/ast_transforms.cpp
        src/compiler/ast_transforms.hpp
        src/compiler/box_set_variables_pass.cpp
        src/compiler/box_set_variables_pass.hpp
        src/compiler/cfg.cpp
        src/compiler/cfg.hpp
        src/compiler/clone_ast.cpp
        src/compiler/clone_ast.hpp
        src/compiler/compilation_config.cpp
        src/compiler/compilation_config.hpp
        src/compiler/compiler.cpp
        src/compiler/compiler.hpp
        src/compiler/debug_info.hpp
        src/compiler/evaluate_constants_pass.cpp
        src/compiler/evaluate_constants_pass.hpp
        src/compiler/expression.hpp
        src/compiler/find_self_variables_pass.cpp
        src/compiler/find_self_variables_pass.hpp
        src/compiler/inline_built_in_operations_pass.cpp
        src/compiler/inline_built_in_operations_pass.hpp
        src/compiler/inline_procedures_pass.cpp
        src/compiler/inline_procedures_pass.hpp
        src/compiler/make_loop_temporaries_pass.cpp
        src/compiler/make_loop_temporaries_pass.hpp
        src/compiler/module_name.cpp
        src/compiler/module_name.hpp
        src/compiler/module_specifier.cpp
        src/compiler/module_specifier.hpp
        src/compiler/optimise_applications_pass.cpp
        src/compiler/optimise_applications_pass.hpp
        src/compiler/parser_expander.cpp
        src/compiler/parser_expander.hpp
        src/compiler/parsing_context.cpp
        src/compiler/parsing_context.hpp
        src/compiler/registers.cpp
        src/compiler/registers.hpp
        src/compiler/remove_unnecessary_definitions_pass.cpp
        src/compiler/remove_unnecessary_definitions_pass.hpp
        src/compiler/scope.cpp
        src/compiler/scope.hpp
        src/compiler/source_code_provider.cpp
        src/compiler/source_code_provider.hpp
        src/compiler/source_file_origin.cpp
        src/compiler/source_file_origin.hpp
        src/compiler/source_location.cpp
        src/compiler/source_location.hpp
        src/compiler/update_variables_pass.cpp
        src/compiler/update_variables_pass.hpp
        src/compiler/variable.cpp
        src/compiler/variable.hpp
        src/context.cpp
        src/context.hpp
        src/io/filesystem.cpp
        src/io/port.cpp
        src/io/port.hpp
        src/io/read.cpp
        src/io/read.hpp
        src/io/reader_stream.cpp
        src/io/reader_stream.hpp
        src/io/tokenizer.cpp
        src/io/tokenizer.hpp
        src/io/write.cpp
        src/io/write.hpp
        src/memory/allocator.hpp
        src/memory/free_store.cpp
        src/memory/free_store.hpp
        src/memory/member.hpp
        src/memory/preserve.hpp
        src/memory/root.hpp
        src/memory/root_provider.cpp
        src/memory/root_provider.hpp
        src/module.cpp
        src/module.hpp
        src/module_resolver.cpp
        src/module_resolver.hpp
        src/object.cpp
        src/object.hpp
        src/runtime/basic_types.cpp
        src/runtime/basic_types.hpp
        src/runtime/character.cpp
        src/runtime/character.hpp
        src/runtime/code_point_properties.cpp
        src/runtime/code_point_properties.hpp
        src/runtime/compare.cpp
        src/runtime/compare.hpp
        src/runtime/error.cpp
        src/runtime/error.hpp
        src/runtime/internal_module.cpp
        src/runtime/internal_module.hpp
        src/runtime/numeric.cpp
        src/runtime/numeric.hpp
        src/runtime/parameter_map.cpp
        src/runtime/parameter_map.hpp
        src/runtime/records.cpp
        src/runtime/records.hpp
        src/runtime/string.cpp
        src/runtime/string.hpp
        src/runtime/syntax.cpp
        src/runtime/syntax.hpp
        src/runtime/time.cpp
        src/runtime/time.hpp
        src/type_indexes.hpp
        src/util/define_procedure.hpp
        src/util/define_struct.hpp
        src/util/depth_first_search.hpp
        src/util/from_scheme.hpp
        src/util/integer_cast.hpp
        src/util/list_iterator.hpp
        src/util/named_runtime_error.hpp
        src/util/object_conversions.hpp
        src/util/object_span.hpp
        src/util/parameterize.hpp
        src/util/sum_type.hpp
        src/util/to_scheme.hpp
        src/vm/bytecode.cpp
        src/vm/bytecode.hpp
        src/vm/call_stack.cpp
        src/vm/call_stack.hpp
        src/vm/stacktrace.cpp
        src/vm/stacktrace.hpp
        src/vm/vm.cpp
        src/vm/vm.hpp
        )

Set(test_sources
        test/scheme_fixture.cpp
        test/test_ast.cpp
        test/test_bytecode.cpp
        test/test_call_stack.cpp
        test/test_cfg.cpp
        test/test_character.cpp
        test/test_compiler.cpp
        test/test_control.cpp
        test/test_conversion.cpp
        test/test_define_procedure.cpp
        test/test_define_struct.cpp
        test/test_depth_first_search.cpp
        test/test_gc.cpp
        test/test_interpreter.cpp
        test/test_io.cpp
        test/test_macros.cpp
        test/test_modules.cpp
        test/test_numeric.cpp
        test/test_parser.cpp
        test/test_port.cpp
        test/test_procedures.cpp
        test/test_reader_stream.cpp
        test/test_records.cpp
        test/test_string.cpp
        test/test_sum_type.cpp
        test/test_syntax.cpp
        test/test_types.cpp
        )

set(program_sources
        src/main.cpp)

add_subdirectory(extern/fmt)
add_subdirectory(extern/googletest)

set_property(TARGET gtest PROPERTY
        MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
set_property(TARGET gtest_main PROPERTY
        MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

add_library(scheme ${scheme_sources})
target_compile_options(scheme PRIVATE ${compile_flags})
target_link_libraries(scheme PUBLIC fmt::fmt-header-only)
target_include_directories(scheme PUBLIC src)
set_property(TARGET scheme PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

add_executable(tests ${test_sources})
target_compile_options(tests PRIVATE ${compile_flags})
target_link_options(tests PRIVATE ${link_flags})
target_include_directories(tests PRIVATE test)
target_link_libraries(tests
  PUBLIC scheme
  PRIVATE gtest_main)
set_property(TARGET tests PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

add_executable(insider ${program_sources})
target_compile_options(insider PRIVATE ${compile_flags})
target_link_options(insider PRIVATE ${link_flags})
target_link_libraries(insider
  PUBLIC scheme)
set_property(TARGET insider PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
