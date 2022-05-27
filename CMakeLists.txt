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
          $<$<CXX_COMPILER_ID:GNU>:-Wno-stringop-overflow>
          $<$<CXX_COMPILER_ID:GNU>:-Wno-restrict>
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
          /MP
          /utf-8
          /bigobj
          $<$<CONFIG:RELEASE>:NDEBUG>)
endif()

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(scheme_sources
        src/compiler/analyser.cpp
        src/compiler/compiler.cpp
        src/compiler/module_name.cpp
        src/compiler/module_specifier.cpp
        src/compiler/scope.cpp
        src/compiler/source_code_provider.cpp
        src/compiler/source_location.cpp
        src/context.cpp
        src/io/port.cpp
        src/io/read.cpp
        src/io/reader_stream.cpp
        src/io/write.cpp
        src/memory/free_store.cpp
        src/memory/root_list.cpp
        src/memory/root_provider.cpp
        src/module.cpp
        src/module_resolver.cpp
        src/object.cpp
        src/runtime/basic_types.cpp
        src/runtime/character.cpp
        src/runtime/code_point_properties.cpp
        src/runtime/compare.cpp
        src/runtime/error.cpp
        src/runtime/internal_module.cpp
        src/runtime/numeric.cpp
        src/runtime/records.cpp
        src/runtime/string.cpp
        src/runtime/syntax.cpp
        src/runtime/time.cpp
        src/vm/bytecode.cpp
        src/vm/call_stack.cpp
        src/vm/execution_state.cpp
        src/vm/vm.cpp
        )

set(test_sources
        test/scheme_fixture.cpp
        test/test_bytecode.cpp
        test/test_call_stack.cpp
        test/test_character.cpp
        test/test_compiler.cpp
        test/test_control.cpp
        test/test_define_procedure.cpp
        test/test_gc.cpp
        test/test_interpreter.cpp
        test/test_io.cpp
        test/test_macros.cpp
        test/test_modules.cpp
        test/test_numeric.cpp
        test/test_port.cpp
        test/test_procedures.cpp
        test/test_reader_stream.cpp
        test/test_records.cpp
        test/test_string.cpp
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
