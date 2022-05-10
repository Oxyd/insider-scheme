cmake_minimum_required(VERSION 3.16)
project(insider-scheme)

option(ASAN "Enable address sanitizer" OFF)
option(UBSAN "Enable UB sanitizer" OFF)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
  set(CMAKE_C_FLAGS "-Wall -Wextra -pedantic -fno-omit-frame-pointer")
  set(CMAKE_CXX_FLAGS "-Wall -Wextra -pedantic -fno-omit-frame-pointer -Wsuggest-override")
  set(CMAKE_C_FLAGS_DEBUG "-ggdb3")
  set(CMAKE_CXX_FLAGS_DEBUG "-ggdb3")
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "-O3 -ggdb3")
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O3 -ggdb3")
  set(CMAKE_C_FLAGS_PROFILE "-O3 -ggdb3 -DNDEBUG -fasynchronous-unwind-tables")
  set(CMAKE_CXX_FLAGS_PROFILE "-O3 -ggdb3 -DNDEBUG -fasynchronous-unwind-tables")
  set(CMAKE_C_FLAGS_RELEASE "-O3 -DNDEBUG")
  set(CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG")
endif()

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
  add_compile_options("/W4")
  add_compile_options("/wd4324")
  add_compile_options("/wd4146")
  add_compile_options("/wd4456")
  add_compile_options("/wd4457")
  add_compile_options("/wd4458")
  add_compile_options("/wd4459")
  add_compile_options("/wd4702")
  add_compile_options("/wd4996")
  add_compile_options("/MP")
  add_compile_options("/utf-8")
  add_compile_options("/bigobj")
  add_compile_definitions("$<$<CONFIG:RELEASE>:NDEBUG>")
  add_link_options("$<$<CONFIG:DEBUG>:/DYNAMICBASE:NO>")
  add_definitions(-DUNICODE -D_UNICODE)
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

add_subdirectory(${FMT_SUBDIRECTORY} ${CMAKE_CURRENT_BINARY_DIR}/fmt)
add_subdirectory(${GOOGLETEST_SUBDIRECTORY} ${CMAKE_CURRENT_BINARY_DIR}/googletest)

set_property(TARGET gtest PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
set_property(TARGET gtest_main PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

if (ASAN AND UNIX)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address")
endif()

if (UBSAN AND UNIX)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=undefined")
endif()

add_library(scheme ${scheme_sources})

target_link_libraries(scheme PUBLIC fmt::fmt-header-only)
target_include_directories(scheme PUBLIC src)
set_property(TARGET scheme PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
  target_compile_options(scheme PUBLIC -Wsuggest-override)
endif()

add_executable(tests ${test_sources})
target_link_libraries(tests
  PUBLIC scheme
  PRIVATE gtest_main)
set_property(TARGET tests PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

add_executable(insider ${program_sources})
target_link_libraries(insider
  PUBLIC scheme)
set_property(TARGET insider PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
