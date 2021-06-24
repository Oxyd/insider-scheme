cmake_minimum_required(VERSION 3.16)
project(insider-scheme)

option(ASAN "Enable address sanitizer" OFF)
option(UBSAN "Enable UB sanitizer" OFF)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
  set(CMAKE_C_FLAGS "-Wall -Wextra -pedantic -fno-omit-frame-pointer")
  set(CMAKE_CXX_FLAGS "-Wall -Wextra -pedantic -fno-omit-frame-pointer")
  set(CMAKE_C_FLAGS_DEBUG "-ggdb3")
  set(CMAKE_CXX_FLAGS_DEBUG "-ggdb3")
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "-O3 -ggdb3 -DNDEBUG")
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O3 -ggdb3 -DNDEBUG")
endif()

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
  add_definitions(-DUNICODE -D_UNICODE)
endif()

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(scheme_sources
  src/analyser.cpp
  src/basic_types.cpp
  src/bytecode.cpp
  src/call_stack.cpp
  src/compare.cpp
  src/compiler.cpp
  src/context.cpp
  src/expression.cpp
  src/free_store.cpp
  src/input_stream.cpp
  src/internal_module.cpp
  src/io.cpp
  src/module.cpp
  src/numeric.cpp
  src/object.cpp
  src/port.cpp
  src/scope.cpp
  src/source_location.cpp
  src/syntax.cpp
  src/vm.cpp
  )

set(test_sources
  test/test_scheme.cpp
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
target_include_directories(scheme INTERFACE src)
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
