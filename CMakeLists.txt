cmake_minimum_required(VERSION 3.16)
project(insider-scheme)

option(ASAN "Enable address sanitizer" OFF)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
  set(CMAKE_C_FLAGS_DEBUG "-ggdb3")
  set(CMAKE_CXX_FLAGS_DEBUG "-ggdb3")
endif()

set(scheme_sources
  src/analyser.cpp
  src/bytecode.cpp
  src/compiler.cpp
  src/free_store.cpp
  src/io.cpp
  src/numeric.cpp
  src/scheme.cpp
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

add_library(scheme ${scheme_sources})
target_compile_features(scheme PUBLIC cxx_std_17)
target_compile_options(scheme PRIVATE -Wsuggest-override)
set_property(TARGET scheme PROPERTY CXX_EXTENSIONS OFF)

target_link_libraries(scheme PUBLIC fmt::fmt-header-only)
target_include_directories(scheme INTERFACE src)
set_property(TARGET scheme PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

add_executable(tests ${test_sources})
target_compile_features(tests PUBLIC cxx_std_17)
set_property(TARGET tests PROPERTY CXX_EXTENSIONS OFF)
target_link_libraries(tests
  PUBLIC scheme
  PRIVATE gtest_main)
set_property(TARGET tests PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")

add_executable(insider ${program_sources})
target_compile_features(insider PUBLIC cxx_std_17)
target_compile_options(insider PRIVATE -Wsuggest-override)
set_property(TARGET insider PROPERTY CXX_EXTENSIONS OFF)
target_link_libraries(insider
  PUBLIC scheme)
target_link_libraries(insider PRIVATE fmt::fmt-header-only)
set_property(TARGET insider PROPERTY
  MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
