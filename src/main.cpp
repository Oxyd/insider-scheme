#include "compiler.hpp"
#include "io.hpp"
#include "scheme.hpp"
#include "vm.hpp"

#include <fmt/format.h>

#include <cstdio>
#include <cstring>

int
main(int argc, char** argv) {
  try {
    if (argc == 2) {
      FILE* f = std::fopen(argv[1], "r");
      if (!f) {
        char* err_desc = strerror(errno);
        fmt::print(stderr, "Can't open input file {}: {}\n", argv[1], err_desc);
        return 1;
      }

      scm::context ctx;
      auto in = scm::make<scm::port>(ctx, f, true, false);
      auto mod = scm::compile_main_module(ctx, scm::read_multiple(ctx, in));
      scm::execute(ctx, mod);
      return 0;
    }
    else {
      fmt::print("{} <file>\n", argv[0]);
      return 1;
    }
  }
  catch (std::runtime_error const& e) {
    fmt::print(stderr, "Error: {}\n", e.what());
  }
}
