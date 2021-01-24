#include "action.hpp"
#include "compiler.hpp"
#include "io.hpp"
#include "scheme.hpp"
#include "vm.hpp"

#include <fmt/format.h>

#include <cstdio>
#include <cstring>
#include <string>

static void
print_usage(char const* program_name) {
  fmt::print("{} [<options> ...] <file>\n", program_name);
  fmt::print("Options:\n"
             "  -I <directory>  -- add directory to module search path\n");
}

int
main(int argc, char** argv) {
  using namespace std::literals;

  std::string program_path;
  insider::context ctx;

  try {
    for (int i = 1; i < argc; ++i) {
      if (argv[i][0] == '-') {
        std::string flag = argv[i];

        if (flag == "-") {
          print_usage(argv[0]);
          return 1;
        }

        std::string argument;
        if (flag.size() > 2) {
          argument = flag.substr(2);
          flag = flag.substr(0, 2);
        } else {
          if (i + 1 == argc) {
            print_usage(argv[0]);
            return 1;
          }

          argument = argv[i + 1];
          ++i;
        }

        if (flag == "-I")
          ctx.append_module_provider(std::make_unique<insider::filesystem_module_provider>(argument));
        else {
          print_usage(argv[0]);
          return 1;
        }
      }
      else {
        if (!program_path.empty()) {
          print_usage(argv[0]);
          return 1;
        }

        program_path = argv[i];
      }
    }

    if (program_path.empty()) {
      print_usage(argv[0]);
      return 1;
    }

    FILE* f = std::fopen(program_path.c_str(), "r");
    if (!f) {
      fmt::print(stderr, "Can't open input file {}: {}\n", program_path, strerror(errno));
      return 1;
    }

    auto in = insider::make_tracked<insider::port>(ctx, f, std::move(program_path), true, false);
    auto mod = insider::compile_main_module(ctx, insider::read_multiple(ctx, in.get()));

    insider::simple_action a{ctx, "Executing program"};
    insider::execute(ctx, mod);
    return 0;
  }
  catch (std::runtime_error const& e) {
    std::fflush(stdout);
    fmt::print(stderr, "Error: {}\n{}\n", e.what(), ctx.error_backtrace);
  }
}
