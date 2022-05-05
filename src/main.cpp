#include "compiler/compiler.hpp"
#include "compiler/source_code_provider.hpp"
#include "context.hpp"
#include "runtime/action.hpp"
#include "vm/vm.hpp"

#include <fmt/format.h>

#include <cstdio>
#include <string>

#ifdef WIN32
#include <Windows.h>
#endif

#ifdef WIN32
static void
enable_virtual_terminal_processing() {
  if (HANDLE stdout_handle = GetStdHandle(STD_OUTPUT_HANDLE); stdout_handle != INVALID_HANDLE_VALUE) {
    DWORD mode = 0;
    GetConsoleMode(stdout_handle, &mode);
    SetConsoleMode(stdout_handle, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
  }
}
#endif

static void
print_usage(char const* program_name) {
  fmt::print("{} [<options> ...] <file>\n", program_name);
  fmt::print("Options:\n"
             "  -I <directory>  -- add directory to module search path\n");
}

int
main(int argc, char** argv) {
  using namespace std::literals;

#ifdef WIN32
  enable_virtual_terminal_processing();
#endif

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
          ctx.append_source_code_provider(std::make_unique<insider::filesystem_source_code_provider>(argument));
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

    auto mod = insider::compile_module(ctx, program_path, true);
    insider::simple_action a{ctx, "Executing program"};
    insider::execute(ctx, mod);
    return 0;
  }
  catch (std::runtime_error const& e) {
    std::fflush(stdout);
    fmt::print(stderr, "Error: {}\n{}\n", e.what(), ctx.error_backtrace);
  }
}
