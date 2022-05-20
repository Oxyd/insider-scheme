#include "compiler/compiler.hpp"
#include "compiler/source_code_provider.hpp"
#include "context.hpp"
#include "io/port.hpp"
#include "io/read.hpp"
#include "runtime/action.hpp"
#include "runtime/syntax.hpp"
#include "vm/vm.hpp"

#include <fmt/format.h>

#include <cstdio>
#include <stdexcept>
#include <string>

#ifdef WIN32
#include <Windows.h>
#endif

#ifdef WIN32
static void
enable_virtual_terminal_processing() {
  if (HANDLE stdout_handle = GetStdHandle(STD_OUTPUT_HANDLE);
      stdout_handle != INVALID_HANDLE_VALUE) {
    DWORD mode = 0;
    GetConsoleMode(stdout_handle, &mode);
    SetConsoleMode(stdout_handle, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
  }
}
#endif

namespace {
  struct options {
    bool                     help = false;
    std::string              program_path;
    std::vector<std::string> module_search_paths;
  };

  class options_parse_error : public std::runtime_error {
  public:
    options_parse_error()
      : std::runtime_error{"Bad option"}
    { }
  };
}

static void
print_usage(char const* program_name) {
  fmt::print("{} [<options> ...] [<file>]\n", program_name);
  fmt::print("Options:\n"
             "  -I <directory>  -- add directory to module search path\n");
}

static options
parse_options(int argc, char** argv) {
  options opts;

  for (int i = 1; i < argc; ++i) {
    if (argv[i][0] == '-') {
      std::string flag = argv[i];

      if (flag == "-")
        throw options_parse_error{};

      std::string argument;
      if (flag.size() > 2) {
        argument = flag.substr(2);
        flag = flag.substr(0, 2);
      } else {
        if (i + 1 == argc)
          throw options_parse_error{};

        argument = argv[i + 1];
        ++i;
      }

      if (flag == "-I")
        opts.module_search_paths.emplace_back(std::move(argument));
      else
        throw options_parse_error{};
    } else {
      if (!opts.program_path.empty())
        throw options_parse_error{};

      opts.program_path = argv[i];
    }
  }

  return opts;
}

static void
run_program(insider::context& ctx, options const& opts) {
  auto mod = insider::compile_module(ctx, opts.program_path, true);
  insider::simple_action a{ctx, "Executing program"};
  insider::execute(ctx, mod);
}

static std::string
format_error(insider::context& ctx, std::runtime_error const& e) {
  if (!ctx.error_backtrace.empty())
    return fmt::format("Error: {}\n{}\n", e.what(), ctx.error_backtrace);
  else
    return fmt::format("Error: {}\n", e.what());
}

static void
run_repl(insider::context& ctx) {
  insider::tracked_ptr<insider::module_> repl_mod
    = insider::make_interactive_module(
        ctx,
        insider::import_modules(insider::module_name{"insider", "internal"})
      );
  insider::tracked_ptr<insider::textual_input_port> input_port
    = insider::track(ctx, insider::get_current_textual_input_port(ctx));
  insider::tracked_ptr<insider::textual_output_port> output_port
    = insider::track(ctx, insider::get_current_textual_output_port(ctx));

  while (true)
    try {
      output_port->write("> ");

      insider::ptr<> expr
        = insider::read_syntax(ctx, input_port.get());
      if (auto stx = insider::match<insider::syntax>(expr)) {
        insider::tracked_ptr<> result = insider::eval(ctx, repl_mod, stx);
        insider::write(ctx, result.get(), output_port.get());
        output_port->write("\n");
      } else
        return;
    } catch (std::runtime_error const& e) {
      output_port->write(format_error(ctx, e));
    }
}

int
main(int argc, char** argv) {
  using namespace std::literals;

#ifdef WIN32
  enable_virtual_terminal_processing();
#endif

  insider::context ctx;
  try {
    options opts = parse_options(argc, argv);

    for (std::string const& path : opts.module_search_paths)
      ctx.module_resolver().append_source_code_provider(
        std::make_unique<insider::filesystem_source_code_provider>(path)
      );

    if (opts.program_path.empty())
      run_repl(ctx);
    else
      run_program(ctx, opts);

    return 0;
  }
  catch (options_parse_error const&) {
    print_usage(argv[0]);
    return 1;
  } catch (std::runtime_error const& e) {
    std::fflush(stdout);
    fmt::print(stderr, "{}", format_error(ctx, e));
  }
}
