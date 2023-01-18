#include "compiler/compilation_config.hpp"
#include "compiler/compiler.hpp"
#include "compiler/source_code_provider.hpp"
#include "context.hpp"
#include "io/port.hpp"
#include "io/read.hpp"
#include "io/write.hpp"
#include "runtime/parameter_map.hpp"
#include "runtime/syntax.hpp"
#include "vm/stacktrace.hpp"
#include "vm/vm.hpp"

#include <fmt/format.h>

#include <cstdio>
#include <memory>
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
    std::string              interaction_environment_specifier
                               = "(insider interactive)";
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
  fmt::print("{} [<options> ...] [<file>]", program_name);
  fmt::print(R"(
Options:
  -I <directory>    -- add directory to module search path
  -x <module name>  -- set interaction-environment module name;
                       default: (insider interactive)
)");
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
      else if (flag == "-x")
        opts.interaction_environment_specifier = std::move(argument);
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

static std::string
format_error(insider::context& ctx, std::string const& message) {
  std::string result = fmt::format("Error: {}", message);

  if (!ctx.last_exception_stacktrace.empty())
    result += "\n" + insider::format_stacktrace(ctx.last_exception_stacktrace);

  if (!ctx.action_backtrace.empty())
    result += "\n" + ctx.action_backtrace;

  return result;
}

static void
show_error(insider::context& ctx, insider::scheme_exception const& e) {
  std::fflush(stdout);
  fmt::print(
    stderr, "{}\n",
    format_error(ctx, insider::datum_to_display_string(ctx, e.object.get()))
  );
}

static void
show_error(insider::context& ctx, std::runtime_error const& e) {
  std::fflush(stdout);
  fmt::print(stderr, "{}\n", format_error(ctx, e.what()));
}

static void
run_program(insider::context& ctx, std::string const& program_path,
            insider::compilation_config const& config) {
  auto mod = insider::compile_module(ctx, program_path, config, true);
  insider::execute(ctx, mod);
}

static void
run_repl(insider::context& ctx, insider::compilation_config const& config) {
  insider::tracked_ptr<insider::module_> repl_mod
    = insider::interaction_environment(ctx);
  insider::tracked_ptr<insider::textual_input_port> input_port
    = insider::track(ctx, insider::get_global_textual_input_port(ctx));
  insider::tracked_ptr<insider::textual_output_port> output_port
    = insider::track(ctx, insider::get_global_textual_output_port(ctx));

  while (true)
    try {
      output_port->write("> ");
      output_port->flush();

      insider::ptr<> expr
        = insider::read_syntax(ctx, input_port.get());
      if (auto stx = insider::match<insider::syntax>(expr)) {
        insider::ptr<> result = insider::eval(ctx, repl_mod, stx, config);
        if (result != ctx.constants->void_) {
          insider::write(ctx, result, output_port.get());
          output_port->write("\n");
        }
      } else
        return;
    } catch (insider::scheme_exception const& e) {
      output_port->write(
        format_error(ctx, insider::datum_to_display_string(ctx, e.object.get()))
      );
      output_port->write('\n');
    } catch (std::runtime_error const& e) {
      output_port->write(format_error(ctx, e.what()));
      output_port->write('\n');
    }
}

static int
run(int argc, char** argv, insider::context& ctx) {
  options opts = parse_options(argc, argv);

  for (std::string const& path : opts.module_search_paths)
    ctx.module_resolver().append_source_code_provider(
      std::make_unique<insider::filesystem_source_code_provider>(path)
    );

  ctx.parameters->set_value(
    ctx.store,
    ctx.constants->interaction_environment_specifier_tag,
    insider::read(ctx, opts.interaction_environment_specifier)
  );

  auto config = insider::compilation_config::optimisations_config(
    std::make_unique<insider::stdout_diagnostic_sink>()
  );

  if (opts.program_path.empty())
    run_repl(ctx, config);
  else
    run_program(ctx, opts.program_path, config);

  return 0;
}

int
main(int argc, char** argv) {
  using namespace std::literals;

#ifdef WIN32
  enable_virtual_terminal_processing();
#endif

  insider::context ctx;
  try {
    return run(argc, argv, ctx);
  } catch (options_parse_error const&) {
    print_usage(argv[0]);
    return 1;
  } catch (insider::scheme_exception const& e) {
    show_error(ctx, e);
    return 1;
  } catch (std::runtime_error const& e) {
    show_error(ctx, e);
    return 1;
  }
}
