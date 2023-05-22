#include "context.hpp"
#include "runtime/basic_types.hpp"
#include "util/define_procedure.hpp"

#include <filesystem>

namespace insider {

auto
guard_filesystem_error(context& ctx, auto&& thunk) {
  try {
    return thunk();
  } catch (std::filesystem::filesystem_error const& e) {
    throw make<file_error>(ctx, e.what());
  }
}

static bool
file_exists(context& ctx, std::filesystem::path const& p) {
  return guard_filesystem_error(ctx, [&] { return std::filesystem::exists(p); });
}

static void
delete_file(context& ctx, std::filesystem::path const& p) {
  guard_filesystem_error(ctx, [&] { return std::filesystem::remove(p); });
}

static std::string
current_working_directory(context& ctx) {
  return guard_filesystem_error(ctx,
                                [] {
                                  return std::filesystem::current_path();
                                });
}

static void
set_current_working_directory(context& ctx, std::string const& new_wd) {
  guard_filesystem_error(ctx,
                         [&] {
                           std::filesystem::current_path(new_wd);
                         });
}

void
export_filesystem(context& ctx, ptr<module_> result) {
  define_procedure<file_exists>(ctx, "file-exists?", result);
  define_procedure<delete_file>(ctx, "delete-file", result);
  define_procedure<current_working_directory>(ctx,
                                              "current-working-directory",
                                              result);
  define_procedure<set_current_working_directory>(
    ctx, "set-current-working-directory!", result
  );
}

} // namespace insider
