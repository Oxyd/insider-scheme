#include "context.hpp"
#include "runtime/basic_types.hpp"
#include "util/define_procedure.hpp"
#include "util/object_span.hpp"

#include <filesystem>
#include <stdexcept>

namespace insider {

namespace fs = std::filesystem;

static ptr<>
path_elements(context& ctx, fs::path const& p) {
  return make_list_from_range(ctx, p,
                              [] (fs::path const& elem) {
                                return elem.string();
                              });
}

static ptr<>
path_append(context& ctx, object_span args) {
  require_arg_count(args, 1);

  fs::path result;
  for (ptr<> arg : args)
    result /= fs::path{expect<string>(arg)->value()};

  return make<string>(ctx, result.string());
}

static fs::path
path_root_name(fs::path const& p) {
  return p.root_name();
}

static fs::path
path_root_directory(fs::path const& p) {
  return p.root_directory();
}

static fs::path
path_root_path(fs::path const& p) {
  return p.root_path();
}

static fs::path
path_relative_path(fs::path const& p) {
  return p.relative_path();
}

static fs::path
path_parent(fs::path const& p) {
  return p.parent_path();
}

static fs::path
path_filename(fs::path const& p) {
  return p.filename();
}

static fs::path
path_stem(fs::path const& p) {
  return p.stem();
}

static fs::path
path_extension(fs::path const& p) {
  return p.extension();
}

static auto
guard_filesystem_error(context& ctx, auto&& thunk) {
  try {
    return thunk();
  } catch (fs::filesystem_error const& e) {
    throw make<file_error>(ctx, e.what());
  }
}

static fs::path
absolute_path(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::absolute(p); });
}

static fs::path
canonical_path(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::canonical(p); });
}

static fs::path
weakly_canonical_path(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::weakly_canonical(p); });
}

static std::optional<fs::path>
relative_path(context& ctx, fs::path const& p, fs::path const& base) {
  return guard_filesystem_error(
    ctx,
    [&] () -> std::optional<fs::path> {
      auto result = fs::relative(p, base);
      if (!result.empty())
        return result;
      else
        return std::nullopt;
    }
  );
}

static fs::path
lexically_normal_path(fs::path const& p) {
  return p.lexically_normal();
}

static std::optional<fs::path>
lexically_relative_path(fs::path const& p, fs::path const& base) {
  auto result = p.lexically_relative(base);
  if (!result.empty())
    return result;
  else
    return std::nullopt;
}

static fs::path
proximate_path(context& ctx, fs::path const& p, fs::path const& base) {
  return guard_filesystem_error(ctx, [&] { return fs::proximate(p, base); });
}

static fs::path
lexically_proximate_path(fs::path const& p, fs::path const& base) {
  return p.lexically_proximate(base);
}

static bool
file_exists(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::exists(p); });
}

static void
delete_file(context& ctx, fs::path const& p) {
  guard_filesystem_error(ctx, [&] { return fs::remove(p); });
}

static fs::path
current_path(vm& state) {
  return guard_filesystem_error(state.ctx, [] { return fs::current_path(); });
}

static void
set_current_path(context& ctx, fs::path const& new_wd) {
  guard_filesystem_error(ctx, [&] { fs::current_path(new_wd); });
}

void
export_filesystem(context& ctx, ptr<module_> result) {
  define_procedure<path_elements>(ctx, "path-elements", result);
  define_raw_procedure<path_append>(ctx, "path-append", result);
  define_procedure<path_root_name>(ctx, "path-root-name", result);
  define_procedure<path_root_directory>(ctx, "path-root-directory", result);
  define_procedure<path_root_path>(ctx, "path-root-path", result);
  define_procedure<path_relative_path>(ctx, "path-relative-path", result);
  define_procedure<path_parent>(ctx, "path-parent", result);
  define_procedure<path_filename>(ctx, "path-filename", result);
  define_procedure<path_stem>(ctx, "path-stem", result);
  define_procedure<path_extension>(ctx, "path-extension", result);
  define_procedure<absolute_path>(ctx, "absolute-path", result);
  define_procedure<canonical_path>(ctx, "canonical-path", result);
  define_procedure<weakly_canonical_path>(ctx, "weakly-canonical-path", result);
  define_procedure<lexically_normal_path>(ctx, "lexically-normal-path", result);
  define_procedure<lexically_relative_path>(ctx, "lexically-relative-path",
                                            result, current_path);
  define_procedure<lexically_proximate_path>(ctx, "lexically-proximate-path",
                                             result, current_path);
  define_procedure<relative_path>(ctx, "relative-path", result, current_path);
  define_procedure<proximate_path>(ctx, "proximate-path", result, current_path);
  define_procedure<file_exists>(ctx, "file-exists?", result);
  define_procedure<delete_file>(ctx, "delete-file", result);
  define_procedure<current_path>(ctx, "current-path", result);
  define_procedure<set_current_path>(
    ctx, "set-current-path!", result
  );
}

} // namespace insider
