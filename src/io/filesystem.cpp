#include "context.hpp"
#include "object.hpp"
#include "runtime/basic_types.hpp"
#include "util/define_procedure.hpp"
#include "util/define_struct.hpp"
#include "util/object_span.hpp"

#include <filesystem>
#include <stdexcept>

namespace insider {

namespace fs = std::filesystem;

namespace {

class file_status : public leaf_object<file_status> {
public:
  static constexpr char const* scheme_name = "insider::file_status";

  explicit
  file_status(fs::file_status status)
    : status_{status}
  { }

  unsigned
  permissions() const { return static_cast<unsigned>(status_.permissions()); }

  ptr<symbol>
  type(context& ctx) const;

private:
  fs::file_status status_;
};

} // anonymous namespace

ptr<symbol>
file_status::type(context& ctx) const {
  switch (status_.type()) {
  case fs::file_type::regular: return ctx.intern("regular");
  case fs::file_type::directory: return ctx.intern("directory");
  case fs::file_type::symlink: return ctx.intern("symlink");
  case fs::file_type::block: return ctx.intern("block");
  case fs::file_type::character: return ctx.intern("character");
  case fs::file_type::fifo: return ctx.intern("fifo");
  case fs::file_type::socket: return ctx.intern("socket");
  case fs::file_type::unknown: return ctx.intern("unknown");
  case fs::file_type::not_found: return ctx.intern("not-found");

  case fs::file_type::none:
  default: return ctx.intern("none");
  }
}

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

static ptr<file_status>
status(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] {
    return make<file_status>(ctx, fs::status(p));
  });
}

static ptr<file_status>
symlink_status(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] {
    return make<file_status>(ctx, fs::symlink_status(p));
  });
}

static bool
is_block_file(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::is_block_file(p); });
}

static bool
is_character_file(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::is_character_file(p); });
}

static bool
is_directory(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::is_directory(p); });
}

static bool
is_empty(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::is_empty(p); });
}

static bool
is_fifo(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::is_fifo(p); });
}

static bool
is_other(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::is_other(p); });
}

static bool
is_regular_file(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::is_regular_file(p); });
}

static bool
is_socket(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::is_socket(p); });
}

static bool
is_symlink(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::is_symlink(p); });
}

void
export_filesystem(context& ctx, ptr<module_> result) {
  define_struct<file_status>(ctx, "file-status", result)
    .field<&file_status::permissions>("permissions")
    .field<&file_status::type>("type")
    ;

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
  define_procedure<status>(ctx, "file-status", result);
  define_procedure<symlink_status>(ctx, "symlink-status", result);
  define_procedure<is_block_file>(ctx, "block-file?", result);
  define_procedure<is_character_file>(ctx, "character-file?", result);
  define_procedure<is_directory>(ctx, "directory?", result);
  define_procedure<is_empty>(ctx, "file-empty?", result);
  define_procedure<is_fifo>(ctx, "fifo?", result);
  define_procedure<is_other>(ctx, "file-other?", result);
  define_procedure<is_regular_file>(ctx, "regular-file?", result);
  define_procedure<is_socket>(ctx, "socket?", result);
  define_procedure<is_symlink>(ctx, "symlink?", result);
}

} // namespace insider
