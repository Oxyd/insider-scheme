#include "context.hpp"
#include "object.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/time.hpp"
#include "util/define_procedure.hpp"
#include "util/object_span.hpp"
#include "util/sum_type.hpp"
#include "util/symbolic_enum.hpp"

#include <chrono>
#include <cstdint>
#include <filesystem>
#include <ranges>
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

static bool
delete_file(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::remove(p); });
}

static std::uintmax_t
delete_all(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::remove_all(p); });
}

static fs::path
current_path(vm& state) {
  return guard_filesystem_error(state.ctx, [] { return fs::current_path(); });
}

static void
set_current_path(context& ctx, fs::path const& new_wd) {
  guard_filesystem_error(ctx, [&] { fs::current_path(new_wd); });
}

static ptr<symbol>
file_type_to_scheme(context& ctx, fs::file_type type) {
  switch (type) {
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
status_to_scheme(context& ctx, fs::file_status status) {
  return make_list(ctx,
                   file_type_to_scheme(ctx, status.type()),
                   to_scheme(ctx, static_cast<unsigned>(status.permissions())));
}

static ptr<>
status(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] {
    return status_to_scheme(ctx, fs::status(p));
  });
}

static ptr<>
symlink_status(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] {
    return status_to_scheme(ctx, fs::symlink_status(p));
  });
}

static void
modify_permissions(context& ctx, fs::path const& p, unsigned perms,
                   bool follow_symlinks, fs::perm_options mode) {
  guard_filesystem_error(
    ctx,
    [&] {
      if (!follow_symlinks)
        mode |= fs::perm_options::nofollow;
      fs::permissions(p, static_cast<fs::perms>(perms), mode);
    }
  );
}

static void
set_permissions(context& ctx, fs::path const& p, unsigned new_perms,
                bool follow_symlinks) {
  modify_permissions(ctx, p, new_perms, follow_symlinks,
                     fs::perm_options::replace);
}

static void
add_permissions(context& ctx, fs::path const& p, unsigned more_perms,
                bool follow_symlinks) {
  modify_permissions(ctx, p, more_perms, follow_symlinks,
                     fs::perm_options::add);
}

static void
remove_permissions(context& ctx, fs::path const& p, unsigned perms,
                   bool follow_symlinks) {
  modify_permissions(ctx, p, perms, follow_symlinks,
                     fs::perm_options::remove);
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

namespace {
  enum class when_exists_values {
    error, skip, overwrite, update
  };

  struct when_exists_symbolic_def {
    using values = when_exists_values;
    static constexpr std::tuple<char const*, when_exists_values> mapping[]{
      {"error", when_exists_values::error},
      {"skip", when_exists_values::skip},
      {"overwrite", when_exists_values::overwrite},
      {"update", when_exists_values::update}
    };
  };

  enum class copy_symlinks_values {
    follow, copy, skip
  };

  struct copy_symlinks_symbolic_def {
    using values = copy_symlinks_values;
    static constexpr std::tuple<char const*, copy_symlinks_values> mapping[]{
      {"follow", copy_symlinks_values::follow},
      {"copy", copy_symlinks_values::copy},
      {"skip", copy_symlinks_values::skip}
    };
  };

  enum class copy_type_values {
    copy_content, directories_only, create_symlinks, create_hard_links
  };

  struct copy_type_symbolic_def {
    using values = copy_type_values;
    static constexpr std::tuple<char const*, copy_type_values> mapping[]{
      {"copy-content", copy_type_values::copy_content},
      {"directories-only", copy_type_values::directories_only},
      {"create-symlinks", copy_type_values::create_symlinks},
      {"create-hard-links", copy_type_values::create_hard_links}
    };
  };
}

using when_exists_enum = symbolic_enum<when_exists_symbolic_def>;
using copy_symlinks_enum = symbolic_enum<copy_symlinks_symbolic_def>;
using copy_type_enum = symbolic_enum<copy_type_symbolic_def>;

static void
add_when_exists_to_options(fs::copy_options& opts, when_exists_values we) {
  switch (we) {
  case when_exists_values::error:
    opts |= fs::copy_options::none;
    break;
  case when_exists_values::skip:
    opts |= fs::copy_options::skip_existing;
    break;
  case when_exists_values::overwrite:
    opts |= fs::copy_options::overwrite_existing;
    break;
  case when_exists_values::update:
    opts |= fs::copy_options::update_existing;
    break;
  }
}

static void
add_recursive_to_options(fs::copy_options& opts, bool recursive) {
  if (recursive)
    opts |= fs::copy_options::recursive;
}

static void
add_symlink_options_to_options(fs::copy_options& opts, copy_symlinks_values cs) {
  switch (cs) {
  case copy_symlinks_values::follow:
    break;
  case copy_symlinks_values::copy:
    opts |= fs::copy_options::copy_symlinks;
    break;
  case copy_symlinks_values::skip:
    opts |= fs::copy_options::skip_symlinks;
    break;
  }
}

static void
add_copy_type_to_options(fs::copy_options& opts, copy_type_values type) {
  switch (type) {
  case copy_type_values::copy_content:
    break;
  case copy_type_values::directories_only:
    opts |= fs::copy_options::directories_only;
    break;
  case copy_type_values::create_symlinks:
    opts |= fs::copy_options::create_symlinks;
    break;
  case copy_type_values::create_hard_links:
    opts |= fs::copy_options::create_hard_links;
    break;
  }
}

static bool
copy_regular_file(context& ctx, fs::path const& from, fs::path const& to,
                  when_exists_enum when_exists) {
  fs::copy_options options{};
  add_when_exists_to_options(options, when_exists.value());

  return guard_filesystem_error(ctx, [&] {
    return fs::copy_file(from, to, options);
  });
}

static void
copy_files(context& ctx, fs::path const& from, fs::path const& to,
           bool recursive, when_exists_enum when_exists,
           copy_symlinks_enum copy_symlinks,
           copy_type_enum type) {
  fs::copy_options options{};
  add_recursive_to_options(options, recursive);
  add_when_exists_to_options(options, when_exists.value());
  add_symlink_options_to_options(options, copy_symlinks.value());
  add_copy_type_to_options(options, type.value());

  guard_filesystem_error(ctx, [&] { fs::copy(from, to, options); });
}

static fs::path
read_symlink(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::read_symlink(p); });
}

static void
create_symlink(context& ctx, fs::path const& target, fs::path const& link) {
  guard_filesystem_error(ctx, [&] { fs::create_symlink(target, link); });
}

static void
create_directory_symlink(context& ctx, fs::path const& target,
                         fs::path const& link) {
  guard_filesystem_error(ctx, [&] {
    fs::create_directory_symlink(target, link);
  });
}

static void
create_hard_link(context& ctx, fs::path const& target, fs::path const& link) {
  guard_filesystem_error(ctx, [&] { fs::create_hard_link(target, link); });
}

static void
copy_symlink(context& ctx, fs::path const& from, fs::path const& to) {
  guard_filesystem_error(ctx, [&] { fs::copy_symlink(from, to); });
}

static bool
create_directory(context& ctx,
                 fs::path const& p,
                 sum_type<string, boolean> const& existing) {
  return guard_filesystem_error(
    ctx,
    [&] {
      if (auto e = match<string>(existing))
        return fs::create_directory(p, e->value());
      else
        return fs::create_directory(p);
    }
  );
}

static bool
create_directories(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::create_directories(p); });
}

static bool
files_equivalent(context& ctx, fs::path const& a, fs::path const& b) {
  return guard_filesystem_error(ctx, [&] { return fs::equivalent(a, b); });
}

static std::uintmax_t
file_size(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::file_size(p); });
}

static std::uintmax_t
hard_link_count(context& ctx, fs::path const& p) {
  return guard_filesystem_error(ctx, [&] { return fs::hard_link_count(p); });
}

static double
last_write_time(context& ctx, fs::path const& p) {
  return guard_filesystem_error(
    ctx,
    [&] {
      auto time = fs::last_write_time(p);
      return system_to_scheme(std::chrono::clock_cast<clock>(time));
    }
  );
}

static void
set_last_write_time(context& ctx, fs::path const& p, double time) {
  guard_filesystem_error(
    ctx,
    [&] {
      auto t = std::chrono::clock_cast<std::chrono::file_clock>(
        scheme_to_system(time)
      );
      fs::last_write_time(p, t);
    }
  );
}

static void
rename(context& ctx, fs::path const& from, fs::path const& to) {
  guard_filesystem_error(ctx, [&] { fs::rename(from, to); });
}

static void
resize(context& ctx, fs::path const& p, std::uintmax_t new_size) {
  guard_filesystem_error(ctx, [&] { fs::resize_file(p, new_size); });
}

static ptr<>
space(context& ctx, fs::path const& p) {
  auto s = fs::space(p);
  return make_list(ctx,
                   to_scheme(ctx, s.capacity),
                   to_scheme(ctx, s.free),
                   to_scheme(ctx, s.available));
}

static fs::path
temporary_directory_path(context& ctx) {
  return guard_filesystem_error(ctx, [] { return fs::temp_directory_path(); });
}

static ptr<>
directory_files(context& ctx, fs::path const& p) {
  return make_list_from_range(
    ctx,
    std::ranges::subrange{fs::directory_iterator{p}, fs::directory_iterator{}},
    [&] (fs::directory_entry const& entry) {
      return to_scheme(ctx, entry.path());
    }
  );
}

static ptr<>
directory_files_recursive(context& ctx, fs::path const& p, bool follow_symlinks,
                          bool skip_denied) {
  fs::directory_options opts{};
  if (follow_symlinks)
    opts |= fs::directory_options::follow_directory_symlink;
  if (skip_denied)
    opts |= fs::directory_options::skip_permission_denied;

  return make_list_from_range(
    ctx,
    std::ranges::subrange{fs::recursive_directory_iterator{p, opts},
                          fs::recursive_directory_iterator{}},
    [&] (fs::directory_entry const& entry) {
      return to_scheme(ctx, entry.path());
    }
  );
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
  define_procedure<delete_all>(ctx, "delete-all-files", result);
  define_procedure<current_path>(ctx, "current-path", result);
  define_procedure<set_current_path>(
    ctx, "set-current-path!", result
  );
  define_procedure<status>(ctx, "file-status", result);
  define_procedure<symlink_status>(ctx, "symlink-status", result);
  define_procedure<set_permissions>(ctx, "set-permissions!", result);
  define_procedure<add_permissions>(ctx, "add-permissions!", result);
  define_procedure<remove_permissions>(ctx, "remove-permissions!", result);
  define_procedure<is_block_file>(ctx, "block-file?", result);
  define_procedure<is_character_file>(ctx, "character-file?", result);
  define_procedure<is_directory>(ctx, "directory?", result);
  define_procedure<is_empty>(ctx, "file-empty?", result);
  define_procedure<is_fifo>(ctx, "fifo?", result);
  define_procedure<is_other>(ctx, "file-other?", result);
  define_procedure<is_regular_file>(ctx, "regular-file?", result);
  define_procedure<is_socket>(ctx, "socket?", result);
  define_procedure<is_symlink>(ctx, "symlink?", result);
  define_procedure<copy_regular_file>(ctx, "copy-regular-file", result);
  define_procedure<copy_files>(ctx, "copy-files", result);
  define_procedure<read_symlink>(ctx, "read-symlink", result);
  define_procedure<create_symlink>(ctx, "create-symlink", result);
  define_procedure<create_hard_link>(ctx, "create-hard-link", result);
  define_procedure<create_directory_symlink>(ctx, "create-directory-symlink",
                                             result);
  define_procedure<copy_symlink>(ctx, "copy-symlink", result);
  define_procedure<create_directory>(ctx, "create-directory", result);
  define_procedure<create_directories>(ctx, "create-directories", result);
  define_procedure<files_equivalent>(ctx, "files-equivalent?", result);
  define_procedure<file_size>(ctx, "file-size", result);
  define_procedure<hard_link_count>(ctx, "hard-link-count", result);
  define_procedure<last_write_time>(ctx, "last-write-time", result);
  define_procedure<set_last_write_time>(ctx, "set-last-write-time!", result);
  define_procedure<rename>(ctx, "rename-file", result);
  define_procedure<resize>(ctx, "resize-file", result);
  define_procedure<space>(ctx, "filesystem-space", result);
  define_procedure<temporary_directory_path>(ctx, "temporary-directory-path",
                                             result);
  define_procedure<directory_files>(ctx, "directory-files", result);
  define_procedure<directory_files_recursive>(ctx, "directory-files/recursive",
                                              result);
}

} // namespace insider
