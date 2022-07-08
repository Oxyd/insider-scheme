#include "analyser.hpp"

#include "compiler/ast_transforms.hpp"
#include "compiler/expression.hpp"
#include "compiler/module_specifier.hpp"
#include "compiler/parser_expander.hpp"
#include "compiler/source_code_provider.hpp"
#include "compiler/syntax_list.hpp"
#include "io/read.hpp"
#include "memory/tracked_ptr.hpp"
#include "runtime/integer.hpp"
#include "runtime/string.hpp"
#include "runtime/symbol.hpp"
#include "runtime/syntax.hpp"
#include "util/list_iterator.hpp"
#include "vm/vm.hpp"

#include <fmt/format.h>

#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace insider {

static expression
analyse_internal(parsing_context& pc, ptr<syntax> stx) {
  expression result = parse(pc, stx);
  result = box_set_variables(pc.ctx, result);
  analyse_free_variables(pc.ctx, result);
  return result;
}

static expression
analyse_expression_list(parsing_context& pc,
                        std::vector<tracked_ptr<syntax>>& exprs) {
  std::vector<tracked_expression> result_exprs;
  result_exprs.reserve(exprs.size());
  for (auto const& datum : exprs)
    result_exprs.emplace_back(pc.ctx.store, analyse_internal(pc, datum.get()));
  return make<sequence_expression>(pc.ctx, untrack_expressions(result_exprs));
}

static expression
analyse_top_level_expressions(parsing_context& pc,
                              tracked_ptr<module_> const& m,
                              std::vector<ptr<syntax>> const& exprs) {
  std::vector<tracked_ptr<syntax>> body = expand_top_level(pc, m, exprs);
  if (body.size() == 1)
    return analyse_internal(pc, body.front().get());
  else
    return analyse_expression_list(pc, body);
}

expression
analyse_transformer(parsing_context& pc, ptr<syntax> stx) {
  return analyse_internal(pc, stx);
}

expression
analyse_meta(parsing_context& pc, ptr<syntax> stx) {
  return analyse_top_level_expressions(pc, track(pc.ctx, pc.module_), {stx});
}

expression
analyse(context& ctx, ptr<syntax> stx, tracked_ptr<module_> const& m,
        source_file_origin const& origin) {
  parameterize origin_param{ctx, ctx.constants->current_source_file_origin_tag,
                            make<opaque_value<source_file_origin>>(ctx, origin)};
  parameterize module_param{ctx, ctx.constants->current_expand_module_tag,
                            m.get()};
  parsing_context pc{ctx, m.get(), origin};
  stx = stx->add_scope(ctx.store, m->scope());
  return analyse_top_level_expressions(pc, m, {stx});
}

static bool
is_directive(ptr<syntax> datum, std::string const& directive) {
  if (syntax_is<pair>(datum)) {
    auto p = syntax_assume_without_update<pair>(datum);
    auto car_stx = expect<syntax>(car(p));
    if (syntax_is<symbol>(car_stx))
      return syntax_assume_without_update<symbol>(car_stx)->value() == directive;
  }
  return false;
}

module_name
parse_module_name(context& ctx, ptr<syntax> stx) {
  module_name result;

  ptr<> datum = syntax_to_list(ctx, stx);
  if (!datum)
    throw make_compile_error<syntax_error>(stx, "Invalid module name");

  for (ptr<> elem : list_range{datum}) {
    ptr<syntax> e = expect<syntax>(elem);
    if (auto s = syntax_match_without_update<symbol>(e))
      result.push_back(s->value());
    else if (auto i = syntax_match_without_update<integer>(e))
      result.push_back(std::to_string(i->value()));
    else
      throw make_compile_error<syntax_error>(e, "Invalid module name");
  }

  return result;
}

import_specifier
parse_only_import_specifier(context& ctx, ptr<pair> spec) {
  import_specifier::only result;
  result.from = std::make_unique<import_specifier>(
    parse_import_specifier(ctx, expect<syntax>(cadr(spec)))
  );

  for (ptr<> identifier : list_range{cddr(spec)})
    result.identifiers.push_back(
      syntax_expect_without_update<symbol>(
        expect<syntax>(identifier)
      )->value()
    );

  return import_specifier{std::move(result)};
}

import_specifier
parse_except_import_specifier(context& ctx, ptr<pair> spec) {
  import_specifier::except result;
  result.from = std::make_unique<import_specifier>(
    parse_import_specifier(ctx, expect<syntax>(cadr(spec)))
  );

  for (ptr<> identifier : list_range{cddr(spec)})
    result.identifiers.push_back(
      syntax_expect_without_update<symbol>(
        expect<syntax>(identifier)
      )->value()
    );

  return import_specifier{std::move(result)};
}

import_specifier
parse_prefix_import_specifier(context& ctx, ptr<pair> spec) {
  import_specifier::prefix result;
  result.from = std::make_unique<import_specifier>(
    parse_import_specifier(ctx, expect<syntax>(cadr(spec)))
  );
  result.prefix_ = syntax_expect_without_update<symbol>(
    expect<syntax>(caddr(spec))
  )->value();

  return import_specifier{std::move(result)};
}

import_specifier
parse_rename_import_specifier(context& ctx, ptr<pair> spec) {
  import_specifier::rename result;
  result.from = std::make_unique<import_specifier>(
    parse_import_specifier(ctx, expect<syntax>(cadr(spec)))
  );

  for (ptr<> name_pair_stx : list_range{cddr(spec)}) {
    ptr<> name_pair = syntax_to_list(ctx, expect<syntax>(name_pair_stx));
    if (list_length(name_pair) != 2)
      throw make_compile_error<syntax_error>(
        expect<syntax>(name_pair_stx),
        "import: rename: Expected a list of length 2"
      );

    auto np = assume<pair>(name_pair);

    result.renames.emplace_back(
      syntax_expect_without_update<symbol>(expect<syntax>(car(np)))->value(),
      syntax_expect_without_update<symbol>(expect<syntax>(cadr(np)))->value()
    );
  }

  return import_specifier{std::move(result)};
}

import_specifier
parse_import_specifier(context& ctx, ptr<syntax> stx) {
  ptr<> spec = syntax_to_list(ctx, stx);
  if (!spec)
    throw make_compile_error<syntax_error>(stx,
                                           "import: Expected a non-empty list");

  auto p = assume<pair>(spec);

  if (auto head = syntax_match_without_update<symbol>(expect<syntax>(car(p)))) {
    if (head->value() == "only")
      return parse_only_import_specifier(ctx, p);
    else if (head->value() == "except")
      return parse_except_import_specifier(ctx, p);
    else if (head->value() == "prefix")
      return parse_prefix_import_specifier(ctx, p);
    else if (head->value() == "rename")
      return parse_rename_import_specifier(ctx, p);
    else
      return import_specifier{parse_module_name(ctx, stx)};
  }
  else
    return import_specifier{parse_module_name(ctx, stx)};

  assert(!"Unreachable");
  throw std::logic_error{"Unreachable"};
}

static void
process_library_import(context& ctx, module_specifier& result, ptr<syntax> stx) {
  for (ptr<> set : list_range{syntax_to_list(ctx, syntax_cdr(ctx, stx))})
    result.imports.push_back(parse_import_specifier(ctx, assume<syntax>(set)));
}

static export_specifier
parse_all_imported_from_export_specifier(context& ctx,
                                         source_location const& loc,
                                         ptr<pair> spec) {
  if (list_length(spec) == 2)
    return export_specifier{export_specifier::all_imported_from{
      parse_module_name(ctx, expect<syntax>(cadr(spec)))
    }};
  else
    throw make_compile_error<syntax_error>(loc,
                                           "Invalid all-imported-from syntax");
}

static export_specifier
parse_export_specifier(context& ctx, ptr<syntax> stx) {
  if (auto head = syntax_match_without_update<symbol>(stx)) {
    return export_specifier{export_specifier::name{
      syntax_expect_without_update<symbol>(stx)->value()
    }};
  } else {
    if (ptr<> spec = syntax_to_list(ctx, stx); spec && is<pair>(spec)) {
      auto spec_lst = assume<pair>(spec);
      auto head_stx = expect<syntax>(car(spec_lst));
      if (auto head = syntax_match_without_update<symbol>(head_stx)) {
        if (head->value() == "all-imported-from")
          return parse_all_imported_from_export_specifier(ctx, stx->location(),
                                                          spec_lst);
      }
    }

    throw make_compile_error<syntax_error>(stx, "Invalid export specifier");
  }
}

static void
process_library_export(context& ctx, module_specifier& result,
                       ptr<syntax> stx) {
  for (ptr<> name : list_range{syntax_to_list(ctx, syntax_cdr(ctx, stx))})
    result.exports.push_back(parse_export_specifier(ctx, assume<syntax>(name)));
}

static module_specifier
read_plain_module(context& ctx, std::vector<ptr<syntax>> const& contents,
                  source_file_origin origin) {
  module_specifier result{ctx.store, std::move(origin)};
  auto current = contents.begin();

  while (current != contents.end())
    if (is_directive(*current, "library")) {
      result.name = parse_module_name(ctx, syntax_cadr(ctx, *current));
      ++current;
    } else if (is_directive(*current, "import")) {
      process_library_import(ctx, result, *current);
      ++current;
    } else if (is_directive(*current, "export")) {
      process_library_export(ctx, result, *current);
      ++current;
    } else
      break;

  result.body.reserve(contents.end() - current);
  for (; current != contents.end(); ++current)
    result.body.push_back(*current);

  return result;
}

static void
process_library_body(context& ctx, module_specifier& result, ptr<syntax> stx) {
  auto body = syntax_to_list(ctx, stx);
  result.body.reserve(result.body.size() + list_length(body) - 1);

  if (body != ctx.constants->null)
    body = cdr(assume<pair>(body));

  while (body != ctx.constants->null) {
    result.body.push_back(expect<syntax>(car(assume<pair>(body))));
    body = cdr(assume<pair>(body));
  }
}

template <auto Reader>
static void
perform_library_include(context& ctx, module_specifier& result,
                        source_file_origin const& origin,
                        source_location const& loc, std::string const& name) {
  if (auto source = find_source_relative(ctx, origin, name)) {
    auto body = Reader(ctx, source->port.get().get());
    result.body.reserve(result.body.size() + body.size());
    std::ranges::copy(body, std::back_inserter(result.body));
  } else
    throw make_compile_error<syntax_error>(
      loc, fmt::format("File {} not found", name)
    );
}

template <auto Reader>
static void
process_library_include(context& ctx, module_specifier& result,
                        source_file_origin const& origin, ptr<syntax> stx) {
  ptr<> const& filenames = cdr(expect<pair>(syntax_to_datum(ctx, stx)));
  for (ptr<> filename : list_range{filenames})
    perform_library_include<Reader>(ctx, result, origin, stx->location(),
                                    expect<string>(filename)->value());
}

static void
process_library_declaration(context& ctx, module_specifier& result,
                            source_file_origin const& origin, ptr<syntax> form);

static void
process_include_library_declarations(context& ctx, module_specifier& result,
                                     source_file_origin const& origin,
                                     ptr<syntax> stx) {
  auto l = cdr(expect<pair>(syntax_to_datum(ctx, stx)));
  for (ptr<> filename_obj : list_range{l}) {
    std::string filename = expect<string>(filename_obj)->value();
    if (auto source = find_source_relative(ctx, origin, filename)) {
      auto contents = read_syntax_multiple(ctx, source->port.get().get());
      for (ptr<syntax> const& s : contents)
        process_library_declaration(ctx, result, source->origin, s);
    }
    else
      throw make_compile_error<syntax_error>(
        stx, fmt::format("File {} not found", filename)
      );
  }
}

static bool
eval_cond_expand_condition(context& ctx, ptr<syntax> condition);

static bool
eval_cond_expand_disjunction(context& ctx, ptr<> elements) {
  for (ptr<> cond : list_range{syntax_to_list(ctx, elements)})
    if (eval_cond_expand_condition(ctx, expect<syntax>(cond)))
      return true;
  return false;
}

static bool
eval_cond_expand_conjunction(context& ctx, ptr<> elements) {
  for (ptr<> cond : list_range{syntax_to_list(ctx, elements)})
    if (!eval_cond_expand_condition(ctx, expect<syntax>(cond)))
      return false;
  return true;
}

static bool
eval_cond_expand_condition(context& ctx, ptr<syntax> condition) {
  if (auto feature = syntax_match_without_update<symbol>(condition)) {
    if (feature == ctx.intern("else"))
      return true;
    else
      return memq(feature, ctx.features());
  } else if (is_directive(condition, "library"))
    return ctx.module_resolver().knows_module(
      ctx, parse_module_name(ctx, syntax_cadr(ctx, condition))
    );
  else if (is_directive(condition, "not"))
    return !eval_cond_expand_condition(ctx, syntax_cadr(ctx, condition));
  else if (is_directive(condition, "or"))
    return eval_cond_expand_disjunction(ctx, syntax_cdr(ctx, condition));
  else if (is_directive(condition, "and"))
    return eval_cond_expand_conjunction(ctx, syntax_cdr(ctx, condition));
  else
    throw make_compile_error<syntax_error>(condition,
                                           "Invalid cond-expand condition");
}

static void
process_library_declarations(context& ctx, module_specifier& result,
                             source_file_origin const& origin,
                             ptr<> list) {
  for (ptr<> decl : list_range{syntax_to_list(ctx, list)})
    process_library_declaration(ctx, result, origin, expect<syntax>(decl));
}

static bool
process_cond_expand_clause(context& ctx, module_specifier& result,
                           source_file_origin const& origin, ptr<syntax> stx) {
  auto condition = syntax_car(ctx, stx);
  auto body = syntax_cdr(ctx, stx);

  if (eval_cond_expand_condition(ctx, condition)) {
    process_library_declarations(ctx, result, origin, body);
    return true;
  } else
    return false;
}

static void
process_cond_expand(context& ctx, module_specifier& result,
                    source_file_origin const& origin, ptr<syntax> stx) {
  for (ptr<> clause : list_range{cdr(expect<pair>(syntax_to_list(ctx, stx)))}) {
    bool taken = process_cond_expand_clause(
      ctx, result, origin, expect<syntax>(clause)
    );
    if (taken)
      break;
  }
}

static void
process_library_declaration(context& ctx, module_specifier& result,
                            source_file_origin const& origin, ptr<syntax> form) {
  static constexpr std::vector<ptr<syntax>>
    (*read_syntax_multiple)(context&, ptr<textual_input_port>)
    = &insider::read_syntax_multiple;

  if (is_directive(form, "import"))
    process_library_import(ctx, result, form);
  else if (is_directive(form, "export"))
    process_library_export(ctx, result, form);
  else if (is_directive(form, "begin"))
    process_library_body(ctx, result, form);
  else if (is_directive(form, "include"))
    process_library_include<read_syntax_multiple>(ctx, result, origin, form);
  else if (is_directive(form, "include-ci"))
    process_library_include<read_syntax_multiple_ci>(ctx, result, origin, form);
  else if (is_directive(form, "include-library-declarations"))
    process_include_library_declarations(ctx, result, origin, form);
  else if (is_directive(form, "cond-expand"))
    process_cond_expand(ctx, result, origin, form);
  else
    throw make_compile_error<syntax_error>(form, "Invalid library declaration");
}

static module_specifier
read_define_library(context& ctx, ptr<syntax> form,
                    source_file_origin const& origin) {
  module_specifier result{ctx.store, origin};

  assert(expect<syntax>(syntax_car(ctx, form))->get_symbol()->value()
         == "define-library");
  if (syntax_cdr(ctx, form) == ctx.constants->null)
    throw make_compile_error<syntax_error>(form,
                                           "Invalid define-library syntax");

  result.name = parse_module_name(ctx, syntax_cadr(ctx, form));

  for (ptr<> decl : list_range{cddr(expect<pair>(syntax_to_list(ctx, form)))})
    process_library_declaration(ctx, result, origin, expect<syntax>(decl));

  return result;
}

module_specifier
read_module(context& ctx, std::vector<ptr<syntax>> const& contents,
            source_file_origin const& origin) {
  if (contents.empty())
    throw std::runtime_error("Empty module body");

  if (is_directive(contents.front(), "define-library"))
    return read_define_library(ctx, contents.front(), origin);
  else
    return read_plain_module(ctx, contents, origin);
}

std::optional<module_name>
read_library_name(context& ctx, ptr<textual_input_port> in) {
  try {
    if (auto first_datum = match<syntax>(read_syntax(ctx, in))) {
      if (is_directive(first_datum, "library")
          || is_directive(first_datum, "define-library"))
        return parse_module_name(ctx, syntax_cadr(ctx, first_datum));
      else
        return {};
    } else
      return {};
  }
  catch (read_error const&) {
    // The file probably isn't a library at all. That is not an error.
    return {};
  }
}

expression
analyse_module(context& ctx, tracked_ptr<module_> const& m,
               module_specifier const& pm, bool main_module) {
  parameterize origin_param{
    ctx, ctx.constants->current_source_file_origin_tag,
    make<opaque_value<source_file_origin>>(ctx, pm.origin)
  };
  parameterize main_module_param{
    ctx, ctx.constants->is_main_module_tag,
    main_module ? ctx.constants->t : ctx.constants->f
  };
  parameterize module_param{
    ctx, ctx.constants->current_expand_module_tag, m.get()
  };
  parsing_context pc{ctx, m.get(), pm.origin};

  return analyse_top_level_expressions(pc, m, pm.body);
}

void
export_analyser(context& ctx, ptr<module_> result) {
  ctx.constants->current_source_file_origin_tag
    = create_parameter_tag(ctx, ctx.constants->f);
  ctx.constants->is_main_module_tag
    = create_parameter_tag(ctx, ctx.constants->f);
  ctx.constants->current_expand_module_tag
    = create_parameter_tag(ctx, ctx.constants->f);

  define_top_level(ctx, "current-source-file-origin-tag", result, true,
                   ctx.constants->current_source_file_origin_tag);
  define_top_level(ctx, "main-module?-tag", result, true,
                   ctx.constants->is_main_module_tag);
  define_top_level(ctx, "current-expand-module-tag", result, true,
                   ctx.constants->current_expand_module_tag);
}

} // namespace insider
