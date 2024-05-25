#include "context.hpp"

#include "compiler/variable.hpp"
#include "memory/free_store.hpp"
#include "runtime/basic_types.hpp"
#include "runtime/internal_module.hpp"
#include "runtime/parameter_map.hpp"
#include "runtime/syntax.hpp"
#include "util/integer_cast.hpp"
#include "util/to_scheme.hpp"

#include <memory>

namespace insider {

//> @module[(insider internal)]

//> @name[if]
//> @syntax{cond-expr then-expr @optional{else-expr}}
//>
//> First @nonterm{test} is evaluated. If it yields any value other than @c{#f},
//> @nonterm{then-expr} is evaluated and the @c{if} expression evaluates to its
//> results. Otherwise, if an @nonterm{else-expr} is present, it is evaluated
//> and the @c{if} expression evaluates to its results. If there is no
//> @nonterm{else-expr} and the condition evaluates to @c{#f}, the @c{if}
//> expression evaluates to @c{#void}.
//>
//> Both @nonterm{then-expr} and @nonterm{else-expr} are in tail position with
//> respect to the @c{if} form.
//>
//> @example{
//>   @code{
//>     (if (> 3 2) 'yes 'no) @evaluates-to{yes}
//>     (if (> 2 3) 'yes 'no) @evaluates-to{no}
//>   }
//> }

//> @name[set!]
//> @syntax{variable expr}
//>
//> @nonterm{Expr} is evaluated and the resulting value is stored in the
//> location to which @nonterm{variable} is bound. The @c{set!} expression
//> itself evaluates to @c{#void}.
//>
//> @example{
//>   @code{
//>     (define x 2)
//>     (+ x 1) @evaluates-to{3}
//>     (set! x 4)
//>     (+ x 1) @evaluates-to{5}
//>   }
//> }

//> @name[lambda]
//> @syntax{formals body}
//>
//> @nonterminal-def[formals]{tail-arg}
//> @nonterminal-def[formals]{
//>   @term{(}
//>   @repeated{mandatory-arg} @repeated{optional-arg}
//>   @optional{@term{.} tail-arg}
//>   @term{)}
//> }
//> @nonterminal-def[mandatory-arg]{@optional{keyword} id}
//> @nonterminal-def[optional-arg]{
//>   @optional{keyword} @term{(}id default-expr@term{)}
//> }
//>
//> A @c{lambda} expression evaluates to a procedure. @nonterm{Formals} defines
//> the arguments the procedure accepts.
//>
//> @example{
//>   @code{
//>     (define f (lambda (a b c) (+ a b c)))
//>     (f 1 2 3) @evaluates-to{6}
//>   }
//> }
//>
//> If a @nonterm{mandatory-arg} or @nonterm{optional-arg} is preceded by a
//> @nonterm{keyword}, the argument is named and can be optionally given by name
//> at the call site rather than by position.
//>
//> When a call site uses named arguments, the argument ordering semantics are
//> as follows: First, all the named call-site arguments are placed in the
//> corresponding named arguments of the procedure. Then the remaining
//> arguments unnamed by the call site are placed in the remaining unfilled
//> procedure arguments in a left-to-right order. It is an error if the call
//> site uses an argument name the procedure doesn't accept.
//>
//> @example{
//>   @code{
//>     (define f
//>       (lambda (a #:second b #:third c)
//>         (list a b c)))
//>
//>     (f 1 2 3)
//>     @evaluates-to{(1 2 3)}
//>
//>     (f 1 #:third 3 #:second 2)
//>     @evaluates-to{(1 2 3)}
//>
//>     (f 1 2 #:third 3)
//>     @evaluates-to{(1 2 3)}
//>
//>     (f 1 3 #:second 2)
//>     @evaluates-to{(1 2 3)}
//>   }
//>   In the last example, first the @c{#:second 2} argument is placed in the
//>   @c{b} slot of the procedure. Then the remaining arguments, @c{1} and
//>   @c{3}, are taken, in this order, and used to fill the remaining argument
//>   slots of the procedure, with @c{1} filling the @c{a} slot, and @c{3}
//>   filling the @c{c} slot since the @c{b} slot is already filled.
//> }
//>
//> If an argument includes a @nonterm{default-expr}, the argument is optional.
//> If a call site does not provide a value for an optional argument, the
//> @nonterm{default-expr} is evaluated in the context of the called procedure
//> and its value is used as the value for the optional argument.
//>
//> @example{
//>   @code{
//>     (define f
//>       (lambda (a (b 2) (c 3))
//>         (list a b c)))
//>
//>     (f 1 2)
//>     @evaluates-to{(1 2 3)}
//>
//>     (f 1)
//>     @evaluates-to{(1 2 3)}
//>
//>     (f 1 2 'three)
//>     @evaluates-to{(1 2 three)}
//>   }
//> }
//>
//> If the @nonterm{formals} include a @nonterm{tail-arg}, the procedure is
//> variadic and can accept an unbound number of arguments. After all
//> @nonterm{mandatory-arg}s and @nonterm{optional-arg}s have been filled, the
//> remaining call-site arguments are collected into a list and the
//> @nonterm{tail-arg} is bound to this list within the procedure.
//>
//> @example{
//>   @code{
//>     (define f
//>       (lambda (a (b 'default) . rest)
//>         (list a b rest)))
//>
//>     (f 1 2)
//>     @evaluates-to{(1 2 ())}
//>
//>     (f 1 2 3 4)
//>     @evaluates-to{(1 2 (3 4))}
//>
//>     (f 1)
//>     @evaluates-to{(1 default ())}
//>   }
//> }

//> @name[define-syntax]
//> @syntax{identifier transformer-expr}
//>
//> @nonterm{transformer-expr} has to be an expression that evaluates to a valid
//> transformer. A transformer is a procedure that takes one argument, the input
//> syntax object, and returns another syntax object.
//>
//> Whenever @nonterm{identifier} is encountered as the first term of an
//> S-expression, and the @nonterm{identifier} is bound to a syntax transformer
//> in that context, the transformer procedure is called at compile-time with
//> the whole S-expression as its input, and the expression is effectively
//> replaced with the result of the transformer procedure.
//>
//> It is an error if @nonterm{transformer-expr} does not evaluate to a
//> procedure of a single argument, or if, when called, the transformer
//> procedure does not return a syntax object.
//>
//> @example{
//>   @code{
//>     (define-syntax with
//>       (lambda (stx)
//>         (let ((expr (syntax->list stx)))
//>           (let ((name (cadr expr))
//>                 (value (caddr expr))
//>                 (body (cddr expr)))
//>             #`(let ((#,name #,value)) #,@"@"body)))))
//>
//>     (with a 2 (+ 5 a))
//>     @evaluates-to{7}
//>   }
//> }

//> @name[begin]
//> @syntax{begin-contents}
//> @nonterminal-def[begin-contents]{@repeated{expression-or-definition}}
//> @nonterminal-def[begin-contents]{expr@_{1} @repeated{expr@_{2}}}
//>
//> There are two forms of @c{begin}. The first form, where
//> @nonterm{begin-contents} can contain expressions and definitions, can appear
//> as a part of a body, or of the top-level, or nested in another @c{begin}
//> that itself is of this form. The @nonterm{expression-or-definition}s are
//> spliced into the surrounding context as if the @c{begin} form were not
//> present.
//>
//> This form is primarily used in the output of macros that need to expand to
//> multiple definitions and splice them into the enclosing context.
//>
//> @example{
//>   @code{
//>     (define-syntax make-vars
//>       (syntax-rules ()
//>         ((make-vars names ...)
//>          (begin
//>            (define names 0) ...))))
//>
//>     (make-vars x y z)
//>     (set! x 2)
//>     (set! y 5)
//>     (+ x y z) @evaluates-to{7}
//>   }
//> }
//>
//> The second form, where @nonterm{begin-contents} is a sequence of
//> expressions, is itself an expression. When evaluated, its @nonterm{expr}s
//> are evaluated in left-to-right order, and the @c{begin} expression evaluates
//> to the result of the last expression.
//>
//> @example{
//>   @code{
//>     (define x 0)
//>     (and (= x 0)
//>          (begin (set! x 5)
//>                 (+ x 1))) @evaluates-to{6}
//>   }
//> }

//> @name[quote]
//> @syntax{datum}
//>
//> Evaluates to @nonterm{datum}.
//>
//> Most data are self-evaluating, and for those @c{(quote datum)} is the same
//> as @c{datum} itself. Other data, however, have special meaning in normal
//> Scheme code – most notably, lists are used to represent procedure calls.
//> For these, @c{quote} removes this special meaning and allows a datum to
//> stand for itself.
//>
//> Note that the reader will read @c{'foo} as the list @c{(quote foo)}.
//>
//> @example{
//>   @code{
//>     (define a 5)
//>     (quote a) @evaluates-to{a}
//>     'a        @evaluates-to{a}
//>     a         @evaluates-to{5}
//>
//>     (define (f x) (* 2 x))
//>     (quote (f 10)) @evaluates-to{(f 10)}
//>     '(f 10)        @evaluates-to{(f 10)}
//>     (f 10)         @evaluates-to{20}
//>
//>     5         @evaluates-to{5}
//>     (quote 5) @evaluates-to{5}
//>     '5        @evaluates-to{5}
//>   }
//> }

//> @in-group[quasiquote]
//> @name[quasiquote]
//> @syntax{qq-template}
//>
//> Similar to @ref[(insider syntax) quote]{@c{quote}}, but optionally allows
//> for parts of the datum to be evaluated, using the @c{unquote} and
//> @c{unquote-splicing} forms.
//>
//> @c{quasiquote} can be abbreviated as @c{`} (backtick), @c{unquote} as @c{,}
//> (comma), and @c{unquote-splicing} as @c{,@"@"} (comma followed by at sign).
//>
//> If @c{(unquote expr)} appears in a @nonterm{qq-template}, the expression
//> is evaluated and its result inserted into the resulting datum instead of the
//> @c{unquote} form.
//>
//> @example{
//>   @code{
//>     `(list ,(+ 1 2) 4)
//>     @evaluates-to{(list 3 4)}
//>     (let ((name 'a)) `(list ,name ',name))
//>     @evaluates-to{(list a (quote a))}
//>   }
//> }
//>
//> If @c{(unquote-splicing expr)} appears in a @nonterm{qq-template}, the
//> expression is evaluated and has to produce a list. The resulting list is
//> then spliced into the output datum.
//>
//> @example{
//>   @code{
//>     `(a ,(+ 1 2) ,@"@"(map abs '(4 -5 6)) b)
//>     @evaluates-to{(a 3 4 5 6 b)}
//>   }
//> }
//>
//> Quasiquote expressions can be nested. Each @c{quasiquote} increases the
//> nesting level, and each @c{unquote} or @c{unquote-splicing} decreases it.
//> @c{unquote} and @c{unquote-splincing} substitutions are only made at the
//> level of the outermost @c{quasiquote}.
//>
//> @example{
//>   @code{
//>     `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
//>     @evaluates-to{(a `(b ,(+ 1 2) ,(foo 4 d) e) f)}
//>   }
//> }

//> @in-group[quasiquote]
//> @name[unquote]
//> @auxiliary-syntax

//> @in-group[quasiquote]
//> @name[unquote-splicing]
//> @auxiliary-syntax

context::context() {
  constants = std::make_unique<struct constants>();
  constants->null = make<null_type>(*this);
  constants->void_ = make<void_type>(*this);
  constants->t = make<boolean>(*this, true);
  constants->f = make<boolean>(*this, false);
  constants->eof = make<eof_type>(*this);
  constants->default_value = make<default_value_type>(*this);
  constants->tail_call_tag = make<tail_call_tag_type>(*this);
  constants->integer_type_symbol = intern(integer_type_name);
  constants->character_type_symbol = intern(character_type_name);
  constants->string_cursor_type_symbol = intern(string_cursor_type_name);

  module_resolver().set_internal_module(make_internal_module(*this));

  struct {
    ptr<core_form_type>& object;
    std::string          name;
  } core_forms[]{
    {constants->let,               "let"},
    {constants->set,               "set!"},
    {constants->lambda,            "lambda"},
    {constants->if_,               "if"},
    {constants->define,            "define"},
    {constants->define_syntax,     "define-syntax"},
    {constants->begin,             "begin"},
    {constants->quote,             "quote"},
    {constants->quasiquote,        "quasiquote"},
    {constants->unquote,           "unquote"},
    {constants->unquote_splicing,  "unquote-splicing"},
    {constants->syntax,            "syntax"},
    {constants->quasisyntax,       "quasisyntax"},
    {constants->unsyntax,          "unsyntax"},
    {constants->unsyntax_splicing, "unsyntax-splicing"},
    {constants->syntax_trap,       "syntax-trap"},
    {constants->syntax_error,      "syntax-error"},
    {constants->let_syntax,        "let-syntax"},
    {constants->letrec_syntax,     "letrec-syntax"},
    {constants->meta,              "meta"}
  };
  for (auto const& form : core_forms) {
    form.object = make<core_form_type>(*this, form.name);
    auto index = add_top_level(form.object, form.name);
    auto name = intern(form.name);
    auto id = make<syntax>(*this, name, scope_set{internal_module()->scope()});
    auto var = make<top_level_variable>(*this, form.name, index);
    internal_module()->scope()->add(store, id, var);
    internal_module()->export_(name);
  }

  constants->init = make<core_form_type>(*this, "init");
  features_ = make_list(*this, intern("r7rs"), intern("full-unicode"));

  set_command_line({});
}

context::~context() {
  constants.reset();
}

template <typename T>
static ptr<T>
intern(context& ctx, std::string const& s,
       std::unordered_map<std::string, ptr<weak_box>>& map) {
  auto interned = map.find(s);
  if (interned != map.end()) {
    if (interned->second->get())
      return assume<T>(interned->second->get());
    else {
      ptr<T> result = make<T>(ctx, s);
      interned->second = make<weak_box>(ctx, result);
      return result;
    }
  }

  ptr<T> result = make<T>(ctx, s);
  map.emplace(s, make<weak_box>(ctx, result));

  return result;
}

ptr<symbol>
context::intern(std::string const& s) {
  return insider::intern<symbol>(*this, s, interned_symbols_);
}

ptr<keyword>
context::intern_keyword(std::string const& s) {
  return insider::intern<keyword>(*this, s, interned_keywords_);
}

ptr<>
context::get_top_level_checked(operand i) const {
  if (static_cast<std::size_t>(i) >= top_level_objects_.size())
    throw std::runtime_error{fmt::format("Nonexistent top-level object {}", i)};

  return top_level_objects_[i].value;
}

void
context::set_top_level(operand i, ptr<> value) {
  assert(i >= 0);
  assert(static_cast<std::size_t>(i) < top_level_objects_.size());
  if (!top_level_objects_[i].mutable_)
    throw std::runtime_error{
      fmt::format("Attempting to mutate immutable top-level {}",
                  top_level_objects_[i].name)
    };
  top_level_objects_[i].value = value;
}

operand
context::add_top_level(ptr<> x, std::string name) {
  top_level_objects_.emplace_back(top_level_binding{x, std::move(name), false});
  return static_cast<operand>(top_level_objects_.size() - 1);
}

operand
context::add_top_level_mutable(ptr<> x, std::string name) {
  top_level_objects_.emplace_back(top_level_binding{x, std::move(name), true});
  return static_cast<operand>(top_level_objects_.size() - 1);
}

std::string
context::get_top_level_name(operand i) const {
  if (static_cast<std::size_t>(i) < top_level_objects_.size())
    return top_level_objects_[i].name;
  else
    throw make_error("Invalid global operand {}", i);
}

void
context::add_feature(std::string const& f) {
  auto f_sym = intern(f);
  if (!memq(f_sym, features_))
    features_ = cons(*this, f_sym, features_);
}

root_ptr<module_>
context::internal_module_tracked() {
  return {store.root_list(), internal_module()};
}

scope::id_type
context::generate_scope_id() {
  return next_scope_id_++;
}

vm_id_type
context::generate_vm_id() {
  return next_vm_id_++;
}

void
context::set_command_line(std::vector<std::string> const& cmd) {
  command_line_ = make_list_from_range(*this, cmd);
}

void
context::root_provider::visit_roots(member_visitor const& f) {
  f(ctx_.constants->null);
  f(ctx_.constants->void_);
  f(ctx_.constants->t);
  f(ctx_.constants->f);
  f(ctx_.constants->eof);
  f(ctx_.constants->default_value);
  f(ctx_.constants->tail_call_tag);
  f(ctx_.constants->integer_type_symbol);
  f(ctx_.constants->character_type_symbol);
  f(ctx_.constants->string_cursor_type_symbol);
  f(ctx_.constants->let);
  f(ctx_.constants->set);
  f(ctx_.constants->init);
  f(ctx_.constants->lambda);
  f(ctx_.constants->if_);
  f(ctx_.constants->define);
  f(ctx_.constants->define_syntax);
  f(ctx_.constants->begin);
  f(ctx_.constants->quote);
  f(ctx_.constants->quasiquote);
  f(ctx_.constants->unquote);
  f(ctx_.constants->unquote_splicing);
  f(ctx_.constants->syntax);
  f(ctx_.constants->quasisyntax);
  f(ctx_.constants->unsyntax);
  f(ctx_.constants->unsyntax_splicing);
  f(ctx_.constants->syntax_trap);
  f(ctx_.constants->syntax_error);
  f(ctx_.constants->let_syntax);
  f(ctx_.constants->letrec_syntax);
  f(ctx_.constants->meta);
  f(ctx_.constants->current_input_port_tag);
  f(ctx_.constants->current_output_port_tag);
  f(ctx_.constants->current_error_port_tag);
  f(ctx_.constants->current_source_file_origin_tag);
  f(ctx_.constants->is_main_module_tag);
  f(ctx_.constants->current_expand_module_tag);
  f(ctx_.constants->interaction_environment_specifier_tag);

  ctx_.parameters.visit_members(f);

  for (auto& [name, symbol] : ctx_.interned_symbols_)
    f(symbol);

  for (auto& [name, keyword] : ctx_.interned_keywords_)
    f(keyword);

  for (ptr<symbol>& name : ctx_.type_name_symbols_)
    f(name);

  for (top_level_binding& x : ctx_.top_level_objects_)
    f(x.value);

  f(ctx_.features_);
  f(ctx_.command_line_);
}

} // namespace insider
