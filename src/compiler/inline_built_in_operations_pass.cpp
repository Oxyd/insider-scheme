#include "compiler/inline_built_in_operations_pass.hpp"

#include "compiler/ast.hpp"
#include "compiler/variable.hpp"
#include "context.hpp"
#include "vm/bytecode.hpp"

namespace insider {

namespace {
  struct built_in_operations_visitor {
    struct operation {
      std::string name;
      opcode      operation;
      std::size_t arity;
      bool        has_result;
    };

    static operation operations[];

    context& ctx;
    std::unordered_map<ptr<top_level_variable>, operation> operations_map;

    explicit
    built_in_operations_visitor(context& ctx);

    void
    enter(auto) { }

    expression
    leave(ptr<application_expression> app) {
      if (auto ref = match<top_level_reference_expression>(app->target()))
        if (auto op = operations_map.find(ref->variable());
            op != operations_map.end())
          if (app->arguments().size() == op->second.arity)
            return substitute_operation(op->second, app);
      return app;
    }

    expression
    leave(auto e) { return e; }

    expression
    substitute_operation(operation const& op, ptr<application_expression> app) {
      return make<built_in_operation_expression>(
        ctx,
        op.operation,
        app->arguments(),
        op.has_result
      );
    }
  };
}

built_in_operations_visitor::operation
built_in_operations_visitor::operations[]{
  {"+", opcode::add, 2, true},
  {"-", opcode::subtract, 2, true},
  {"*", opcode::multiply, 2, true},
  {"/", opcode::divide, 2, true},
  {"=", opcode::arith_equal, 2, true},
  {"<", opcode::less, 2, true},
  {">", opcode::greater, 2, true},
  {"<=", opcode::less_or_equal, 2, true},
  {">=", opcode::greater_or_equal, 2, true},
  {"box", opcode::box, 1, true},
  {"unbox", opcode::unbox, 1, true},
  {"box-set!", opcode::box_set, 2, false},
  {"cons", opcode::cons, 2, true},
  {"car", opcode::car, 1, true},
  {"cdr", opcode::cdr, 1, true},
  {"vector-set!", opcode::vector_set, 3, false},
  {"vector-ref", opcode::vector_ref, 2, true},
  {"type", opcode::type, 1, true},
  {"eq?", opcode::eq, 2, true}
};

built_in_operations_visitor::built_in_operations_visitor(context& ctx)
  : ctx{ctx}
{
  for (operation const& op : operations) {
    auto binding = ctx.internal_module()->find(ctx.intern(op.name));
    operations_map.emplace(assume<top_level_variable>(binding->variable),
                           op);
  }
}

expression
inline_built_in_operations(context& ctx, expression e, analysis_context) {
  return transform_ast(ctx, e, built_in_operations_visitor{ctx});
}

} // namespace insider