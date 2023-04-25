#include "records.hpp"

#include "context.hpp"
#include "runtime/symbol.hpp"
#include "util/define_procedure.hpp"

namespace insider {

std::size_t
record_instance::extra_elements(ptr<record_type> type) {
  return type->num_fields();
}

record_instance::record_instance(ptr<record_type> type)
  : dynamic_size_object{type->num_fields()}
  , type_{type}
{
  for (std::size_t i = 0; i < size(); ++i)
    storage_element(i) = member_ptr<>{};
}

record_instance::record_instance(record_instance&& other) noexcept
  : dynamic_size_object{other}
  , type_{other.type_}
{
  for (std::size_t i = 0; i < size(); ++i)
    storage_element(i) = other.storage_element(i);
}

void
record_instance::visit_members(member_visitor const& f) const {
  f(type_);
  for (std::size_t i = 0; i < size(); ++i)
    storage_element(i).visit_members(f);
}

void
record_instance::set(free_store& store, std::size_t field, ptr<> value) {
  storage_element(field).assign(store, this, value);
}

ptr<>
record_instance::ref(std::size_t field) {
  return storage_element(field).get();
}

ptr<record_instance>
make_instance(context& ctx, ptr<record_type> type) {
  return make<record_instance>(ctx, type);
}

static ptr<record_type>
make_record_type(context& ctx, std::size_t num_fields, ptr<symbol> name) {
  return make<record_type>(ctx, num_fields, name);
}

static void
record_instance_set(context& ctx, ptr<record_instance> inst, std::size_t field,
                    ptr<> value) {
  inst->set(ctx.store, field, value);
}

void
export_records(context& ctx, ptr<module_> result) {
  define_procedure<make_record_type>(ctx, "make-record-type", result);
  define_procedure<make_instance>(ctx, "make-record-instance", result);
  define_procedure<record_instance_set>(ctx, "record-set!", result);
  define_procedure<&record_instance::ref>(ctx, "record-ref", result);
  define_constant_evaluable_procedure<&record_instance::type>(ctx, "record-type",
                                                              result);
}

} // namespace insider
