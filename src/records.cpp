#include "records.hpp"

#include "context.hpp"
#include "define_procedure.hpp"

namespace insider {

std::size_t
record_instance::extra_elements(ptr<record_type> type) {
  return type->num_fields();
}

record_instance::record_instance(ptr<record_type> type)
  : dynamic_size_object{type->num_fields()}
  , type_{type}
{ }

record_instance::record_instance(record_instance&& other)
  : dynamic_size_object{other}
  , type_{other.type_}
{
  for (std::size_t i = 0; i < size(); ++i)
    storage_element(i) = other.storage_element(i);
}

void
record_instance::visit_members(member_visitor const& f) {
  f(type_);
  for (std::size_t i = 0; i < size(); ++i)
    f(storage_element(i));
}

void
record_instance::set(std::size_t field, ptr<> value) {
  storage_element(field) = value;
}

ptr<>
record_instance::ref(std::size_t field) {
  return storage_element(field);
}

ptr<record_instance>
make_instance(context& ctx, ptr<record_type> type) {
  return make<record_instance>(ctx, type);
}

void
export_records(context& ctx, module_& result) {
  define_procedure(ctx, "make-record-type", result, true,
                   [] (context& ctx, std::size_t num_fields) {
                     return make<record_type>(ctx, num_fields);
                   });
  define_procedure(ctx, "make-record-instance", result, true, make_instance);
  define_procedure(ctx, "record-set!", result, true, &record_instance::set);
  define_procedure(ctx, "record-ref", result, true, &record_instance::ref);
  define_procedure(ctx, "record-type", result, true, &record_instance::type);
}

} // namespace insider
