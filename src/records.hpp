#ifndef INSIDER_RECORDS_HPP
#define INSIDER_RECORDS_HPP

#include "object.hpp"

#include <vector>

namespace insider {

class module;

class record_type : public leaf_object<record_type> {
public:
  static constexpr char const* scheme_name = "insider::record_type";

  explicit
  record_type(std::size_t num_fields) : num_fields_{num_fields} { }

  std::size_t
  num_fields() const { return num_fields_; }

  std::size_t
  hash() const { return 0; }

private:
  std::size_t num_fields_;
};

class record_instance : public dynamic_size_object<record_instance, ptr<>> {
public:
  static constexpr char const* scheme_name = "insider::record_instance";

  static std::size_t
  extra_elements(ptr<record_type> type);

  explicit
  record_instance(ptr<record_type> type) : type_{type} { }

  ptr<record_type>
  type() const { return type_; }

  void
  set(std::size_t field, ptr<> value);

  ptr<>
  ref(std::size_t field);

  void
  visit_members(member_visitor const& f);

  std::size_t
  size() const;

  std::size_t
  hash() const { return type_->hash(); }

private:
  ptr<record_type> type_;
};

ptr<record_instance>
make_instance(context&, ptr<record_type>);

void
export_records(context&, module&);

} // namespace insider

#endif
