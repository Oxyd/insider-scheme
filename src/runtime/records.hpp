#ifndef INSIDER_RUNTIME_RECORDS_HPP
#define INSIDER_RUNTIME_RECORDS_HPP

#include "memory/member_ptr.hpp"
#include "object.hpp"
#include "type_indexes.hpp"

#include <vector>

namespace insider {

class module_;

class record_type : public leaf_object<record_type> {
public:
  static constexpr char const* scheme_name = "insider::record_type";
  static constexpr word_type static_type_index = type_indexes::record_type;

  explicit
  record_type(std::size_t num_fields) : num_fields_{num_fields} { }

  std::size_t
  num_fields() const { return num_fields_; }

private:
  std::size_t num_fields_;
};

class record_instance
  : public dynamic_size_object<record_instance, mutable_member_ptr<>>
{
public:
  static constexpr char const* scheme_name = "insider::record_instance";
  static constexpr word_type static_type_index = type_indexes::record_instance;

  static std::size_t
  extra_elements(ptr<record_type> type);

  explicit
  record_instance(ptr<record_type> type);

  record_instance(record_instance&&) noexcept;

  ptr<record_type>
  type() const { return type_; }

  void
  set(free_store&, std::size_t field, ptr<> value);

  ptr<>
  ref(std::size_t field);

  void
  visit_members(member_visitor const& f) const;

private:
  ptr<record_type> const type_;
};

ptr<record_instance>
make_instance(context&, ptr<record_type>);

} // namespace insider

#endif
