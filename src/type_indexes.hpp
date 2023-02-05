#ifndef INSIDER_TYPE_INDEXES_HPP
#define INSIDER_TYPE_INDEXES_HPP

namespace insider::type_indexes {

// Plain enum for implicit convertibility to word_type.
enum index {
  null = 1,
  void_,
  eof,
  default_value,
  parameter_tag,
  core_form,
  tail_call_tag,
  boolean,
  pair,
  vector,
  bytevector,
  box,
  weak_box,
  procedure_prototype,
  procedure,
  native_procedure,
  native_continuation,
  values_tuple,
  big_integer,
  fraction,
  floating_point,
  complex,
  record_type,
  record_instance,
  string,
  symbol,
  keyword,
  syntax,
  scope,
  textual_input_port,
  binary_input_port,
  textual_output_port,
  binary_output_port,
};

} // namespace insider::type_indexes

#endif
