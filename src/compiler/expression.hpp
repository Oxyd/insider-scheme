#ifndef INSIDER_COMPILER_EXPRESSION_HPP
#define INSIDER_COMPILER_EXPRESSION_HPP

#include "util/sum_type.hpp"

namespace insider {

using expression = sum_type<
  class literal_expression,
  class local_reference_expression,
  class top_level_reference_expression,
  class unknown_reference_expression,
  class application_expression,
  class built_in_operation_expression,
  class sequence_expression,
  class let_expression,
  class local_set_expression,
  class top_level_set_expression,
  class lambda_expression,
  class if_expression,
  class loop_body,
  class loop_continue
>;

using tracked_expression = tracked_sum_type<expression>;

} // namespace insider

#endif
