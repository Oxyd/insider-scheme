#include "compiler/source_file_origin.hpp"

#include "compiler/source_code_provider.hpp"

namespace insider {

source_file_origin
make_eval_origin() {
  return {&null_source_code_provider_instance, "<eval expression>"};
}

} // namespace insider
