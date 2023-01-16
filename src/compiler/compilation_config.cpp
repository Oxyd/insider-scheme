#include "compiler/compilation_config.hpp"

#include "compiler/ast_transforms.hpp"

namespace insider {

compilation_config
compilation_config::optimisations_config() {
  return compilation_config{all_passes};
}

compilation_config
compilation_config::debug_config() {
  return compilation_config{no_optimisations};
}

} // namespace insider
