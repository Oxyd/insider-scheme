#ifndef INSIDER_COMPILER_SOURCE_FILE_ORIGIN_HPP
#define INSIDER_COMPILER_SOURCE_FILE_ORIGIN_HPP

#include <filesystem>

namespace insider {

class source_code_provider;

struct source_file_origin {
  source_code_provider* provider;
  std::filesystem::path path;
};

source_file_origin
make_eval_origin();

} // namespace insider

#endif
