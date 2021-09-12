#ifndef INSIDER_SOURCE_FILE_ORIGIN_HPP
#define INSIDER_SOURCE_FILE_ORIGIN_HPP

#include <filesystem>

namespace insider {

class source_code_provider;

struct source_file_origin {
  source_code_provider* provider;
  std::filesystem::path path;
};

} // namespace insider

#endif
