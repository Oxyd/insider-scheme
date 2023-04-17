#ifndef INSIDER_IO_TOKENIZER_HPP
#define INSIDER_IO_TOKENIZER_HPP

#include "compiler/source_location.hpp"
#include "object.hpp"

#include <string>
#include <variant>

namespace insider {

class reader_stream;

struct token {
  struct end { };
  struct left_paren { };
  struct right_paren { };
  struct octothorpe_left_paren { };
  struct octothorpe_u8 { };
  struct dot { };

  struct generic_literal {
    ptr<> value;
  };

  struct boolean_literal {
    bool value;
  };

  struct void_literal { };

  struct default_value_literal { };

  struct identifier {
    std::string value;
  };

  struct keyword_literal {
    std::string value;
  };

  struct quote { };
  struct backquote { };
  struct comma { };
  struct comma_at { };
  struct octothorpe_quote { };
  struct octothorpe_backquote { };
  struct octothorpe_comma { };
  struct octothorpe_comma_at { };

  struct datum_label_definition {
    std::string label;
  };

  struct datum_label_reference {
    std::string label;
  };

  struct datum_comment { };

  using value_type = std::variant<
    end,
    left_paren,
    right_paren,
    octothorpe_left_paren,
    octothorpe_u8,
    dot,
    generic_literal,
    boolean_literal,
    void_literal,
    default_value_literal,
    identifier,
    keyword_literal,
    quote,
    backquote,
    comma,
    comma_at,
    octothorpe_quote,
    octothorpe_backquote,
    octothorpe_comma,
    octothorpe_comma_at,
    datum_label_definition,
    datum_label_reference,
    datum_comment
  >;

  value_type      value;
  source_location location;
};

ptr<>
read_number(context& ctx, reader_stream& stream, source_location const& loc,
            unsigned default_base = 10);

token
read_token(context& ctx, reader_stream& stream);

} // namespace insider

#endif
