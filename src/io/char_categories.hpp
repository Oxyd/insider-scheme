#ifndef INSIDER_IO_CHAR_CATEGORIES_HPP
#define INSIDER_IO_CHAR_CATEGORIES_HPP

namespace insider {

inline bool
whitespace(char32_t c) {
  return c == ' ' || c == '\n' || c == '\t';
}

inline bool
delimiter(char32_t c) {
  return whitespace(c)
         || c == '(' || c == ')'
         || c == '\'' || c == '"' || c == '`'
         || c == '#' || c == ';'
    ;
}

inline bool
control(char32_t c) {
  return c < 0x20;
}

}

#endif
