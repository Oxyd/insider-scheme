(library (insider char))
(import (insider syntax) (except (insider internal) define let))
(export char-alphabetic? char-numeric? char-whitespace? char-upper-case? char-lower-case? digit-value
        char->integer integer->char)
