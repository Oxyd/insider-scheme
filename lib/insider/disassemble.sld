(define-library (insider disassemble)
  (import (insider syntax) (insider basic-procedures) (insider io)
          (insider string) (insider list) (insider vector) (insider numeric)
          (only (insider internal)
                procedure-prototype-bytecode procedure-prototype-name opcodes
                instruction-opcode instruction-operands top-level-name
                procedure-prototype-constants top-level-value immediate-bias
                procedure-parameter-count procedure-closure-size
                procedure-prototype))
  (export disassemble)
  (include "disassemble.scm"))
