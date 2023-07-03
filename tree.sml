structure Tree =
struct

  datatype expr = TRUE
                | FALSE
                | NOTHING
                | INT of int
                | FLOAT of real
                | NAME of Temp.label
                | TEMP of Temp.temp
                | BINOP of binop * expr * expr
                | MEM of expr
                | CALL of Temp.label * expr list

       and stmt = MOVE of expr * expr
                | EXPR of expr
                | JUMP of Temp.label
                | CJUMP of relop * expr * expr * Temp.label * Temp.label (*turn into record*)
                | SEQ of stmt * stmt
                | LABEL of Temp.label
                | NOP

       and binop = PLUS | MINUS | MUL | DIV | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

       and relop = EQ | NE | LT | GT | GE | ULT | ULE | UGT | UGE

  (*fun makeSeq (SEQ (a, b), rest)*)

end

