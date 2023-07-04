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

  fun pprint tree = 
    let
      fun operator PLUS = "+"
        | operator MINUS = "-"
        | operator MUL = "*"
        | operator DIV = "/"
        | operator AND = "&"
        | operator OR = "|"
        | operator _ = raise Utils.NotImplemented

      fun expression (CALL (label, args)) = PPrint.Seq [PPrint.String "CALL(", PPrint.String ((Temp.labelToString label) ^ ","), PPrint.List (PPrint.String ",", map expression args), PPrint.String ")"]
        | expression TRUE = PPrint.String "TRUE"
        | expression FALSE = PPrint.String "FALSE"
        | expression NOTHING = PPrint.String "NOTHING"
        | expression (INT i) = PPrint.String (Int.toString i)
        | expression (FLOAT f) = PPrint.String (Real.toString f)
        | expression (NAME label) = PPrint.String (Temp.labelToString label)
        | expression (TEMP label) = PPrint.String (Temp.labelToString label)
        | expression (BINOP (oper, left, right)) = PPrint.Seq [PPrint.String ((operator oper) ^ "("), expression left, PPrint.String ",", expression right, PPrint.String ")"]
        | expression _ = raise Utils.NotImplemented

      fun statement NOP = PPrint.String "NOP"
        | statement (LABEL label) = PPrint.String ("LABEL " ^ (Temp.labelToString label))
        | statement (SEQ (left, right)) = PPrint.seq (statement left, statement right)
        | statement (EXPR e) = expression e
        | statement (MOVE (dst, src)) = PPrint.Seq [PPrint.String "MOVE(", expression dst, PPrint.String ",", expression src, PPrint.String ")"]
        | statement (JUMP label) = PPrint.String ("JMP " ^ (Temp.labelToString label))
        | statement _ = raise Utils.NotImplemented
    in
      statement tree
    end

end

