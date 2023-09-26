structure Tree =
struct

  datatype expr = TRUE
                | FALSE
                | NOTHING
                | INT of int
                | FLOAT of real
                | TEMP of Temp.temp
                | BINOP of binop * expr * expr
                | MEM of expr
                | CALL of Temp.label * expr list (* No temps used for return values. *)

       and stmt = MOVE of expr * expr
                | EXPR of expr
                | JUMP of Temp.label
                | CJUMP of relop * expr * expr * Temp.label * Temp.label
                | SEQ of stmt list
                | LABEL of Temp.label
                | NOP

       and binop = PLUS | MINUS | MUL | DIV | MOD | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

       and relop = EQ | NE | LT | GT | LE | GE

  fun isJump (JUMP _) = true
    | isJump (CJUMP (_, _, _, _, _)) = true
    | isJump _ = false

  fun pprint tree = 
    let
      fun operator PLUS = "+"
        | operator MINUS = "-"
        | operator MUL = "*"
        | operator DIV = "/"
        | operator AND = "&"
        | operator OR = "|"
        | operator _ = raise Utils.NotImplemented

      fun relop EQ = "EQ"
        | relop NE = "NE"
        | relop LT = "LT"
        | relop GT = "GT"
        | relop LE = "LE"
        | relop GE = "GE"

      fun expression (CALL (label, args)) = PPrint.Seq [PPrint.String "CALL(", PPrint.String ((Temp.labelToString label) ^ ","), PPrint.List (PPrint.String ",", map expression args), PPrint.String ")"]
        | expression TRUE = PPrint.String "TRUE"
        | expression FALSE = PPrint.String "FALSE"
        | expression NOTHING = PPrint.String "NOTHING"
        | expression (INT i) = PPrint.String (Int.toString i)
        | expression (FLOAT f) = PPrint.String (Real.toString f)
        | expression (TEMP label) = PPrint.String (Temp.labelToString label)
        | expression (BINOP (oper, left, right)) = PPrint.Seq [PPrint.String ((operator oper) ^ "("), expression left, PPrint.String ",", expression right, PPrint.String ")"]
        | expression _ = raise Utils.NotImplemented

      fun statement NOP = PPrint.String "NOP"
        | statement (LABEL label) = PPrint.String ("LABEL " ^ (Temp.labelToString label))
        | statement (SEQ l) = PPrint.block (map statement l)
        | statement (EXPR e) = expression e
        | statement (MOVE (dst, src)) = PPrint.Seq [PPrint.String "MOVE(", expression dst, PPrint.String ",", expression src, PPrint.String ")"]
        | statement (JUMP label) = PPrint.String ("JMP " ^ (Temp.labelToString label))
        | statement (CJUMP (operator, left, right, trueLabel, falseLabel)) =
            PPrint.Seq [
              PPrint.String "CJUMP(",
              PPrint.String (relop operator),
              PPrint.String ",",
              expression left,
              PPrint.String ",",
              expression right,
              PPrint.String ",",
              PPrint.String (Temp.labelToString trueLabel),
              PPrint.String ",",
              PPrint.String (Temp.labelToString falseLabel),
              PPrint.String ")"
            ]
    in
      statement tree
    end

end

