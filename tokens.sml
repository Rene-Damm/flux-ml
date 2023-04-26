structure Tokens =
struct

  type pos = int

  datatype token = EOF
                 | METHOD of pos
                 | TYPE of pos
                 | FIELD of pos
                 | ABSTRACT of pos
                 | IMMUTABLE of pos
                 | MUTABLE of pos
                 | BUILTIN of pos
                 | RETURN of pos
                 | ID of string * pos
                 | SEMICOLON of pos
                 | COLON of pos
                 | LPAREN of pos
                 | RPAREN of pos
                 | LBRACE of pos
                 | RBRACE of pos

  fun toString EOF = "EOF"
    | toString (METHOD _) = "METHOD"
    | toString (TYPE _) = "TYPE"
    | toString (FIELD _) = "FIELD"
    | toString (ABSTRACT _) = "ABSTRACT"
    | toString (IMMUTABLE _) = "IMMUTABLE"
    | toString (MUTABLE _) = "MUTABLE"
    | toString (BUILTIN _) = "BUILTIN"
    | toString (RETURN _) = "RETURN"
    | toString (ID(t, _)) = t
    | toString (SEMICOLON _) = ";"
    | toString (COLON _) = ":"
    | toString (LPAREN _) = "("
    | toString (RPAREN _) = ")"
    | toString (LBRACE _) = "{"
    | toString (RBRACE _) = "}"

end

