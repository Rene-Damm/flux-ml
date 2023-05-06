structure Types =
struct
  
  datatype ty =
      NamedType         of Symbol.symbol * ty option ref
    | FunctionType      of ty * ty
    | TupleType         of ty * ty
    | UnionType         of ty * ty
    | IntersectionType  of ty * ty

  (*fun isSubtype (NamedType(_, baseType1), NamedType(s, baseType2)) = (s = Env.builtinObject)*)

end

