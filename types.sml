structure Types =
struct

  datatype ty =
      RootType
    | DerivedType       of Symbol.symbol * ty
    | FunctionType      of ty * ty
    | TupleType         of ty * ty
    | UnionType         of ty * ty
    | IntersectionType  of ty * ty
    | MutableType       of ty
    | ImmutableType     of ty
    | SingletonType     of ty

  (*fun isSubtype (NamedType(_, baseType1), NamedType(s, baseType2)) = (s = Env.builtinObject)*)

end

