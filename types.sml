structure Types =
struct

  datatype ty =
      RootType
    | DerivedType       of Symbol.symbol * ty
    | MethodType        of ty * ty
    | ClosureType       of ty * ty
    | TupleType         of ty * ty
    | UnionType         of ty * ty
    | IntersectionType  of ty * ty
    | ParameterizedType of ty * ty
    | InstancedType     of ty * ty
    | MutableType       of ty
    | ImmutableType     of ty
    | SingletonType     of ty

  fun isSubtype (_, RootType) = true
    | isSubtype (DerivedType (s1, t1), DerivedType (s2, t2)) =
        if Symbol.isSame (s1, s2) then true
        else isSubtype (t1, t2)
    | isSubtype (UnionType (a, b), t) = isSubtype (a, t) andalso isSubtype (b, t)
    | isSubtype (IntersectionType (a, b), t) = isSubtype (a, t) orelse isSubtype (b, t)
    | isSubtype (MutableType (_), ImmutableType (_)) = false
    | isSubtype (ImmutableType (_), MutableType (_)) = false
    | isSubtype (MethodType (a1, r1), MethodType (a2, r2)) = isSubtype (a1, a2) andalso isSubtype (r1, r2)
    | isSubtype (ClosureType (a1, r1), ClosureType (a2, r2)) = isSubtype (a1, a2) andalso isSubtype (r2, r1)
    | isSubtype (_, _) = false

end

