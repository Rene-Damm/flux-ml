structure Types =
struct

  datatype ty =
      RootType
    | DerivedType       of Symbol.symbol * ty
    | VariableType      of Symbol.symbol * ty
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
    | isSubtype (DerivedType (s1, t1), right as DerivedType (s2, t2)) =
        if Symbol.isSame (s1, s2)
        then true
        else isSubtype (t1, right)
    | isSubtype (DerivedType (_, b), t) = isSubtype (b, t)
    | isSubtype (VariableType (_, b), t) = isSubtype (b, t)
    | isSubtype (UnionType (a, b), t) = isSubtype (a, t) andalso isSubtype (b, t)
    | isSubtype (IntersectionType (a, b), t) = isSubtype (a, t) orelse isSubtype (b, t)
    | isSubtype (MutableType (_), ImmutableType (_)) = false
    | isSubtype (ImmutableType (_), MutableType (_)) = false
    | isSubtype (MethodType (a1, r1), MethodType (a2, r2)) = isSubtype (a1, a2) andalso isSubtype (r1, r2)
    | isSubtype (ClosureType (a1, r1), ClosureType (a2, r2)) = isSubtype (a1, a2) andalso isSubtype (r2, r1)
    | isSubtype (_, _) = false

  fun getLeftOperandType (MethodType (l, _)) = l
    | getLeftOperandType (ClosureType (l, _)) = l
    | getLeftOperandType (TupleType (l, _)) = l
    | getLeftOperandType (UnionType (l, _)) = l
    | getLeftOperandType (IntersectionType (l, _)) = l
    | getLeftOperandType (ParameterizedType (l, _)) = l
    | getLeftOperandType (InstancedType (l, _)) = l
    | getLeftOperandType _ = raise Utils.ShouldNotGetHere

  fun getRightOperandType (MethodType (_, r)) = r
    | getRightOperandType (ClosureType (_, r)) = r
    | getRightOperandType (TupleType (_, r)) = r
    | getRightOperandType (UnionType (_, r)) = r
    | getRightOperandType (IntersectionType (_, r)) = r
    | getRightOperandType (ParameterizedType (_, r)) = r
    | getRightOperandType (InstancedType (_, r)) = r
    | getRightOperandType _ = raise Utils.ShouldNotGetHere

  fun toString (RootType) = "Object"
    | toString (DerivedType (s, t)) = (Symbol.toString s) ^ " : " ^ (toString t)
    | toString (VariableType (s, t)) = "'" ^ (Symbol.toString s) ^ " : " ^ (toString t)
    | toString (UnionType (a, b)) = "(" ^ (toString a) ^ ") & (" ^ (toString b) ^ ")"
    | toString (IntersectionType (a, b)) = "(" ^ (toString a) ^ ") | (" ^ (toString b) ^ ")"
    | toString (MutableType (t)) = "mutable " ^ (toString t)
    | toString (ImmutableType (t)) = "immutable " ^ (toString t)
    | toString (SingletonType (t)) = "singleton " ^ (toString t)
    | toString (MethodType (a, r)) = "(" ^ (toString a) ^ ") -> (" ^ (toString r) ^ ")"
    | toString (ClosureType (a, r)) = "(" ^ (toString a) ^ ") => (" ^ (toString r) ^ ")"
    | toString (ParameterizedType (DerivedType (s, t), p)) = (Symbol.toString s) ^ "<" ^ (toString p) ^ "> : " ^ (toString t)
    | toString (ParameterizedType (t, p)) = "<" ^ (toString p) ^ "> " ^ (toString t)
    | toString _ = raise Utils.NotImplemented

end

