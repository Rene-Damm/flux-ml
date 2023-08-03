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
    | ParameterizedType of ty * ty (* Tuple of variable types plus base type that uses them. *)
    | InstancedType     of ty * ty (* Parameterized type that is instanced plus tuple of argument types. *) (*REVIEW: do we need this?!?!?!?*)
    | MutableType       of ty
    | ImmutableType     of ty
    | SingletonType     of ty

  fun toString (RootType) = "Object"
    | toString (DerivedType (s, t)) = (Symbol.toString s) ^ " : " ^ (toString t)
    | toString (VariableType (s, t)) = "'" ^ (Symbol.toString s) ^ " : " ^ (toString t)
    | toString (UnionType (a, b)) = "(" ^ (toString a) ^ ") & (" ^ (toString b) ^ ")"
    | toString (IntersectionType (a, b)) = "(" ^ (toString a) ^ ") | (" ^ (toString b) ^ ")"
    | toString (MethodType (a, r)) = "(" ^ (toString a) ^ ") -> (" ^ (toString r) ^ ")"
    | toString (ClosureType (a, r)) = "(" ^ (toString a) ^ ") => (" ^ (toString r) ^ ")"
    | toString (TupleType (a, b)) = (toString a) ^ "*" ^ (toString b)
    | toString (ParameterizedType (DerivedType (s, t), p)) = (Symbol.toString s) ^ "<" ^ (toString p) ^ "> : " ^ (toString t)
    | toString (ParameterizedType (t, p)) = "<" ^ (toString p) ^ "> " ^ (toString t)
    | toString (InstancedType (t, p)) = "(" ^ (toString t) ^ "<" ^ (toString p) ^ ">)"
    | toString (MutableType (t)) = "mutable " ^ (toString t)
    | toString (ImmutableType (t)) = "immutable " ^ (toString t)
    | toString (SingletonType (t)) = "singleton " ^ (toString t)

  fun toList (TupleType (a, b)) = a::(toList b)
    | toList t = [t]

  fun isSubtype (_, RootType) = true
    | isSubtype (DerivedType (s1, t1), right as DerivedType (s2, t2)) = Symbol.isSame (s1, s2) orelse isSubtype (t1, right)
    | isSubtype (DerivedType (_, b), t) = isSubtype (b, t)
    | isSubtype (VariableType (_, b), t) = isSubtype (b, t)
    | isSubtype (UnionType (a, b), t) = isSubtype (a, t) andalso isSubtype (b, t)
    | isSubtype (IntersectionType (a, b), t) = isSubtype (a, t) orelse isSubtype (b, t)
    | isSubtype (MutableType (_), ImmutableType (_)) = false
    | isSubtype (ImmutableType (_), MutableType (_)) = false
    | isSubtype (MutableType (a), b) = isSubtype (a, b)
    | isSubtype (ImmutableType (a), b) = isSubtype (a, b)
    | isSubtype (TupleType (a, b), TupleType (c, d)) = isSubtype (a, c) andalso isSubtype (b, d)
    | isSubtype (MethodType (a1, r1), MethodType (a2, r2)) = isSubtype (a1, a2) andalso isSubtype (r1, r2)
    | isSubtype (ClosureType (a1, r1), ClosureType (a2, r2)) = isSubtype (a1, a2) andalso isSubtype (r2, r1)
    | isSubtype (_, _) = false

  fun isFunctionType (MethodType (_, _)) = true
    | isFunctionType (ClosureType (_, _)) = true
    | isFunctionType (ParameterizedType (_, MethodType (_, _))) = true
    | isFunctionType (ParameterizedType (_, ClosureType (_, _))) = true
    | isFunctionType (InstancedType (_, MethodType (_, _))) = true
    | isFunctionType (InstancedType (_, ClosureType (_, _))) = true
    | isFunctionType _ = false

  fun isParameterizedType (ParameterizedType (_, _)) = true
    | isParameterizedType _ = false

  fun getLeftOperandType (MethodType (l, _)) = l
    | getLeftOperandType (ClosureType (l, _)) = l
    | getLeftOperandType (TupleType (l, _)) = l
    | getLeftOperandType (UnionType (l, _)) = l
    | getLeftOperandType (IntersectionType (l, _)) = l
    | getLeftOperandType (ParameterizedType (_, r)) = getLeftOperandType r
    | getLeftOperandType (InstancedType (_, r)) = getLeftOperandType r
    | getLeftOperandType t = (print (toString t); raise Utils.ShouldNotGetHere)

  fun getRightOperandType (MethodType (_, r)) = r
    | getRightOperandType (ClosureType (_, r)) = r
    | getRightOperandType (TupleType (_, r)) = r
    | getRightOperandType (UnionType (_, r)) = r
    | getRightOperandType (IntersectionType (_, r)) = r
    | getRightOperandType (ParameterizedType (_, r)) = getRightOperandType r
    | getRightOperandType (InstancedType (_, r)) = getRightOperandType r
    | getRightOperandType _ = raise Utils.ShouldNotGetHere

  exception TooManyArgumentsInInstantation
  exception TypeConstraintNotMetInInstantiation of { pos: int, var: ty, arg: ty }

  (* Creates an InstancedType from a ParameterizedType.

     Raises TooManyArgumentsInInstantation or TypeConstraintNotMetInInstantiation if instantiation fails.

     Example: 
       type List< T : Object >;
       object Foo : List< Integer >;

       List< T : Object > == ParameterizedType(VariableType("T", RootType), DerivedType("List", RootType))
       List< Integer > == InstancedType(ParameterType(VariableType("T", RootType), DerivedType("List", RootType)), DerivedType("Integer", ...))
  *)
  fun instantiate (parameterizedType as ParameterizedType (vars, base), args) =
        let
          val varsList = toList vars
          val argsList = toList args

          val numVars = length varsList
          val numArgs = length argsList

          (* Check each supplied argument against its corresponding type variable.
             If all constraints are met, return a SuccessfulInstantiation. *)
          fun doIt n (var::restVar) (arg::restArg) =
                if isSubtype (arg, var)
                then doIt (n - 1) restVar restArg
                else raise TypeConstraintNotMetInInstantiation { pos = n, var = var, arg = var }
            | doIt _ _ [] = InstancedType (parameterizedType, args)
            | doIt _ _ _ = raise Utils.ShouldNotGetHere
        in
          if numArgs > numVars
          then raise TooManyArgumentsInInstantation
          else doIt (numVars - 1) varsList argsList
        end
    | instantiate (_, _) = raise Utils.ShouldNotGetHere

    (* Takes an InstancedType and applies the type arguments inside of it to its
       ParameterizedType. The result is either a plain type with no parameterization (and
       thus VariableTypes) left -or-, if there are fewer type arguments supplied in the
       InstancedType than required by the ParameterizedType, the result is a new ParameterizedType
       with just the supplied parameters removed ("partial specialization"). *)
  fun applyInstantiation (InstancedType (ParameterizedType (vars, base), args)) =
        let 
          fun replace (DerivedType (s, t), oldType, newType) = DerivedType (s, replace (t, oldType, newType))
            | replace (VariableType (s1, c1), oldType as VariableType (s2, _), newType) =
                if Symbol.isSame (s1, s2)
                then newType
                else VariableType (s1, replace (c1, oldType, newType))
            | replace (MethodType (a, b), oldType, newType) = MethodType (replace (a, oldType, newType), replace (b, oldType, newType))
            | replace (ClosureType (a, b), oldType, newType) = ClosureType (replace (a, oldType, newType), replace (b, oldType, newType))
            | replace (TupleType (a, b), oldType, newType) = TupleType (replace (a, oldType, newType), replace (b, oldType, newType))
            | replace (UnionType (a, b), oldType, newType) = UnionType (replace (a, oldType, newType), replace (b, oldType, newType))
            | replace (IntersectionType (a, b), oldType, newType) = IntersectionType (replace (a, oldType, newType), replace (b, oldType, newType))
            (*REVIEW: Is this correct for parameterized and instanced types? *)
            | replace (ParameterizedType (a, b), oldType, newType) = ParameterizedType (replace (a, oldType, newType), replace (b, oldType, newType))
            | replace (InstancedType (a, b), oldType, newType) = InstancedType (replace (a, oldType, newType), replace (b, oldType, newType))
            | replace (MutableType t, oldType, newType) = MutableType (replace (t, oldType, newType))
            | replace (ImmutableType t, oldType, newType) = ImmutableType (replace (t, oldType, newType))
            | replace (SingletonType t, oldType, newType) = SingletonType (replace (t, oldType, newType))
            | replace (t, _, _) = t

          val varsList = toList vars
          val argsList = toList args
        in
          Utils.zipFold replace base varsList argsList
        end
    | applyInstantiation _ = raise Utils.ShouldNotGetHere

end

