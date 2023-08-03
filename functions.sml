structure Functions =
struct

  (* A dispatch tree forms a DAG of ever more specialized method types as one
   * walks down the tree. *)
  datatype 'a dispatchNode =
      DispatchNode of { methodType: Types.ty,
                        children: 'a dispatchNode list,
                        (*TODO: add separate label for body*)
                        label: Temp.label,
                        method: 'a }

  exception DuplicateTypeInDispatchTree

  (* A function is a named collection of methods arranged in a dispatch tree. *)
  type 'a fu = Symbol.symbol * 'a dispatchNode

  fun createDispatchTree (methodType, label, method) =
    DispatchNode { methodType = methodType, children = [], label = label, method = method }

  fun insertDispatchNode (node as DispatchNode { methodType = nodeType, children = nodeChildren, label = nodeLabel, method = nodeMethod }, methodType, label, method) =
  let
    fun goThroughChildren [] = [DispatchNode { methodType = methodType, children = [], label = label, method = method }]
      | goThroughChildren ((child as DispatchNode { methodType = childType, children = childChildren, label = childLabel, method = childMethod })::rest) =
          let
            val subtype = Types.isSubtype (methodType, childType)
            val supertype = Types.isSubtype (childType, methodType)
          in
            if subtype andalso supertype then raise DuplicateTypeInDispatchTree
            else if subtype then insertDispatchNode (child, methodType, label, method)::(goThroughChildren rest) (* More specialized *)
            else DispatchNode { methodType = methodType, children = [child], label = label, method = method }::rest (* Less specialized *)
          end
  in
    if Types.isSubtype (nodeType, methodType) then DispatchNode { methodType = methodType, children = [node], label = label, method = method }
    else DispatchNode { methodType = nodeType, children = goThroughChildren nodeChildren, label = nodeLabel, method = nodeMethod }
  end

  (* Given a dispatch tree of methods and a type argument type (optional) and value argument type, find the method
     to invoke. Return its DispatchNode. *)
  fun lookupDispatchNode (node as DispatchNode { methodType = methodType, children = children, label = _, method = _ }, typeArgType, valueArgType) =
    let
      
      fun doIt () =
        let
          val _ = Utils.assert "Method type is function type (1)" (Types.isFunctionType methodType)

          (* If the method type is parameterized, apply the given type arguments first (which may be none). *)
          val methodType' =
            case (methodType, typeArgType)
              of (parameterizedType as Types.ParameterizedType (vars, base), SOME typeArgs) =>
                  let
                    val instancedType = Types.instantiate (parameterizedType, typeArgs)
                  in
                    Types.applyInstantiation instancedType
                  end
               | (_, _) => methodType

          val _ = Utils.assert "Method type is function type (2)" (Types.isFunctionType methodType')

          (* If the method type is still parameterized, infer any remaining type parameters. *)
          val methodType'' = 
            if Types.isParameterizedType methodType'
            then raise Utils.NotImplemented (*TODO: infer type argument *)
            else methodType'

          val _ = Utils.assert "Method type is function type (3)" (Types.isFunctionType methodType'')

          val methodArgType = Types.getLeftOperandType methodType''

          fun lookupChild [] = NONE
            | lookupChild (child::rest) =
                case lookupDispatchNode (child, typeArgType, valueArgType)
                  of NONE => lookupChild rest
                   | r => r
        in
          if Types.isSubtype (valueArgType, methodArgType)
          then
            case lookupChild children
              of SOME n => SOME n
               | NONE => SOME node
          else NONE
        end

    in
      doIt () handle
          Types.TooManyArgumentsInInstantation => NONE
        | Types.TypeConstraintNotMetInInstantiation _ => NONE
    end

  fun getDispatchNodeChildren (DispatchNode { methodType = _, children = c, label = _, method = _ }) = c
  fun getDispatchNodeMethod (DispatchNode { methodType = _, children = _, label = _, method = m }) = m
  fun getDispatchNodeLabel (DispatchNode { methodType = _, children = _, label = l, method = _ }) = l
  fun getDispatchNodeType (DispatchNode { methodType = t, children = _, label =_, method = _ }) = t

end

