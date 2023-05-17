structure Functions =
struct

  (* A dispatch tree forms a DAG of ever more specialized method types as one
   * walks down the tree. *)
  datatype 'a dispatchNode =
      DispatchNode of { methodType: Types.ty,
                        children: 'a dispatchNode list,
                        method: 'a }

  exception DuplicateTypeInDispatchTree

  (* A function is a named collection of methods arranged in a dispatch tree. *)
  type 'a fu = Symbol.symbol * 'a dispatchNode

  fun createDispatchTree (methodType, method) =
    DispatchNode { methodType = methodType, children = [], method = method }

  fun insertDispatchNode (node as DispatchNode { methodType = nodeType, children = nodeChildren, method = nodeMethod }, methodType, method) =
  let
    fun goThroughChildren [] = [DispatchNode { methodType = methodType, children = [], method = method }]
      | goThroughChildren ((child as DispatchNode { methodType = childType, children = childChildren, method = childMethod })::rest) =
          let
            val subtype = Types.isSubtype (methodType, childType)
            val supertype = Types.isSubtype (childType, methodType)
          in
            if subtype andalso supertype then raise DuplicateTypeInDispatchTree
            else if subtype then insertDispatchNode (child, methodType, method)::(goThroughChildren rest) (* More specialized *)
            else DispatchNode { methodType = methodType, children = [child], method = method }::rest (* Less specialized *)
          end
  in
    if Types.isSubtype (nodeType, methodType) then DispatchNode { methodType = methodType, children = [node], method = method }
    else DispatchNode { methodType = nodeType, children = goThroughChildren nodeChildren, method = nodeMethod }
  end

  fun getDispatchNodeChildren (DispatchNode { methodType = _, children = c, method = _ }) = c
  fun getDispatchNodeMethod (DispatchNode { methodType = _, children = _, method = m }) = m

end

