structure Function =
struct

  datatype dispatchNode =
      DispatchNode of Types.ty * dispatchNode list

  type method = unit

  type fu = Symbol.symbol * method list

end

