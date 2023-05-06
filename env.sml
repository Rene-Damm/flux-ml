structure Env =
struct

  val builtinObject = Symbol.create "Object"

  fun dumpTypes(table: Types.ty Symbol.table, stream) =
  let
    fun put s = TextIO.output(stream, s)
    fun putln s = (put s; put "\n")

    fun printType (Types.NamedType((s, _), _)) = (put "type:"; putln s)
      | printType _ = ()

    fun print [] = ()
      | print (t::rest) = (printType t; print rest)
  in
    print (IntBinaryMap.listItems table)
  end

  fun dumpFunctions(table: Function.fu Symbol.table, stream) =
  let
    fun put s = TextIO.output(stream, s)
    fun putln s = (put s; put "\n")

    fun printFunction ((name, _), _) = (put "func:"; putln name)

    fun print [] = ()
      | print (f::rest) = (printFunction f; print rest)
  in
    print (IntBinaryMap.listItems table)
  end

end

