signature ENV =
sig

  val builtinObject : Symbol.symbol

  type typeTable = Types.ty Symbol.table
  type functionTable = Function.fu Symbol.table

  val dumpTypes : typeTable * TextIO.outstream -> unit
  val dumpFunctions : functionTable * TextIO.outstream -> unit

end

structure Env : ENV =
struct

  type typeTable = Types.ty Symbol.table
  type functionTable = Function.fu Symbol.table

  val builtinObject = Symbol.create "Object"

  fun dumpTypes(table: typeTable, stream) =
  let
    fun put s = TextIO.output(stream, s)
    fun putln s = (put s; put "\n")

    fun printType (s, Types.DerivedType(_)) = (put "type:"; putln (Symbol.toString s))
      | printType (s, Types.RootType) = (put "type:"; putln (Symbol.toString s))
      | printType _ = ()

    fun print [] = ()
      | print (t::rest) = (printType t; print rest)
  in
    print (Symbol.all table)
  end

  fun dumpFunctions(table: functionTable, stream) =
  let
    fun put s = TextIO.output(stream, s)
    fun putln s = (put s; put "\n")

    fun printFunction (s, _) = (put "func:"; putln (Symbol.toString s))

    fun print [] = ()
      | print (f::rest) = (printFunction f; print rest)
  in
    print (Symbol.all table)
  end

end

