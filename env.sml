signature ENV =
sig

  val builtinObject : Symbol.symbol
  val builtinNothing : Symbol.symbol
  val builtinInteger : Symbol.symbol
  val builtinFloat : Symbol.symbol
  val builtinString : Symbol.symbol

  type typeTable = Types.ty Symbol.table
  type 'a functionTable = 'a Functions.fu Symbol.table

  val dumpTypes : typeTable * TextIO.outstream -> unit
  val dumpFunctions : 'a functionTable * TextIO.outstream -> unit

end

structure Env : ENV =
struct

  type typeTable = Types.ty Symbol.table
  type 'a functionTable = 'a Functions.fu Symbol.table

  val builtinObject = Symbol.create "Object"
  val builtinNothing = Symbol.create "Nothing"
  val builtinInteger = Symbol.create "Integer"
  val builtinFloat = Symbol.create "Float"
  val builtinString = Symbol.create "String"

  fun dumpTypes(table: typeTable, stream) =
  let
    fun put s = TextIO.output(stream, s)
    fun putln s = (put s; put "\n")

    fun printType (s, Types.DerivedType(_)) = (put "type:"; putln (Symbol.toString s))
      | printType (s, Types.RootType) = (put "type:"; putln (Symbol.toString s))
      | printType (s, Types.UnionType(_, _)) = (put "type:"; putln (Symbol.toString s))
      | printType (s, Types.IntersectionType(_, _)) = (put "type:"; putln (Symbol.toString s))
      | printType _ = ()

    fun print [] = ()
      | print (t::rest) = (printType t; print rest)
  in
    print (Symbol.all table)
  end

  fun dumpFunctions(table: 'a functionTable, stream) =
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
