signature ENV =
sig

  val builtinObject : Symbol.symbol
  val builtinNothing : Symbol.symbol
  val builtinInteger : Symbol.symbol
  val builtinFloat : Symbol.symbol
  val builtinString : Symbol.symbol
  val builtinTrue : Symbol.symbol
  val builtinFalse : Symbol.symbol
  val builtinPlus : Symbol.symbol
  val builtinMinus : Symbol.symbol
  val builtinModulo : Symbol.symbol
  val builtinDefault : Symbol.symbol

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
  val builtinTrue = Symbol.create "True"
  val builtinFalse = Symbol.create "False"
  val builtinPlus = Symbol.create "+"
  val builtinMinus = Symbol.create "-"
  val builtinModulo = Symbol.create "%"
  val builtinDefault = Symbol.create "Default"

  fun dumpTypes(table: typeTable, stream) =
  let
    fun put s = TextIO.output(stream, s)
    fun putln s = (put s; put "\n")

    fun printType (s, t) = (put "type:"; putln (Types.toString t))

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

