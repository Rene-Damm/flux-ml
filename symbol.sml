signature SYMBOL =
sig

  type symbol

  exception SymbolNotFound of symbol

  val create : string -> symbol
  val toString : symbol -> string
  val isSame : (symbol * symbol) -> bool

  type 'a table

  val emptyTable : 'a table
  val enter : ('a table * symbol * 'a) -> 'a table
  val remove : ('a table * symbol) -> 'a table
  val lookup : ('a table * symbol ) -> 'a option
  val find : ('a table * symbol ) -> 'a
  val all : ('a table) -> (symbol * 'a) list
  val entries : ('a table) -> 'a list
  val numItems : ('a table) -> int

end

structure Symbol : SYMBOL =
struct

  type symbol = Atom.atom

  exception SymbolNotFound of symbol

  fun create name = Atom.atom name
  fun toString symbol = Atom.toString symbol
  fun isSame (a, b) = Atom.same (a, b)

  type 'a table = 'a AtomBinaryMap.map

  val emptyTable = AtomBinaryMap.empty

  fun enter (table, symbol, value) =
    AtomBinaryMap.insert (table, symbol, value)

  fun remove (table, symbol) =
    let
      val (newTable, _) = AtomBinaryMap.remove (table, symbol)
    in
      newTable
    end

  fun lookup (table, symbol) =
    AtomBinaryMap.find (table, symbol)

  fun find (table, symbol) =
    case List.find (fn (s, _) => (Utils.println ("want: " ^ (toString s)); Utils.println ("got: " ^ (toString s)); String.compare(toString s, toString symbol) = EQUAL)) (AtomBinaryMap.listItemsi table)
      of SOME (_, v) => v
       | NONE => (Utils.println (toString symbol); raise SymbolNotFound symbol)
       (*
    case lookup (table, symbol)
      of SOME value => value
       | NONE => (Utils.println (toString symbol); raise SymbolNotFound symbol)
       *)

  fun all table = AtomBinaryMap.listItemsi table
  fun entries table = AtomBinaryMap.listItems table
  fun numItems table = AtomBinaryMap.numItems table

end

