signature SYMBOL =
sig

  type symbol

  val create : string -> symbol
  val toString : symbol -> string
  val isSame : (symbol * symbol) -> bool

  type 'a table

  val emptyTable : 'a table
  val enter : ('a table * symbol * 'a) -> 'a table
  val lookup : ('a table * symbol ) -> 'a option
  val all : ('a table) -> (symbol * 'a) list
  val entries : ('a table) -> 'a list

end

structure Symbol : SYMBOL =
struct

  type symbol = Atom.atom

  fun create name = Atom.atom name
  fun toString symbol = Atom.toString symbol
  fun isSame (a, b) = Atom.same (a, b)

  type 'a table = 'a AtomBinaryMap.map

  val emptyTable = AtomBinaryMap.empty

  fun enter(table, symbol, value) =
    AtomBinaryMap.insert(table, symbol, value)

  fun lookup(table, symbol) =
    AtomBinaryMap.find(table, symbol)

  fun all table =
    AtomBinaryMap.listItemsi table

  fun entries table =
    AtomBinaryMap.listItems table

end

