structure Symbol =
struct

  type symbol = string * int

  exception Symbol
  val nextSymbol = ref 0
  val hashtable : (string, int) HashTable.hash_table =
    HashTable.mkTable(HashString.hashString, op =)(123, Symbol)

  fun create name =
    case HashTable.find hashtable name
      of SOME i => (name, i)
       | NONE => let val i = !nextSymbol
                 in
                   nextSymbol := i + 1;
                   HashTable.insert hashtable (name, i);
                   (name, i)
                 end

  fun toString (name, _) = name

  type 'a table = 'a IntBinaryMap.map

  val emptyTable = IntBinaryMap.empty

  fun enter(table: 'a table, (_, n): symbol, value: 'a) =
    IntBinaryMap.insert(table, n, value)

  fun lookup(table: 'a table, (_, n): symbol) =
    IntBinaryMap.find(table, n)

end

