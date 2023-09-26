structure Utils =
struct

  exception ShouldNotGetHere
  exception NotImplemented
  exception AssertFailure of string

  fun println str = print (str ^ "\n")

  fun assert _ true = ()
    | assert str false = (println str; raise AssertFailure str)

  fun writeToFile (filename, writeFn) =
    let
      val stream = TextIO.openOut filename
    in
      writeFn (stream);
      TextIO.closeOut stream
    end

  (* Just a little helper to identify the source of valOf failures given that SMLNJ has no backtraces *)
  fun valOf sym opt =
    case opt
      of SOME v => v
       | NONE => (print (sym ^ " FAIL!"); raise Option.Option)

  (* Zips two lists into one. *)
  fun zip func (a::restA) (b::restB) = (func (a, b))::(zip func restA restB)
    | zip _ [] _ = []
    | zip _ _ [] = []

  fun zip3 func (a::restA) (b::restB) (c::restC) = (func (a, b, c))::(zip3 func restA restB restC)
    | zip3 _ [] _ _ = []
    | zip3 _ _ [] _ = []
    | zip3 _ _ _ [] = []

  (* Zips and folds two lists at the same time. *)
  fun zipFold func c (a::restA) (b::restB) = zipFold func (func (c, a, b)) restA restB
    | zipFold _ c [] _ = c
    | zipFold _ c _ [] = c

  fun takeItem func (a::[]) = if func a then (a, []) else raise List.Empty
    | takeItem func (a::rest) = if func a then (a, rest) else let val (item, lst) = takeItem func rest in (item, a::lst) end
    | takeItem _ [] = raise List.Empty

  fun maybeTakeItem func (a::[]) = if func a then (SOME a, []) else (NONE, [a])
    | maybeTakeItem func (a::rest) = if func a then (SOME a, rest) else let val (item, lst) = maybeTakeItem func rest in (item, a::lst) end
    | maybeTakeItem _ [] = (NONE, [])

end

