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

  (* Zips and folds two lists at the same time. *)
  fun zipFold func c (a::restA) (b::restB) = zipFold func (func (c, a, b)) restA restB
    | zipFold _ c [] _ = c
    | zipFold _ c _ [] = c

end

