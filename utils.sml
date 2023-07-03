structure Utils =
struct

  exception ShouldNotGetHere
  exception NotImplemented

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

end

