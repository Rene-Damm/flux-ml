structure Utils =
struct

  exception ShouldNotGetHere

  fun writeToFile (filename, writeFn) =
  let
    val stream = TextIO.openOut filename
  in
    writeFn (stream);
    TextIO.closeOut stream
  end

end

