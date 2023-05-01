structure Main =
struct

  fun run() =
    let
      val ast = Parse.parse "program.flux"
    in
      AST.print TextIO.stdOut ast
    end

end

