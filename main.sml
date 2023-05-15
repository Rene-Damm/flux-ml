structure Main =
struct

  fun run() =
    let
      val ast = Parse.parse "program.flux"
      val _ = Utils.writeToFile ("program.flux.ast", fn stream => AST.print stream ast)
    in
      Translate.translateProgram ast
    end

end

