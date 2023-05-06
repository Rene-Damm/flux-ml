structure Main =
struct

  fun run() =
    let
      val ast = Parse.parse "program.flux"
    in
      Translate.translateProgram ast
    end

end

