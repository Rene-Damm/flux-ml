structure Main =
struct

  fun run() =
    let
      val ast = Parse.parse "program.flux"
    in
      1
    end

end

