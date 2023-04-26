structure Main =
struct

  fun run() =
    let
      val file = TextIO.openIn "program.flux"
      fun getStream _ = TextIO.input file
      val lexer = Mlex.makeLexer getStream
      fun doIt() =
        let val t = lexer()
        in
          print (Tokens.toString t);
          print "\n";
          case t of
               Tokens.EOF => ()
             | _ => doIt()
        end
    in
      doIt();
      TextIO.closeIn file
    end

end

