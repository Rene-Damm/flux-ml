structure Parse :
sig
  val parse : string -> AST.definition list
end =
struct

  structure FluxLrVals = FluxLrValsFun(structure Token = LrParser.Token)
  structure FluxLex = FluxLexFun(structure Tokens = FluxLrVals.Tokens)
  structure FluxParser = Join(
    structure LrParser = LrParser
    structure ParserData = FluxLrVals.ParserData
    structure Lex = FluxLex)

  fun parse filename =
    let
      val file = TextIO.openIn filename
      fun get _ = TextIO.input file
      fun parseError(msg, startPos, endPos) = ()
      val lexer = LrParser.Stream.streamify (FluxLex.makeLexer get)
      val (ast, _) = FluxParser.parse(30, lexer, parseError, ())
    in
      TextIO.closeIn file;
      ast
    end

end

