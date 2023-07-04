structure PPrint =
struct

  datatype ppelement = Block of ppelement | Seq of ppelement list | List of ppelement * ppelement list | String of string | Newline | Space

  datatype pprinter = PPrinter of { stream: TextIO.outstream, indentLevel: int ref }

  fun block elements = Block (List (Seq [String ";", Newline], elements))

  fun seq (Seq left, Seq right) = Seq (List.concat [left, right])
    | seq (left, Seq right) = Seq (left::right)
    | seq (left, right) = Seq [left, right]

  fun print stream elements =
    let
      fun put s = TextIO.output (stream, s)
      fun putln s = (put s; put "\n")
      fun putIndent 0 = ()
        | putIndent n = (put "  "; putIndent (n - 1))
      val indentLevel = ref 0
      fun increaseIndentation () = (indentLevel := !indentLevel + 1)
      fun decreaseIndentation () = (indentLevel := !indentLevel - 1)
      fun indent () = putIndent (!indentLevel)

      fun prn (Block element) = (putln "["; increaseIndentation (); indent (); prn element; decreaseIndentation (); indent (); put "]")
        | prn (Seq elements) = app prn elements
        | prn (List (separator, elements)) =
            let
              fun elt [] = ()
                | elt (e::[]) = prn e
                | elt (e::rest) = (prn e; prn separator; elt rest)
            in
              elt elements
            end
        | prn (String str) = put str
        | prn Newline = (put "\n"; indent ())
        | prn Space = put " "
    in
      app prn elements
    end

end

