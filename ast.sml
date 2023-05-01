structure AST =
struct

  datatype modifier =
      Abstract              of Source.region
    | Immutable             of Source.region
    | Mutable               of Source.region
    | Builtin               of Source.region

  datatype identifier =
      Identifier            of { region: Source.region, text: string }

  datatype expression =
      Name                  of identifier
    | Integer               of { region: Source.region, value: int }
    | Float                 of { region: Source.region, value: real }
    | String                of { region: Source.region, value: string }

  datatype statement =
      Return                of { region: Source.region, value: expression option }

  datatype expressionOrStatementBlock =
      Expression            of expression
    | Statement             of statement list

  datatype definitionType = Type | Method | Field | Object | Local

  datatype definition =
      Definition            of { region: Source.region,
                                 modifiers: modifier list,
                                 defType: definitionType,
                                 name: identifier }

  fun print stream ast =
    let
      fun put s = TextIO.output(stream, s)
      fun putln s = (put s; put "\n")
      fun putIndent 0 = ()
        | putIndent n = (put "  "; putIndent (n - 1))
      val indentLevel = ref 0
      val increaseIndentation = (indentLevel := !indentLevel + 1)
      val decreaseIndentation = (indentLevel := !indentLevel - 1)
      fun indent () = putIndent (!indentLevel)

      fun id (Identifier { region = _, text = t }) = put t

      fun definitionType Type = "TYPE"
        | definitionType Method = "METHOD"
        | definitionType Field = "FIELD"
        | definitionType Object = "OBJECT"
        | definitionType Local = "LOCAL"

      fun definition (Definition { defType = t, region = _, modifiers = _, name = n }) = 
        (indent ();
         put (definitionType t);
         put " ";
         id n;
         put "\n")

      fun definitionList [] = ()
        | definitionList (d::rest) = (definitionList rest; definition d)
    in
      definitionList ast
    end

end

