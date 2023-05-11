structure AST =
struct

  datatype modifier =
      Abstract              of Source.region
    | Immutable             of Source.region
    | Mutable               of Source.region
    | Builtin               of Source.region

  datatype identifier =
      Identifier            of { region: Source.region, text: string }

  datatype operator = AND | OR

  datatype expression =
      Name                  of identifier
    | Integer               of { region: Source.region, value: int }
    | Float                 of { region: Source.region, value: real }
    | String                of { region: Source.region, value: string }
    | Binary                of { region: Source.region, operator: operator, left: expression, right: expression }

  datatype statement =
      Return                of { region: Source.region, value: expression option }
    | Variable              of definition

  and expressionOrStatementBlock =
      Expression            of expression
    | Statement             of statement list

  and definitionType = Type | Method | Field | Object | Local

  and definition =
      Definition            of { region: Source.region,
                                 modifiers: modifier list,
                                 defType: definitionType,
                                 typeExpr: expression option,
                                 name: identifier,
                                 body: expressionOrStatementBlock option }

  fun getDefinitionType (Definition { region = _, modifiers = _, defType = d, typeExpr = _, name = _, body = _ }) = d
  fun getDefinitionName (Definition { region = _, modifiers = _, defType = _, typeExpr = _, name = n, body = _ }) = n
  fun getDefinitionModifiers (Definition { region = _, modifiers = m, defType = _, typeExpr = _, name = _, body = _ }) = m
  fun getDefinitionTypeExpr (Definition { region = _, modifiers = _, defType = _, typeExpr = t, name = _, body = _ }) = t
  fun getDefinitionBody (Definition { region = _, modifiers = _, defType = _, typeExpr = _, name = _, body = b }) = b

  fun getIdentifierText (Identifier { region = _, text = t }) = t

  fun print stream ast =
    let
      fun put s = TextIO.output(stream, s)
      fun putln s = (put s; put "\n")
      fun putIndent 0 = ()
        | putIndent n = (put "  "; putIndent (n - 1))
      val indentLevel = ref 0
      fun increaseIndentation () = (indentLevel := !indentLevel + 1)
      fun decreaseIndentation () = (indentLevel := !indentLevel - 1)
      fun indent () = putIndent (!indentLevel)

      fun id (Identifier { region = _, text = t }) = put t

      fun operator AND = put "AND"
        | operator OR = put "OR"

      fun expression (Name(n)) = id n
        | expression (Integer { region = _, value = i }) = put (Int.toString i)
        | expression (Float { region = _, value = f }) = put (Real.toString f)
        | expression (String { region = _, value = s }) = put s
        | expression (Binary { region = _, operator = oper, left = l, right = r }) = (put "BINARY{o="; operator oper; put ",l="; expression l; put ",r="; expression r; put "}")

      fun expressionOpt (NONE) = ()
        | expressionOpt (SOME e) = expression e

      fun statement (Return { region = _, value = NONE }) = put "RETURN"
        | statement (Return { region = _, value = SOME e }) = (put "RETURN{e="; expression e; put "}")
        | statement (Variable(d)) = (put "LOCAL{d="; definition d; put "}")

      and exprOrStatement (NONE) = ()
        | exprOrStatement (SOME(Expression(e))) = expression e
        | exprOrStatement (SOME(Statement(l))) =
          let
            fun print [] = ()
              | print (s::rest) = (print rest; statement s)
          in
            put "[";
            increaseIndentation;
            print l;
            decreaseIndentation;
            put "]"
          end

      and modifier (Abstract(_)) = "ABSTRACT"
        | modifier (Immutable(_)) = "IMMUTABLE"
        | modifier (Mutable(_)) = "MUTABLE"
        | modifier (Builtin(_)) = "BUILTIN"

      and modifierList [] = ()
        | modifierList (m::[]) = put (modifier m)
        | modifierList (m::rest) = (put (modifier m); put ","; modifierList rest)

      and definitionType Type = "TYPE"
        | definitionType Method = "METHOD"
        | definitionType Field = "FIELD"
        | definitionType Object = "OBJECT"
        | definitionType Local = "LOCAL"

      and definition (Definition { defType = t, region = _, modifiers = m, typeExpr = e, name = n, body = b }) =
        (put "DEF{";
         put "name=";
         id n;
         put ",defType=";
         put (definitionType t);
         put ",modifiers=";
         modifierList m;
         put ",type=";
         expressionOpt e;
         put ",body=";
         exprOrStatement b;
         put "}\n")

      fun definitionList [] = ()
        | definitionList (d::rest) = (definitionList rest; indent (); definition d)

      fun program defList =
        (put "[\n";
         increaseIndentation ();
         definitionList defList;
         decreaseIndentation ();
         put "]\n")

    in
      program ast
    end

end

