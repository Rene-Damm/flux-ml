structure AST =
struct

  datatype modifier =
      Abstract              of Source.region
    | Immutable             of Source.region
    | Mutable               of Source.region
    | Builtin               of Source.region

  datatype identifier =
      Identifier            of { region: Source.region, text: string }

  datatype operator = AND | OR | APPLY | TYPE_APPLY | TUPLE | PLUS | MINUS | MUL | DIV | MOD
                    | DOT | NOT | GTEQ | LTEQ | EQ | NEQ | LT | GT | MUTABLE | IMMUTABLE

  datatype expression =
      Nothing               of { region: Source.region }
    (*| Name                  of { region: Source.region, name: identifier, typeArgs: expression list }*)
    | Name                  of identifier
    | Integer               of { region: Source.region, value: int }
    | Float                 of { region: Source.region, value: real }
    | String                of { region: Source.region, value: string }
    | Binary                of { region: Source.region, operator: operator, left: expression, right: expression }
    | Unary                 of { region: Source.region, operator: operator, expr: expression }

  datatype statement =
      Return                of { region: Source.region, value: expression option }
    | If                    of { region: Source.region, condition: expression, trueBranch: statement, falseBranch: statement option }
    | Switch                of { region: Source.region, value: expression, branches: caseBlock list }
    | Variable              of definition
    | ExpressionStatement   of expression
    | BlockStatement        of statement list

  and caseBlock = Case of { region: Source.region, values: expression list, body: statement list }

  and expressionOrStatementBlock =
      Expression            of expression
    | Statement             of statement list

  and definitionType = Type | Method | Field | Object | Local

  and definition =
      Definition            of { region: Source.region,
                                 modifiers: modifier list,
                                 defType: definitionType,
                                 typeParameters: definition list,
                                 valueParameters: definition list,
                                 typeExpr: expression option,
                                 name: identifier option,
                                 body: expressionOrStatementBlock option }

  fun getExpressionRegion (Name (Identifier { region = r, text = _ })) = r
    | getExpressionRegion (Nothing { region = r }) = r
    | getExpressionRegion (Integer { region = r, value = _ }) = r
    | getExpressionRegion (Float { region = r, value = _ }) = r
    | getExpressionRegion (String { region = r, value = _ }) = r
    | getExpressionRegion (Binary { region = r, operator = _, left = _, right = _ }) = r
    | getExpressionRegion (Unary { region = r, operator = _, expr = _ }) = r

  fun getDefinitionType (Definition { region = _, modifiers = _, defType = d, typeParameters = _, valueParameters = _, typeExpr = _, name = _, body = _ }) = d
  fun getDefinitionName (Definition { region = _, modifiers = _, defType = _, typeParameters = _, valueParameters = _, typeExpr = _, name = n, body = _ }) = n
  fun getDefinitionModifiers (Definition { region = _, modifiers = m, defType = _, typeParameters = _, valueParameters = _, typeExpr = _, name = _, body = _ }) = m
  fun getDefinitionTypeExpr (Definition { region = _, modifiers = _, defType = _, typeParameters = _, valueParameters = _, typeExpr = t, name = _, body = _ }) = t
  fun getDefinitionTypeParameters (Definition { region = _, modifiers = _, defType = _, typeParameters = t, valueParameters = _, typeExpr = _, name = _, body = _ }) = t
  fun getDefinitionValueParameters (Definition { region = _, modifiers = _, defType = _, typeParameters = _, valueParameters = v, typeExpr = _, name = _, body = _ }) = v
  fun getDefinitionBody (Definition { region = _, modifiers = _, defType = _, typeParameters = _, valueParameters = _, typeExpr = _, name = _, body = b }) = b

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
      fun seq sep func [] = ()
        | seq sep func (x::[]) = func x
        | seq sep func (x::rest) = (func x; put sep; seq sep func rest)

      fun id (Identifier { region = _, text = t }) = put t

      fun operator AND = put "AND"
        | operator OR = put "OR"
        | operator TYPE_APPLY = put "TYPE_APPLY"
        | operator APPLY = put "APPLY"
        | operator TUPLE = put "TUPLE"
        | operator PLUS = put "PLUS"
        | operator MINUS = put "MINUS"
        | operator MUL = put "MUL"
        | operator DIV = put "DIV"
        | operator MOD = put "MOD"
        | operator DOT = put "DOT"
        | operator NOT = put "NOT"
        | operator EQ = put "EQ"
        | operator NEQ = put "NEQ"
        | operator LT = put "LT"
        | operator GT = put "GT"
        | operator LTEQ = put "LTEQ"
        | operator GTEQ = put "GTEQ"
        | operator MUTABLE = put "MUTABLE"
        | operator IMMUTABLE = put "IMMUTABLE"

      fun expression (Name (n)) = id n
        | expression (Nothing { region = _ }) = put "()"
        | expression (Integer { region = _, value = i }) = put (Int.toString i)
        | expression (Float { region = _, value = f }) = put (Real.toString f)
        | expression (String { region = _, value = s }) = (put "\""; put s; put "\"")
        | expression (Binary { region = _, operator = oper, left = l, right = r }) = (put "BINARY{o="; operator oper; put ",l="; expression l; put ",r="; expression r; put "}")
        | expression (Unary { region = _, operator = oper, expr = e }) = (put "UNARY{o="; operator oper; put ",e="; expression e; put "}")

      fun expressionOpt (NONE) = ()
        | expressionOpt (SOME e) = expression e

      fun caseBlock (Case { region = _, values = v, body = b }) = (put "CASE("; seq "," (fn e => expression e) v; put "{"; seq ";" (fn s => statement s) b; put "}")

      and statement (Return { region = _, value = NONE }) = put "RETURN"
        | statement (Return { region = _, value = SOME e }) = (put "RETURN{e="; expression e; put "}")
        | statement (If { region = _, condition = e, trueBranch = t, falseBranch = NONE }) = (put "IF("; expression e; put ","; statement t; put ")")
        | statement (If { region = _, condition = e, trueBranch = t, falseBranch = SOME f }) = (put "IF("; expression e; put ","; statement t; put ","; statement f; put ")")
        | statement (Switch { region = _, value = e, branches = l }) = (put "SWITCH("; expression e; put "){"; seq ";" (fn b => caseBlock b) l; put "}")
        | statement (Variable d) = (put "LOCAL{d="; definition d; put "}")
        | statement (ExpressionStatement e) = (put "EXPR{e="; expression e; put "}")
        | statement (BlockStatement l) = (put "BLOCK{"; map (fn s => (statement s; put "\n")) l; put "}")

      and exprOrStatement (NONE) = ()
        | exprOrStatement (SOME (Expression (e))) = expression e
        | exprOrStatement (SOME (Statement (l))) =
          let
            fun print [] = ()
              | print (s::rest) = (statement s; print rest)
          in
            put "[";
            increaseIndentation;
            print l;
            decreaseIndentation;
            put "]"
          end

      and modifier (Abstract (_)) = "ABSTRACT"
        | modifier (Immutable (_)) = "IMMUTABLE"
        | modifier (Mutable (_)) = "MUTABLE"
        | modifier (Builtin (_)) = "BUILTIN"

      and modifierList [] = ()
        | modifierList (m::[]) = put (modifier m)
        | modifierList (m::rest) = (put (modifier m); put ","; modifierList rest)

      and definitionType Type = "TYPE"
        | definitionType Method = "METHOD"
        | definitionType Field = "FIELD"
        | definitionType Object = "OBJECT"
        | definitionType Local = "VAR"

      and definition (Definition { defType = t, region = _, modifiers = m, typeParameters = tp, valueParameters = vp, typeExpr = e, name = n, body = b }) =
        (put "DEF{";
         put "name=";
         case n
           of SOME n => id n;
         put ",defType=";
         put (definitionType t);
         put ",modifiers=";
         modifierList m;
         put ",type=";
         expressionOpt e;
         put ",typeParms=";
         definitionList tp;
         put ",valueParms=";
         definitionList vp;
         put ",body=";
         exprOrStatement b;
         put "}\n")

      and definitionList [] = ()
        | definitionList (d::rest) = (definition d; indent (); definitionList rest)

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

