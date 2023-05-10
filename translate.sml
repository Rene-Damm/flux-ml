structure Translate =
struct

  exception Argh of string

  fun translateProgram program =
  let 

    (*
    fun typeModifiers [] = []
      | typeModifiers (AST.Abstract(_)::rest) = Types.Abstract::(typeModifiers rest)
      | typeModifiers (AST.Immutable(_)::rest) = Types.Immutable::(typeModifiers rest)
      | typeModifiers (AST.Mutable(_)::rest) = Types.Mutable::(typeModifiers rest)
      | typeModifiers (AST.Builtin(_)::rest) = typeModifiers rest
      *)

    fun collectTypeDefinition (table, def) =
    let
      val name = Symbol.create (AST.getIdentifierText (AST.getDefinitionName def))
    in
      case Symbol.lookup (table, name)
        of SOME _ => raise Argh("already defined") (****TODO: error, already defined *)
         | NONE => Symbol.enter (table, name, def)
    end

    fun collectTypeDefinitions (table, []) = table
      | collectTypeDefinitions (table, definition::rest) =
        let
          val newTable = case (AST.getDefinitionType definition)
                          of AST.Type => collectTypeDefinition (table, definition)
                           | AST.Object => collectTypeDefinition (table, definition)
                           | _ => table
        in
          collectTypeDefinitions (newTable, rest)
        end

    fun generateTypes defTable =
    let

      fun translateExpr (AST.Name(id), tenv) =
          let
            val symbol = Symbol.create (AST.getIdentifierText id)
          in
            case Symbol.lookup (tenv, symbol)
              of SOME t => (t, tenv)
               | NONE => genType (symbol, tenv)
          end
        | translateExpr _ = raise Argh("Invalid type expr")

      and genType (symbol, tenv) =
      let
        val definition = Option.valOf (Symbol.lookup (defTable, symbol))
        val body = AST.getDefinitionBody definition
        val typeExpr = AST.getDefinitionTypeExpr definition

        fun genDerivedType (expr) = let val (baseType, newTenv) = translateExpr (expr, tenv)
                                    in (Types.DerivedType(symbol, baseType), newTenv) end
        fun genTypeDerivedFromObject () = let val (objType, tenv_) = genType (Env.builtinObject, tenv)
                                          in (Types.DerivedType(symbol, objType), tenv_) end

        val (ty, newTenv) = case (body, typeExpr)
                              of (SOME (AST.Expression(e)), NONE) => translateExpr (e, tenv) (* alias type *)
                               | (NONE, SOME e) => genDerivedType e
                               | (NONE, NONE) => if Symbol.isSame (symbol, Env.builtinObject)
                                                 then (Types.RootType, tenv)
                                                 else genTypeDerivedFromObject ()
                               | _ => raise Argh("invalid type definition")

      in
        case Symbol.lookup (tenv, symbol)
          of NONE => let val newTenv_ = Symbol.enter (newTenv, symbol, ty)
                     in (ty, newTenv_) end
           | SOME t => (t, tenv) (* Already generated *)
      end

      fun genTypes ([], tenv) = tenv
        | genTypes ((s, _)::rest, tenv) =
            let val (_, newTenv) = genType (s, tenv)
            in genTypes(rest, newTenv) end

    in
      genTypes ((Symbol.all defTable), Symbol.emptyTable)
    end

    (****should this also just collect ASTs only?*)
    fun collectMethod (fenv, def) =
    let
      val name = Symbol.create (AST.getIdentifierText (AST.getDefinitionName def))
    in
      case Symbol.lookup (fenv, name)
        of SOME (_, methods) => Symbol.enter (fenv, name, (name, ()::methods))
         | NONE => Symbol.enter(fenv, name, (name, []))
    end

    fun collectFunctions (fenv, []) = fenv
      | collectFunctions (fenv, definition::rest) =
        let
          val newFenv = case (AST.getDefinitionType definition)
                          of AST.Method => collectMethod (fenv, definition)
                           | _ => fenv
        in
          collectFunctions (newFenv, rest)
        end

    (* Put all type/object definition ASTs into a table and then
       generate types for them. *)
    val typeDefs = collectTypeDefinitions (Symbol.emptyTable, program)
    val tenv = generateTypes typeDefs

    val fenv = collectFunctions (Symbol.emptyTable, program)

  in
    Env.dumpTypes (tenv, TextIO.stdOut);
    Env.dumpFunctions (fenv, TextIO.stdOut)
  end

end

