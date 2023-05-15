structure Translate =
struct

  exception Argh of string

  fun translateProgram program =
  let 

    (**********************************************************************************************)
    fun translateTypeExpr_ lookupSymbolFn (AST.Name(id), tenv) =
        let
          val symbol = Symbol.create (AST.getIdentifierText id)
        in
          lookupSymbolFn (tenv, symbol)
        end
      | translateTypeExpr_ lookupSymbolFn (AST.Binary { region = _, operator = oper, left = left, right = right }, tenv) =
        let
          val (leftType, tenv1) = translateTypeExpr_ lookupSymbolFn (left, tenv)
          val (rightType, tenv2) = translateTypeExpr_ lookupSymbolFn (right, tenv1)
        in
          case oper
            of AST.AND => (Types.UnionType(leftType, rightType), tenv2)
             | AST.OR => (Types.IntersectionType(leftType, rightType), tenv2)
        end
      | translateTypeExpr_ _ _ = raise Argh("Invalid type expr")

    val translateTypeExpr = translateTypeExpr_ (fn (tenv, symbol) => (Option.valOf (Symbol.lookup (tenv, symbol)), tenv))
    
    (**********************************************************************************************)
    fun collectTypeDefinition (table, def) =
    let
      val name = Symbol.create (AST.getIdentifierText (AST.getDefinitionName def))
    in
      case Symbol.lookup (table, name)
        of SOME _ => raise Argh("already defined") (****TODO: error, already defined *)
         | NONE => Symbol.enter (table, name, def)
    end

    (**********************************************************************************************)
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

    (**********************************************************************************************)
    fun generateTypes defTable =
    let

      fun translateExpr (exp, tenv)  = translateTypeExpr_ (fn (tenv_, symbol) =>
            case Symbol.lookup (tenv_, symbol)
              of SOME t => (t, tenv_)
               | NONE => genType (symbol, tenv_)) (exp, tenv)

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

    (**********************************************************************************************)
    (****should this also just collect ASTs only?*)
    fun collectMethod (fenv, tenv, def) =
    let
      val name = Symbol.create (AST.getIdentifierText (AST.getDefinitionName def))
      val nothingType = Option.valOf (Symbol.lookup (tenv, Env.builtinNothing))

      val valueParms = AST.getDefinitionValueParameters def
      (****TODO handle type parameters *)
      val typeParms = AST.getDefinitionTypeParameters def

      (****TODO proper typing of args *)
      fun genTypeForArgument argDef =
        let
          val id = AST.getDefinitionName argDef
          val sym = Symbol.create (AST.getIdentifierText id)
        in
          Option.valOf (Symbol.lookup (tenv, sym))
        end

      fun genArgType (argDef::[]) = genTypeForArgument argDef
        | genArgType (argDef::rest) = Types.TupleType (genArgType rest, genTypeForArgument argDef)
        | genArgType [] = raise Utils.ShouldNotGetHere

      val argType = case valueParms
                      of [] => nothingType
                       | argDef::[] => genTypeForArgument argDef
                       | argDef::rest => Types.TupleType (genArgType rest, genTypeForArgument argDef)

      val resultType = case (AST.getDefinitionTypeExpr def)
                         of SOME e => let val (t, _) = translateTypeExpr (e, tenv) in t end
                          | NONE => nothingType

      val methodType = Types.MethodType (argType, resultType)

    in
      case Symbol.lookup (fenv, name)
        of SOME (_, dispatchTree) => Symbol.enter (fenv, name, (name, Functions.insertDispatchNode (dispatchTree, methodType, ())))
         | NONE => Symbol.enter(fenv, name, (name, Functions.createDispatchTree (methodType, ())))
    end

    (**********************************************************************************************)
    fun collectFunctions (fenv, tenv, []) = fenv
      | collectFunctions (fenv, tenv, definition::rest) =
        let
          val newFenv = case (AST.getDefinitionType definition)
                          of AST.Method => collectMethod (fenv, tenv, definition)
                           | _ => fenv
        in
          collectFunctions (newFenv, tenv, rest)
        end

    (* Put all type/object definition ASTs into a table and then
       generate types for them. *)
    val typeDefs = collectTypeDefinitions (Symbol.emptyTable, program)
    val tenv = generateTypes typeDefs

    val fenv = collectFunctions (Symbol.emptyTable, tenv, program)

  in
    Env.dumpTypes (tenv, TextIO.stdOut);
    Env.dumpFunctions (fenv, TextIO.stdOut)
  end

end

