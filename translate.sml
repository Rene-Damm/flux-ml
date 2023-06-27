structure Translate =
struct

  (* refer to 'levels' as 'scopes' instead *)

  exception Argh of string

  datatype fragment =
      PROC of { label: Temp.label, body: Tree.stmt, epiloque: Temp.label, returnValue: Temp.label option, frame: Arch.frame }
    | STRING of Temp.label * string

  datatype variable = VAR of { temp: Temp.temp }

  (* Translates a *SINGLE* AST into a list of fragments.
   * No support for translating a program with multiple units. *)
  fun translateProgram program =
  let 

    fun createDefinitionSymbol def = Symbol.create (AST.getIdentifierText (AST.getDefinitionName def))

    (**********************************************************************************************)
    exception StringTableException
    val stringTable = HashTable.mkTable (HashString.hashString, op =) (100, StringTableException)

    fun internStringLiteral text =
      case HashTable.find stringTable text
        of SOME (STRING (label, _)) => label
         | _ =>
             let
               val label = Temp.newNamedLabel "string"
               val fragment = STRING (label, text)
             in
               HashTable.insert stringTable (text, fragment);
               label
             end

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
            of AST.AND => (Types.UnionType (leftType, rightType), tenv2)
             | AST.OR => (Types.IntersectionType (leftType, rightType), tenv2)
             | AST.APPLY => (Types.InstancedType (leftType, rightType), tenv2)
             | _ => raise Utils.NotImplemented
        end
      | translateTypeExpr_ _ _ = raise Argh("Invalid type expr")

    val translateTypeExpr = translateTypeExpr_ (fn (tenv, symbol) => (Option.valOf (Symbol.lookup (tenv, symbol)), tenv))
    
    (**********************************************************************************************)
    fun collectTypeDefinition (table, def) =
      let
        val name = createDefinitionSymbol def
      in
        case Symbol.lookup (table, name)
          of SOME _ => raise Argh("already defined") (****TODO: error, already defined *)
           | NONE => Symbol.enter (table, name, def)
      end

    (**********************************************************************************************)
    (* First pass just puts type *definitions* in a table, i.e. the definition ASTs themselves. *)
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
    (* Second pass generates types for all definitions (and their respective expressions) that have been gathered.
     * This way we can handle even type aliases easily. *)
    fun generateTypes defTable =
      let

        fun createTupleType func tenv (v::[]) = func (v, tenv)
          | createTupleType func tenv (v::rest) =
              let
                val (left, tenv') = func (v, tenv)
                val (right, tenv'') = createTupleType func tenv' rest
              in
                (Types.TupleType (left, right), tenv'')
              end
          | createTupleType _ _ [] = raise Utils.ShouldNotGetHere

        fun translateExpr (exp, tenv) = translateTypeExpr_ (fn (tenv_, symbol) =>
              case Symbol.lookup (tenv_, symbol)
                of SOME t => (t, tenv_)
                 | NONE => genType (symbol, tenv_)) (exp, tenv)

        and genTypeForTypeParameter (argDef, tenv) =
          let
            val name = createDefinitionSymbol argDef
          in
            case AST.getDefinitionTypeExpr argDef
              of NONE => (case Symbol.lookup (tenv, name)
                            of SOME t => (Types.VariableType (name, t), tenv)
                             | NONE =>
                                let
                                  val (t, tenv') = genType (name, tenv)
                                in
                                  (Types.VariableType (name, t), tenv')
                                end)
               | SOME e =>
                  let
                    val (t, tenv') = translateExpr (e, tenv)
                  in
                    (Types.VariableType (name, t), tenv')
                  end
          end

        and genType (symbol, tenv) =
          let
            val definition = Option.valOf (Symbol.lookup (defTable, symbol))
            val body = AST.getDefinitionBody definition
            val typeExpr = AST.getDefinitionTypeExpr definition

            (*TODO: need to allow type parameters to shadow definitions in global scope *)
            val (typeParameters, _) = case AST.getDefinitionTypeParameters definition
                                            of [] => (NONE, tenv)
                                             | l => let val (t, tenv') = createTupleType genTypeForTypeParameter tenv l in (SOME t, tenv') end

            fun genDerivedType (expr) = let val (baseType, tenv') = translateExpr (expr, tenv)
                                        in (Types.DerivedType(symbol, baseType), tenv') end
            fun genTypeDerivedFromObject () = let val (objType, tenv') = genType (Env.builtinObject, tenv)
                                              in (Types.DerivedType(symbol, objType), tenv') end

            val (ty, tenv') = case (body, typeExpr)
                                of (SOME (AST.Expression(e)), NONE) => translateExpr (e, tenv) (* alias type *)
                                 | (NONE, SOME e) => genDerivedType e
                                 | (NONE, NONE) => if Symbol.isSame (symbol, Env.builtinObject)
                                                   then (Types.RootType, tenv)
                                                   else genTypeDerivedFromObject ()
                                 | _ => raise Argh("invalid type definition")

            val ty' = case typeParameters
                        of SOME t => Types.ParameterizedType (ty, t)
                         | NONE => ty

          in
            case Symbol.lookup (tenv, symbol)
              of NONE => let val tenv'' = Symbol.enter (tenv', symbol, ty')
                         in (ty', tenv'') end
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
    fun collectMethod (fenv, tenv, def) =
      let
        val name = createDefinitionSymbol def
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
          | genArgType (argDef::rest) = Types.TupleType (genTypeForArgument argDef, genArgType rest)
          | genArgType [] = raise Utils.ShouldNotGetHere

        val argType = case valueParms
                        of [] => nothingType
                         | argDef::[] => genTypeForArgument argDef
                         | argDef::rest => Types.TupleType (genTypeForArgument argDef, genArgType rest)

        val resultType = case (AST.getDefinitionTypeExpr def)
                           of SOME e => let val (t, _) = translateTypeExpr (e, tenv) in t end
                            | NONE => nothingType

        val methodType = Types.MethodType (argType, resultType)

        val label = Temp.newNamedLabel (Symbol.toString name)

      in
        case Symbol.lookup (fenv, name)
          of SOME (_, dispatchTree) => Symbol.enter (fenv, name, (name, Functions.insertDispatchNode (dispatchTree, methodType, label, def)))
           | NONE => Symbol.enter(fenv, name, (name, Functions.createDispatchTree (methodType, label, def)))
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

    fun lookupBuiltinType name =
      case Symbol.lookup (tenv, name)
        of SOME t => t
         | NONE => raise Argh("builtin type not found")

    val intType = lookupBuiltinType Env.builtinInteger
    val floatType = lookupBuiltinType Env.builtinFloat
    val stringType = lookupBuiltinType Env.builtinString
    val nothingType = lookupBuiltinType Env.builtinNothing

    (* Collect all functions and arrange the methods in them into dispatch trees. *)
    val fenv = collectFunctions (Symbol.emptyTable, tenv, program)

    (**********************************************************************************************)
    fun translateFunction (_, (name, dispatchTree)) =
      let

        fun processNode (Functions.DispatchNode { methodType = methodType, children = children, label = label, method = method }) =
          let 

            val methodResultType = (Types.getRightOperandType methodType)
            val hasReturnValue = not (Types.isSubtype (methodResultType, nothingType))

            val epiloqueLabel = Temp.newNamedLabel ((Symbol.toString name) ^ "_epilogue")
            val returnValueLabel = Temp.newTemp ()

            (* Translates an AST.expression into a tree and returns that and the Types.ty of the expression. *)
            fun translateExpr venv (AST.Integer { region = _, value = v }) = (Tree.INT v, intType)
              | translateExpr venv (AST.Float { region = _, value = v }) = (Tree.FLOAT v, floatType)
              | translateExpr venv (AST.String { region = _, value = v }) =
                  let
                    val label = internStringLiteral v
                  in
                    (Tree.TEMP label, stringType)
                  end
              | translateExpr venv (AST.Binary { region = _, operator = operator, left = left, right = right }) = raise Utils.NotImplemented
              | translateExpr venv (AST.Name id) =
                  let
                    val symbol = Symbol.create (AST.getIdentifierText id)
                  in
                    case Symbol.lookup (venv, symbol)
                      of SOME s => raise Utils.NotImplemented
                       | NONE => (print (Symbol.toString symbol); raise Utils.NotImplemented)
                  end

            (*TODO: allow statements to modify venv *)
            fun translateStmt venv (AST.Return { region = _, value = v }) =
                  let
                    val (exp, ty) = case v
                                      of SOME e => let val (e, t) = translateExpr venv e in (SOME e, t) end
                                       | NONE => (NONE, nothingType)
                  in
                    if not (Types.isSubtype (ty, methodResultType))
                    then raise Argh ("Incorrect return type")
                    else
                      case exp
                        of SOME e => Tree.SEQ (Tree.MOVE (Tree.TEMP returnValueLabel, e), Tree.JUMP epiloqueLabel)
                         | NONE => Tree.JUMP epiloqueLabel
                  end
              | translateStmt venv _ = raise Utils.NotImplemented

            fun translateStmts venv [] = raise Utils.ShouldNotGetHere
              | translateStmts venv (stmt::[]) = translateStmt venv stmt
              (*TODO: this should unfold nested SEQs*)
              | translateStmts venv (stmt::rest) = Tree.SEQ (translateStmt venv stmt, translateStmts venv rest)

            (* There are no global values so every method gets a fresh value environment. *)
            (*TODO: put predefined values like `argument` in the env *)
            val venvInitial = Symbol.emptyTable

            fun getArgFormat (Types.DerivedType (name, _)) =
                  if Symbol.isSame (name, Env.builtinInteger) then Arch.Int32
                  else if Symbol.isSame (name, Env.builtinFloat) then Arch.Float32
                  else Arch.Address
              | getArgFormat _ = Arch.Address

            fun getArgFormats (Types.TupleType (left, right)) = (getArgFormat left)::(getArgFormats right)
              | getArgFormats t = [getArgFormat t]

            val argFormats = getArgFormats (Types.getLeftOperandType methodType)

            val label = Temp.newNamedLabel (Symbol.toString name)
            (*REVIEW: Isn't it too early to bring in arch-dependencies? Shouldn't this be restricted to the backend? *)
            val (frame, valueArgsAccess) = Arch.newFrame (label, argFormats)

            fun processValueArgs ([], venv) = venv
              | processValueArgs (argDef::rest, venv) =
                let
                  val argName = createDefinitionSymbol argDef
                  (* For now, add all args as temps just like any other variable. *)
                  val venv' = Symbol.enter (venv, argName, Tree.TEMP (Temp.newTemp ()))
                in
                  processValueArgs (rest, venv')
                end

            val venv' = processValueArgs ((AST.getDefinitionValueParameters method), venvInitial)

            val body =
              case (AST.getDefinitionBody method)
                of SOME (AST.Expression(e)) => raise Utils.NotImplemented
                 | SOME (AST.Statement([])) => Tree.NOP
                 | SOME (AST.Statement(l)) => translateStmts venv' l
                 | _ => Tree.NOP

          in
            PROC { label = label, body = body, frame = frame, epiloque = epiloqueLabel, returnValue = if hasReturnValue then SOME returnValueLabel else NONE }::(List.concat (map processNode children))
          end
      in
        processNode dispatchTree
      end

    val progFragments = List.concat (map translateFunction (Symbol.all fenv))
    val stringFragments = HashTable.listItems stringTable

  in
    Env.dumpTypes (tenv, TextIO.stdOut);
    Env.dumpFunctions (fenv, TextIO.stdOut);

    List.concat [progFragments, stringFragments]
  end

end

