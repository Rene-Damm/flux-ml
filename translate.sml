structure Translate =
struct

  (* refer to 'levels' as 'scopes' instead *)

  exception Argh of string

  datatype fragment =
      PROC of { label: Temp.label, body: Tree.stmt, epiloque: Temp.label, returnValue: Temp.label option, frame: Arch.frame }
    | STRING of Temp.label * string

  datatype variable = VAR of { location: Tree.expr, typ: Types.ty }

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
      | translateTypeExpr_ lookupSymbolFn (AST.Unary { region = _, operator = oper, expr = expr }, tenv) =
        let
          val (ty, tenv') = translateTypeExpr_ lookupSymbolFn (expr, tenv)
        in
          case oper
            of AST.MUTABLE => (Types.MutableType ty, tenv')
             | AST.IMMUTABLE => (Types.ImmutableType ty, tenv')
             | _ => raise Utils.ShouldNotGetHere
        end
      | translateTypeExpr_ _ _ = raise Argh("Invalid type expr")

    val translateTypeExpr = translateTypeExpr_ (fn (tenv, symbol) => (Utils.valOf "translateTypeExpr_" (Symbol.lookup (tenv, symbol)), tenv))
    
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
            val definition = Utils.valOf "genType" (Symbol.lookup (defTable, symbol))
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
        val nothingType = Utils.valOf "collectMethod" (Symbol.lookup (tenv, Env.builtinNothing))

        val valueParms = AST.getDefinitionValueParameters def
        (****TODO handle type parameters *)
        val typeParms = AST.getDefinitionTypeParameters def

        (****TODO proper typing of args *) (*<----------------------*)
        fun genTypeForArgument argDef =
          let
            val id = AST.getDefinitionName argDef
            val sym = Symbol.create (AST.getIdentifierText id)
            val typeExpr = AST.getDefinitionTypeExpr argDef
          in
            case typeExpr
              of SOME e => let val (t, _) = translateTypeExpr (e, tenv) in t end
               | NONE => Utils.valOf "genTypeForArgument" (Symbol.lookup (tenv, sym))
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

    (**********************************************************************************************)
    fun generateSingletons ([], tenv) = Symbol.emptyTable
      | generateSingletons (ast::rest, tenv) =
          let
            val venv = generateSingletons (rest, tenv)
          in
            case AST.getDefinitionType ast
              of AST.Object =>
                  let 
                    val name = createDefinitionSymbol ast
                    val ty = Utils.valOf "generateSingletons" (Symbol.lookup (tenv, name))
                    val tree =
                      if Symbol.isSame (name, Env.builtinTrue)
                      then Tree.TRUE
                      else if Symbol.isSame (name, Env.builtinFalse)
                      then Tree.FALSE
                      else if Symbol.isSame (name, Env.builtinNothing)
                      then Tree.NOTHING
                      else raise Utils.NotImplemented
                    val venv' = Symbol.enter (venv, name, VAR { location = tree, typ = ty })
                  in
                    venv'
                  end
               | _ => venv
          end

    (* Put all type/object definition ASTs into a table and then
       generate types for them. *)
    val typeDefs = collectTypeDefinitions (Symbol.emptyTable, program)
    val tenv = generateTypes typeDefs

    (* Put all OBJECT definitions in a table as variables. *)
    val venvGlobal = generateSingletons (Symbol.entries typeDefs, tenv)

    fun lookupBuiltinType name =
      case Symbol.lookup (tenv, name)
        of SOME t => t
         | NONE => raise Argh("builtin type not found")

    fun lookupBuiltinValue name =
      case Symbol.lookup (venvGlobal, name)
        of SOME (VAR { location = l, typ = _ }) => l
         | NONE => raise Argh("builtin value not found")

    val intType = lookupBuiltinType Env.builtinInteger
    val floatType = lookupBuiltinType Env.builtinFloat
    val stringType = lookupBuiltinType Env.builtinString
    val nothingType = lookupBuiltinType Env.builtinNothing

    fun isBuiltinNumericType t = Types.isSubtype (t, intType) orelse Types.isSubtype (t, floatType)

    val trueValue = lookupBuiltinValue Env.builtinTrue
    val nothingValue = lookupBuiltinValue Env.builtinNothing
    val falseValue = lookupBuiltinValue Env.builtinFalse

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
              | translateExpr venv (AST.Binary { region = _, operator = AST.APPLY, left = left, right = right }) =
                  let
                    val (rightTree, rightType) =
                      case right
                        of AST.Binary { region = _, operator = AST.TUPLE, left = l, right = r } => raise Utils.NotImplemented
                         | _ => let val (tree, ty) = translateExpr venv right in ([tree], ty) end
                  in
                    case left
                      of AST.Name id =>
                           let
                             val (_, dispatchTree) = case Symbol.lookup (fenv, (Symbol.create (AST.getIdentifierText id)))
                                                       of SOME node => node
                                                        | NONE => raise Utils.NotImplemented
                             val dispatchNode = case Functions.lookupDispatchNode (dispatchTree, rightType)
                                                  of SOME node => node
                                                   | NONE => raise Utils.NotImplemented
                             val resultType = Types.getRightOperandType (Functions.getDispatchNodeType dispatchNode)
                           in
                             (Tree.CALL (Functions.getDispatchNodeLabel dispatchNode, rightTree), resultType)
                           end
                       | expr as AST.Binary { region = _, operator = AST.DOT, left = _, right = _ } => raise Utils.NotImplemented
                       | _ => raise Utils.ShouldNotGetHere
                  end
              | translateExpr venv (AST.Binary { region = _, operator = operator, left = left, right = right }) =
                  let
                    val (leftTree, leftType) = translateExpr venv left
                    val (rightTree, rightType) = translateExpr venv right
                  in
                    case operator
                      of AST.TUPLE => raise Utils.NotImplemented
                       | _ =>
                           if (isBuiltinNumericType leftType) andalso (isBuiltinNumericType rightType)
                          then
                            case operator
                              of _ => raise Utils.NotImplemented (*TODO replace call with direct expression tree*)
                          else
                            let 
                              val functionName = case operator
                                                   of AST.PLUS => Env.builtinPlus
                                                    | _ => raise Utils.NotImplemented
                              val function = case Symbol.lookup (fenv, functionName)
                                               of SOME (_, f) => f
                                                | _ => raise Argh ("undefined operator")
                              val node = Functions.lookupDispatchNode (function, Types.TupleType (leftType, rightType))
                            in
                              case node
                                of SOME (Functions.DispatchNode { methodType = methodType, children = _, label = label, method = _ }) =>
                                    (Tree.CALL (label, [leftTree, rightTree]), Types.getRightOperandType methodType)
                                 | NONE => raise Argh ("no matching implementation for operator")
                            end
                  end
              | translateExpr venv (AST.Name id) =
                  let
                    val symbol = Symbol.create (AST.getIdentifierText id)
                  in
                    (* Try value first *)
                    case Symbol.lookup (venv, symbol)
                      of SOME (VAR { location = expr, typ = ty }) => (expr, ty)
                       | NONE =>
                           (* No value, try function *)
                           case Symbol.lookup (fenv, symbol)
                             of SOME dispachNode => raise Utils.NotImplemented (*TODO: create closure *)
                              | NONE => (print (Symbol.toString symbol); raise Utils.NotImplemented)
                  end
              | translateExpr venv (AST.Nothing { region = _ }) = (nothingValue, nothingType)
              | translateExpr venv _ = raise Utils.NotImplemented

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
              | translateStmt venv (AST.ExpressionStatement(expr)) =
                  let
                    val (tree, typ) = translateExpr venv expr
                  in
                    Tree.EXPR (tree)
                  end
              | translateStmt venv _ = raise Utils.NotImplemented

            fun translateStmts venv [] = raise Utils.ShouldNotGetHere
              | translateStmts venv (stmt::[]) = translateStmt venv stmt
              (*TODO: this should unfold nested SEQs*)
              | translateStmts venv (stmt::rest) = Tree.SEQ (translateStmt venv stmt, translateStmts venv rest)

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

            fun processValueArgs ([], _, venv) = venv
              | processValueArgs (argDef::rest, argType::restTypes, venv) =
                let
                  val argName = createDefinitionSymbol argDef
                  (* For now, add all args as temps just like any other variable. *)
                  val venv' = Symbol.enter (venv, argName, VAR { location = Tree.TEMP (Temp.newTemp ()), typ = argType })
                in
                  processValueArgs (rest, restTypes, venv')
                end
              | processValueArgs (_, _, _) = raise Utils.ShouldNotGetHere

            val venv' = processValueArgs ((AST.getDefinitionValueParameters method), (Types.toList (Types.getLeftOperandType methodType)), venvGlobal)

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

