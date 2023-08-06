structure Translate =
struct

  (* refer to 'levels' as 'scopes' instead *)

  exception Argh of string

  datatype fragment =
      PROC of { label: Temp.label, body: Tree.stmt, epiloque: Temp.label, returnValue: Temp.label option, frame: Arch.frame }
    | STRING of Temp.label * string

  datatype variable = VAR of { location: Tree.expr, typ: Types.ty }

  datatype method = METHOD of { definition: AST.definition, builtin: bool, venv: variable Symbol.table, tenv: Types.ty Symbol.table }

  (* Translates a *SINGLE* AST into a list of fragments.
   * No support for translating a program with multiple units. *)
  fun translateProgram program =
  let 

    fun createDefinitionSymbol def = Symbol.create (AST.getIdentifierText (Utils.valOf "createDefinitionSymbol" (AST.getDefinitionName def)))

    (**********************************************************************************************)
    (* String literals *)

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
    (* Translate type expressions *)

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
             | AST.TYPE_APPLY => (Types.InstancedType (leftType, rightType), tenv2)
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
    (* First pass just puts type *definitions* in a table, i.e. the definition ASTs themselves. *)

    fun collectTypeDefinition (def, table) =
      let
        val name = createDefinitionSymbol def
      in
        case Symbol.lookup (table, name)
          of SOME _ => raise Argh ("already defined") (****TODO: error, already defined *)
           | NONE => Symbol.enter (table, name, def)
      end

    fun collectTypeDefinitions (table, []) = table
      | collectTypeDefinitions (table, definition::rest) =
        let
          val newTable = case (AST.getDefinitionType definition)
                          of AST.Type => collectTypeDefinition (definition, table)
                           | AST.Object => collectTypeDefinition (definition, table)
                           | _ => table
        in
          collectTypeDefinitions (newTable, rest)
        end

    (**********************************************************************************************)
    (* Second pass generates types for all definitions (and their respective expressions) that have been gathered.
     * This way we can handle even type aliases easily. *)

    fun generateTypes (defTable, typeTable) =
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
                                (*REVIEW: This handling of aliases will likely not be enough to deal with aliases that have type parameters *)
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
        genTypes ((Symbol.all defTable), typeTable)
      end

    (**********************************************************************************************)
    (* Global type env *)

    (* Put all type/object definition ASTs into a table and then
       generate types for them. *)
    val typeDefs = collectTypeDefinitions (Symbol.emptyTable, program)
    val tenv = generateTypes (typeDefs, Symbol.emptyTable)

    fun lookupBuiltinType name =
      case Symbol.lookup (tenv, name)
        of SOME t => t
         | NONE => raise Argh("builtin type not found")

    val intType = lookupBuiltinType Env.builtinInteger
    val floatType = lookupBuiltinType Env.builtinFloat
    val stringType = lookupBuiltinType Env.builtinString
    val nothingType = lookupBuiltinType Env.builtinNothing

    fun isBuiltinNumericType t = Types.isSubtype (t, intType) orelse Types.isSubtype (t, floatType)

    (**********************************************************************************************)
    (* Global value env *)

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

    (* Put all OBJECT definitions in a table as variables. *)
    val venvGlobal = generateSingletons (Symbol.entries typeDefs, tenv)

    fun lookupBuiltinValue name =
      case Symbol.lookup (venvGlobal, name)
        of SOME (VAR { location = l, typ = _ }) => l
         | NONE => raise Argh("builtin value not found")

    val trueValue = lookupBuiltinValue Env.builtinTrue
    val nothingValue = lookupBuiltinValue Env.builtinNothing
    val falseValue = lookupBuiltinValue Env.builtinFalse

    (**********************************************************************************************)
    (* Global function env *)

    fun collectMethod (fenv, tenv, def) =
      let
        val name = createDefinitionSymbol def

        fun inferName (AST.Name id) = AST.getIdentifierText id
          | inferName _ = raise Utils.NotImplemented

        val valueArgs = AST.getDefinitionValueParameters def
        val typeArgs = AST.getDefinitionTypeParameters def

        (*TODO: type args should be processed in parallel just like global type definitions; not one after the other *)

        fun processTypeArg tenv argDef =
          let
            val id = AST.getDefinitionName argDef
            val typeExpr = AST.getDefinitionTypeExpr argDef
            
            val sym = Symbol.create (case id
                                       of SOME id => AST.getIdentifierText id
                                        | NONE => inferName (Utils.valOf "processTypeArg" typeExpr))

            val (ty, tenv') = 
              case typeExpr
                of SOME e => translateTypeExpr (e, tenv)
                 | NONE => (Utils.valOf "genTypeForArgument" (Symbol.lookup (tenv, sym)), tenv)

          in
            (ty, Symbol.enter (tenv', sym, ty))
          end

        fun processValueArg venv tenv argDef =
          let
            val id = AST.getDefinitionName argDef
            val typeExpr = AST.getDefinitionTypeExpr argDef
            
            val sym = Symbol.create (case id
                                       of SOME id => AST.getIdentifierText id
                                        | NONE => inferName (Utils.valOf "processValueArg" typeExpr))

            val ty = 
              case typeExpr
                of SOME e => let val (t, _) = translateTypeExpr (e, tenv) in t end
                 | NONE => Utils.valOf "genTypeForArgument" (Symbol.lookup (tenv, sym))

            val var = VAR { location = Tree.TEMP (Temp.newTemp ()), typ = ty }
          in
            (ty, Symbol.enter (venv, sym, var))
          end

        fun processTypeArgs tenv (argDef::[]) = processTypeArg tenv argDef
          | processTypeArgs tenv (argDef::rest) =
              let
                val (left, tenv') = processTypeArg tenv argDef
                val (right, tenv'') = processTypeArgs tenv' rest
              in
                (Types.TupleType (left, right), tenv'')
              end
          | processTypeArgs tenv [] = raise Utils.ShouldNotGetHere
          
        fun processValueArgs venv tenv (argDef::[]) = processValueArg venv tenv argDef
          | processValueArgs venv tenv (argDef::rest) =
              let
                val (left, venv') = processValueArg venv tenv argDef
                val (right, venv'') = processValueArgs venv' tenv rest
              in
                (Types.TupleType (left, right), venv'')
              end
          | processValueArgs venv tenv [] = raise Utils.ShouldNotGetHere

        val (typeArgType, tenv') =
          case typeArgs
            of [] => (nothingType, tenv)
             | _ => processTypeArgs tenv typeArgs

        val (valueArgType, venv') =
          case valueArgs
            of [] => (nothingType, venvGlobal)
             | _ => processValueArgs venvGlobal tenv' valueArgs

        val resultType = case (AST.getDefinitionTypeExpr def)
                           of SOME e => let val (t, _) = translateTypeExpr (e, tenv') in t end
                            | NONE => nothingType

        val methodType = case typeArgs
                           of [] => Types.MethodType (valueArgType, resultType)
                            | _ => Types.ParameterizedType (typeArgType, Types.MethodType (valueArgType, resultType))

        val label = Temp.newNamedLabel (Symbol.toString name)
        val method = METHOD { definition = def, venv = venv', tenv = tenv', builtin = AST.hasBuiltinModifier def  }

      in
        case Symbol.lookup (fenv, name)
          of SOME (_, dispatchTree) => Symbol.enter (fenv, name, (name, Functions.insertDispatchNode (dispatchTree, methodType, label, method)))
           | NONE => Symbol.enter (fenv, name, (name, Functions.createDispatchTree (methodType, label, method)))
      end

    fun collectFunctions (fenv, tenv, []) = fenv
      | collectFunctions (fenv, tenv, definition::rest) =
        let
          val newFenv = case (AST.getDefinitionType definition)
                          of AST.Method => collectMethod (fenv, tenv, definition)
                           | _ => fenv
        in
          collectFunctions (newFenv, tenv, rest)
        end

    (* Collect all functions and arrange the methods in them into dispatch trees of METHODs. *)
    val fenv = collectFunctions (Symbol.emptyTable, tenv, program)

    (**********************************************************************************************)
    (* Translate value expressions *)

    fun genCallExpr (typeArg, valueArg) = raise Utils.NotImplemented

    fun genBinaryExpr (operator, leftType, leftTree, rightType, rightTree) =
      let 
        val functionName = case operator
                             of AST.PLUS => Env.builtinPlus
                              | AST.MOD => Env.builtinModulo
                              | AST.EQ => Env.builtinEqual
                              | AST.NEQ => Env.builtinNotEqual
                              | _ => raise Utils.NotImplemented
        val function = case Symbol.lookup (fenv, functionName)
                         of SOME (_, f) => f
                          | _ => raise Argh ("undefined operator")
        val node = Functions.lookupDispatchNode (function, NONE, Types.TupleType (leftType, rightType))
      in
        case node
          of SOME (Functions.DispatchNode { methodType = methodType, children = _, label = label, method = _ }) =>
              (*TODO: builtin*)
              (Tree.CALL (label, [leftTree, rightTree]), Types.getRightOperandType methodType)
           | NONE => (Utils.println (Types.toString leftType); Utils.println (Types.toString rightType); raise Argh "no matching implementation for operator")
      end

    fun translateFunction (_, (name, dispatchTree)) =
      let

        fun processNode (Functions.DispatchNode { methodType = methodType, children = children, label = label, method = METHOD { definition = method, venv = venv, tenv = tenv, builtin = isBuiltin } }) =
          let 

            val methodResultType = (Types.getRightOperandType methodType)
            val hasReturnValue = not (Types.isSubtype (methodResultType, nothingType))

            val epiloqueLabel = Temp.newNamedLabel ((Symbol.toString name) ^ "_epilogue")
            (*REVIEW: Should return vlues always use the same fixed register/temp?*)
            val returnValueLabel = Temp.newTemp ()

            fun getArgFormat (Types.DerivedType (name, _)) =
                  if Symbol.isSame (name, Env.builtinInteger) then Arch.Int32
                  else if Symbol.isSame (name, Env.builtinFloat) then Arch.Float32
                  else Arch.Address
              | getArgFormat _ = Arch.Address

            fun getArgFormats (Types.TupleType (left, right)) = (getArgFormat left)::(getArgFormats right)
              | getArgFormats t = [getArgFormat t]

            val argFormats = getArgFormats (Types.getLeftOperandType methodType)

            (*REVIEW: Isn't it too early to bring in arch-dependencies? Shouldn't this be restricted to the backend? *)
            val (frame, valueArgsAccess) = Arch.newFrame (label, argFormats)

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
                        (*TODO: TYPE_APPLY *)
                         | _ => let val (tree, ty) = translateExpr venv right in ([tree], ty) end
                  in
                    case left
                      of AST.Name id =>
                           let
                             val (_, dispatchTree) = case Symbol.lookup (fenv, (Symbol.create (AST.getIdentifierText id)))
                                                       of SOME node => node
                                                        | NONE => raise Utils.NotImplemented
                             val dispatchNode = case Functions.lookupDispatchNode (dispatchTree, NONE, rightType)
                                                  of SOME node => node
                                                   | NONE => raise Utils.NotImplemented
                             val resultType = Types.getRightOperandType (Functions.getDispatchNodeType dispatchNode)
                           in
                             (Tree.CALL (Functions.getDispatchNodeLabel dispatchNode, rightTree), resultType)
                           end
                       | expr as AST.Binary { region = _, operator = AST.DOT, left = _, right = _ } => raise Utils.NotImplemented
                       | _ => raise Utils.ShouldNotGetHere
                  end
              | translateExpr venv (AST.Binary { region = _, operator = AST.TYPE_APPLY, left = AST.Name id, right = right }) =
                  let
                    val symbol = Symbol.create (AST.getIdentifierText id)
                    val (t, _) = translateTypeExpr (right, tenv)
                  in
                    (***************************TODO*********************)
                    (* Try value first *)
                    case Symbol.lookup (venv, symbol)
                      of SOME (VAR { location = expr, typ = ty }) => raise Utils.NotImplemented (* apply type args to function pointer *)
                       | NONE =>
                           (* No value, try function *)
                           case Symbol.lookup (fenv, symbol)
                             of SOME (_, dispatchTree) =>
                                let 
                                  val dispatchNode = Functions.lookupDispatchNode (dispatchTree, SOME t, nothingType)
                                in
                                  case dispatchNode
                                    of SOME d => (Tree.CALL (Functions.getDispatchNodeLabel d, [nothingValue]), Types.getRightOperandType (Functions.getDispatchNodeType d))
                                     | NONE => raise Argh "Function is not nullary"
                                end
                              | NONE => (print (Symbol.toString symbol); raise Utils.NotImplemented)
                  end
              | translateExpr venv (AST.Binary { region = _, operator = AST.TYPE_APPLY, left = left, right = right }) = raise Utils.NotImplemented
              | translateExpr venv (AST.Binary { region = _, operator = operator, left = left, right = right }) =
                  let
                    val (leftTree, leftType) = translateExpr venv left
                    val (rightTree, rightType) = translateExpr venv right
                  in
                    genBinaryExpr (operator, leftType, leftTree, rightType, rightTree)
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
                             of SOME (_, dispatchTree) =>
                                let 
                                  val dispatchNode = Functions.lookupDispatchNode (dispatchTree, NONE, nothingType)
                                in
                                  case dispatchNode
                                    of SOME d => (Tree.CALL (Functions.getDispatchNodeLabel d, [nothingValue]), Types.getRightOperandType (Functions.getDispatchNodeType d))
                                     | NONE => raise Argh "Function is not nullary"
                                end
                              | NONE => (print (Symbol.toString symbol); raise Utils.NotImplemented)
                  end
              | translateExpr venv (AST.Nothing { region = _ }) = (nothingValue, nothingType)
              | translateExpr venv _ = raise Utils.NotImplemented

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
                        of SOME e => (Tree.SEQ [Tree.MOVE (Tree.TEMP returnValueLabel, e), Tree.JUMP epiloqueLabel], venv)
                         | NONE => (Tree.JUMP epiloqueLabel, venv)
                  end
              | translateStmt venv (AST.ExpressionStatement(expr)) =
                  let
                    val (tree, typ) = translateExpr venv expr
                  in
                    (Tree.EXPR tree, venv)
                  end
              | translateStmt venv (AST.If { region = _, condition = c, trueBranch = t, falseBranch = f}) = raise Utils.NotImplemented
              | translateStmt venv (AST.Switch { region = _, value = v, branches = branches }) =
                  let

                    val (valTree, valType) = translateExpr venv v
                    val temp = Tree.TEMP (Arch.newLocal frame)
                    val endLabel = Temp.newLabel ()

                    (* We need labels for the indidividual blocks so that we can jump to them from the previous block.
                       However, we don't need that for the first one. And the last one should just jump to the end label. *)
                    val caseLabels = map (fn _ => Temp.newLabel ()) branches
                    val caseLabels' = (List.tl caseLabels) @ [endLabel]

                    fun translateCaseBlock (label, AST.Case { region = _, values = values, body = body }, nextCaseBlockLabel) =
                      let
                        val bodyLabel = Temp.newLabel ()
                        val (bodyTree, _) = translateStmts venv body

                        fun caseValue v =
                          let
                            val (tree, typ) = translateExpr venv v
                            (*TODO: check we get a Boolean back here *)
                            val (eqTree, _) = genBinaryExpr (AST.EQ, valType, temp, typ, tree)
                          in
                            (* Note that we do NOT check whether the type of the case value is a subtype of the value
                               type used in the switch statement. All that we require is that there is an equality function
                               defined for the pair. *)
                            Tree.CJUMP (Tree.EQ, eqTree, Tree.TRUE, bodyLabel, nextCaseBlockLabel)
                          end

                      in
                        Tree.SEQ (
                           [Tree.LABEL label] @
                           (map caseValue values) @
                           [Tree.LABEL bodyLabel] @
                           [bodyTree]
                        )
                      end

                  in
                    (Tree.SEQ (
                       (* Assign value to temp so that we don't compute it over and over.
                          There's probably potential here for skipping the temp for certain expressions. *)
                       [Tree.MOVE (temp, valTree)] @
                       (Utils.zip3 translateCaseBlock caseLabels branches caseLabels') @
                       [Tree.LABEL endLabel]
                     ),
                     venv)
                  end
              | translateStmt venv (AST.Variable d) =
                  let
                    val name = createDefinitionSymbol d
                    val temp = Arch.newLocal frame

                    val defType = case AST.getDefinitionTypeExpr d
                                    of SOME e => let val (t, _) = translateTypeExpr (e, tenv) in SOME t end
                                     | NONE => NONE

                    val (tree, ty) = 
                      case AST.getDefinitionBody d
                        of NONE => (case defType
                                      of SOME t => (Tree.MOVE (Tree.TEMP temp, genCallExpr (t, nothingValue)), t)
                                       | NONE => raise Argh ("can't have local var with no init expr and no type"))
                         | SOME (AST.Expression e) =>
                            let
                              val (v, t) = translateExpr venv e
                            in
                              case defType
                                of NONE => (Tree.EXPR v, t)
                                 | SOME ty => if not (Types.isSubtype (ty, t))
                                              then raise Argh ("mismatch in init expr for local var")
                                              else (Tree.EXPR v, ty)
                            end
                         | SOME (AST.Statement l) => raise Utils.NotImplemented

                    val venv' = Symbol.enter (venv, name, VAR { location = Tree.TEMP temp, typ = ty })
                  in
                    (tree, venv')
                  end
              | translateStmt venv (AST.BlockStatement l) =
                  case l
                    of [] => (Tree.NOP, venv)
                     | _ => translateStmts venv l

            and translateStmts venv [] = raise Utils.ShouldNotGetHere
              | translateStmts venv (stmt::[]) = translateStmt venv stmt
              (*TODO: this should unfold nested SEQs*)
              | translateStmts venv (stmt::rest) =
                  let
                    val (left, venv') = translateStmt venv stmt
                    val (right, venv'') = translateStmts venv' rest
                  in
                    (Tree.SEQ [left, right], venv'')
                  end

            val body =
              case (AST.getDefinitionBody method)
                of SOME (AST.Expression (e)) => raise Utils.NotImplemented
                 | SOME (AST.Statement ([])) => Tree.NOP
                 | SOME (AST.Statement (l)) =>
                    let
                      val (t, _) = translateStmts venv l
                    in
                      (*TODO: emit epiloque *)
                      Tree.SEQ [t, Tree.LABEL epiloqueLabel]
                    end
                 | _ => Tree.NOP

          in
            (*TODO: for builtin methods, use a predefined label and don't emit PROC entries
                    or......... what about representing builtins in Tree code? *)
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

