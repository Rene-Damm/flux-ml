structure Translate =
struct

  exception Argh

  fun translateProgram program =
  let 

    fun collectType (tenv, def) =
    let
      val name = Symbol.create (AST.getIdentifierText (AST.getDefinitionName def))
    in
      case Symbol.lookup (tenv, name)
        of SOME _ => raise Argh (****TODO: error, already defined *)
         | NONE => Symbol.enter (tenv, name, Types.NamedType(name, ref NONE))
    end

    fun collectTypes (tenv, []) = tenv
      | collectTypes (tenv, definition::rest) =
        let
          val newTenv = case (AST.getDefinitionType definition)
                          of AST.Type => collectType (tenv, definition)
                           | _ => tenv
        in
          collectTypes (newTenv, rest)
        end

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

    val tenv = collectTypes (Symbol.emptyTable, program)
    val fenv = collectFunctions (Symbol.emptyTable, program)

  in
    Env.dumpTypes (tenv, TextIO.stdOut);
    Env.dumpFunctions (fenv, TextIO.stdOut)
  end

end

