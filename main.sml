structure Main =
struct

  fun run() =
    let
      val ast = Parse.parse "program.flux"
      val _ = Utils.writeToFile ("program.flux.ast", fn stream => AST.print stream ast)
      val fragments = Translate.translateProgram ast

      fun pprint (Translate.PROC { label = label, body = body, epiloque = epiloque, returnValue = returnValue, frame = frame }) =
            [PPrint.String ("PROC ." ^ (Temp.labelToString label)), PPrint.block
              [PPrint.Seq [PPrint.String "body=", PPrint.Block (PPrint.Seq [Tree.pprint body, PPrint.Newline])],
               PPrint.String ("retVal=" ^ (case returnValue of SOME l => Temp.tempToString l | NONE => "NONE"))]]
        | pprint (Translate.STRING (label, str)) =
            [PPrint.String ("STRING ." ^ (Temp.labelToString label)), PPrint.Space, PPrint.String ("\"" ^ str ^ "\"")]

      fun pprintFragments [] = []
        | pprintFragments (f::rest) = List.concat [pprint f, [PPrint.Newline, PPrint.Newline], pprintFragments rest]

      fun canonicalizeFragment (Translate.PROC { label = label, body = body, epiloque = epiloque, returnValue = returnValue, frame = frame }) =
            let
              val linearizedBody = CFlow.linearize body
            in
              Translate.PROC { label = label, body = Tree.SEQ linearizedBody, epiloque = epiloque, returnValue = returnValue, frame = frame }
            end
        | canonicalizeFragment f = f

      fun scheduleFragment (Translate.PROC { label = label, body = body, epiloque = epiloque, returnValue = returnValue, frame = frame }) =
            let
              val (startLabel, basicBlockTable) = case body of Tree.SEQ stmts => CFlow.basicBlocks stmts | _ => raise Utils.ShouldNotGetHere
              val scheduledStmts = CFlow.traceSchedule (startLabel, basicBlockTable)
            in
              Translate.PROC { label = label, body = Tree.SEQ scheduledStmts, epiloque = epiloque, returnValue = returnValue, frame = frame }
            end
        | scheduleFragment f = f

      val canonicalFragments = map canonicalizeFragment fragments
      val scheduledFragments = map scheduleFragment canonicalFragments

    in
      Utils.writeToFile ("program.flux.tree", fn stream => PPrint.print stream (pprintFragments fragments));
      Utils.writeToFile ("program.flux.tree.1canon", fn stream => PPrint.print stream (pprintFragments canonicalFragments));
      Utils.writeToFile ("program.flux.tree.2schedule", fn stream => PPrint.print stream (pprintFragments scheduledFragments))
    end

end

