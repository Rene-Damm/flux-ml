structure CFlow =
struct

  datatype basicblock = BasicBlock of { label: Temp.label, body: Tree.stmt list, exits: Temp.label list }

  fun getBasicBlockLabel (BasicBlock { label = l, body = _, exits = _ }) = l

  (* Takes a Tree.smt and turns it into a linear list of statements with no SEQs. *)
  fun linearize (Tree.SEQ stmts) = List.concat (map linearize stmts)
    | linearize stmt = [stmt]

  (* Takes a linear list of Tree.stmts and produces a table of basic blocks. *)
  fun basicBlocks stmts =
    let

      fun replaceLabel (label, oldLabel, newLabel) =
        if Temp.isSameLabel (label, oldLabel)
        then newLabel
        else label

      fun removeLabel label ((s as Tree.LABEL l)::rest) = if Temp.isSameLabel (label, l) then rest else s::(removeLabel label rest)
        | removeLabel label (s::rest) = s::(removeLabel label rest)
        | removeLabel label [] = []

      (* Does not replace labels in calls; there can't be redundancy in those. *)
      fun replaceLabelInJumps oldLabel newLabel (Tree.JUMP label) = Tree.JUMP (replaceLabel (label, oldLabel, newLabel))
        | replaceLabelInJumps oldLabel newLabel (Tree.CJUMP (operator, left, right, trueBranch, falseBranch)) =
            Tree.CJUMP (operator, left, right, replaceLabel (trueBranch, oldLabel, newLabel), replaceLabel (falseBranch, oldLabel, newLabel))
        | replaceLabelInJumps _ _ s = s

      (* Takes a list of statements and replaces all directly adjacent LABELs with a single LABEL statement. *)
      fun removeRedundantLabels filteredStmts ((stmt1 as Tree.LABEL firstLabel)::((stmt2 as Tree.LABEL secondLabel)::rest)) =
            let
              val filteredStmts' = map (replaceLabelInJumps firstLabel secondLabel) filteredStmts
              val filteredStmts'' = removeLabel firstLabel filteredStmts'
            in
              removeRedundantLabels filteredStmts'' (stmt2::rest)
            end
        | removeRedundantLabels filteredStmts (s::rest) = removeRedundantLabels filteredStmts rest
        | removeRedundantLabels filteredStmts [] = filteredStmts

      fun isJumpToLabel label (Tree.JUMP l) = Temp.isSameLabel (l, label)
        | isJumpToLabel label (Tree.CJUMP (_, _, _, trueBranch, falseBranch)) = (Temp.isSameLabel (trueBranch, label)) orelse (Temp.isSameLabel (falseBranch, label))
        | isJumpToLabel _ _ = false

      fun removeUnusedLabels filteredStmts ((stmt as Tree.LABEL label)::rest) =
            if List.exists (isJumpToLabel label) filteredStmts
            then removeUnusedLabels filteredStmts rest
            else removeUnusedLabels (removeLabel label filteredStmts) rest
        | removeUnusedLabels filteredStmts (s::rest) =
            removeUnusedLabels filteredStmts rest
        | removeUnusedLabels filteredStmts [] = filteredStmts

      val cleanedStmts = removeRedundantLabels stmts stmts
      val cleanedStmts' = removeUnusedLabels cleanedStmts cleanedStmts

      fun endBlock (SOME (BasicBlock { label = l, body = b, exits = e })) mapOfBlocks =
            let
              (* We prepend rather than append to the statement lists while putting together a basic block.
                 Put the statement sequence back in the correct order by reversing it. *)
              val block = BasicBlock { label = l, body = List.rev b, exits = e }
            in
              Symbol.enter (mapOfBlocks, l, block)
            end
        | endBlock NONE mapOfBlocks = mapOfBlocks

      (* Creates a dictionary of basic blocks for given sequence of statements. *)
      fun process current mapOfBlocks ((stmt as Tree.LABEL l)::rest) =
            (* LABEL starts new block. *)
            process (SOME (BasicBlock { label = l, body = [stmt], exits = [] })) (endBlock current mapOfBlocks) rest
        | process (SOME (BasicBlock { label = blockLabel, body = b, exits = _ })) mapOfBlocks ((stmt as Tree.JUMP exitLabel)::rest) =
            (* JUMP ends current block. *)
            process NONE (endBlock (SOME (BasicBlock { label = blockLabel, body = stmt::b, exits = [exitLabel] })) mapOfBlocks) rest
        | process (SOME (BasicBlock { label = l, body = b, exits = _ })) mapOfBlocks ((stmt as Tree.CJUMP (operator, left, right, trueBranch, falseBranch))::rest) =
            (* CJUMP ends current block. *)
            process NONE (endBlock (SOME (BasicBlock { label = l, body = stmt::b, exits = [trueBranch, falseBranch] })) mapOfBlocks) rest
        | process (SOME (BasicBlock { label = l, body = b, exits = _ })) mapOfBlocks (s::rest) =
            process (SOME (BasicBlock { label = l, body = s::b, exits = [] })) mapOfBlocks rest
        | process current mapOfBlocks [] = endBlock current mapOfBlocks
        | process _ _ s = (PPrint.print TextIO.stdOut [Tree.pprint (Tree.SEQ s)]; raise Utils.ShouldNotGetHere)

      val startLabel = Temp.newLabel ()

    in
      (startLabel, process NONE Symbol.emptyTable ((Tree.LABEL startLabel)::stmts))
    end

  (* Perform a simple version of trace scheduling. Returns a linear list of statements in the order arrived at by the scheduler. *)
  fun traceSchedule (startLabel, basicBlockTable) =
    let

      fun traceAll label blockTable =
        let
          val (block as BasicBlock { label = _, body = body, exits = exits }) = Symbol.find (blockTable, label)

          fun newTrace () =
            let
              val (newLabel, _) = List.hd (Symbol.all blockTable)
              val remainingBlocks = Symbol.remove (blockTable, newLabel)
            in
              traceAll newLabel remainingBlocks
            end
        in
          case exits
            of [] => if (Symbol.numItems blockTable) = 0
                     then [block] (* Have traced everything. *)
                     else block::(newTrace ())
             | jmpLabel::[] => raise Utils.NotImplemented
             | trueLabel::falseLabel::[] => raise Utils.NotImplemented
             | _ => raise Utils.ShouldNotGetHere
        end

      fun listStatements ((BasicBlock { label = _, body = stmts, exits = _ })::rest) = stmts @ (listStatements rest)
        | listStatements [] = []

    in
      List.app (fn (s, _) => Utils.println ("bblock: " ^ (Temp.labelToString s))) (Symbol.all basicBlockTable);
      listStatements (traceAll startLabel basicBlockTable)
    end

end

