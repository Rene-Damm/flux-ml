structure Mlex  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
INITIAL | STRING
    structure UserDeclarations = 
      struct

type lexresult = Tokens.token
fun eof() = Tokens.EOF



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TYPE(yypos)))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.METHOD(yypos)))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FIELD(yypos)))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ABSTRACT(yypos)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IMMUTABLE(yypos)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MUTABLE(yypos)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BUILTIN(yypos)))
fun yyAction7 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.ID(yytext, yypos))
      end
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN STRING; continue()))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMICOLON(yypos)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COLON(yypos)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN(yypos)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN(yypos)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE(yypos)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE(yypos)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Diagnostics.error; continue()))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; continue()))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ59(strm', lastMatch)
            else if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp = #"`"
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction0(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"`"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ19(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ22(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ21(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"y"
              then yyQ20(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"y"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp = #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction5(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction5(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp = #"`"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ19(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ29(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ28(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ27(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ26(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ25(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction1(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction1(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction1(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"`"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ19(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"d"
              then yyQ33(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"d"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ32(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"h"
              then yyQ31(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"h"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ30(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then if inp = #"0"
                      then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                    else if inp < #"0"
                      then yyAction7(strm, yyNO_MATCH)
                    else if inp <= #"9"
                      then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                      else yyAction7(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ24(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"e"
                  then yyQ23(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction4(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"`"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ19(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ41(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ40(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ39(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ37(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ36(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ35(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ34(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"`"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ19(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"d"
              then yyQ45(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"d"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ44(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ43(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ42(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction6(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction6(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp = #"`"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ19(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ51(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ50(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ49(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ48(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ47(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ46(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction3(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction3(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"`"
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ19(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
                  else yyAction3(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ58(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"c"
              then yyQ57(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"c"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ56(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ55(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ54(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ53(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"b"
              then yyQ52(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"`"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction7(strm, yyNO_MATCH)
                      else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction7(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp = #"`"
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ19(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyQ4(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"\r"
              then if inp = #"\t"
                  then yyQ4(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction15(strm, yyNO_MATCH)
                else if inp <= #"\n"
                  then yyQ4(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ4(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\r"
              then yyQ4(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"\r"
              then if inp = #"\t"
                  then yyQ4(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp < #"\t"
                  then yyAction15(strm, yyNO_MATCH)
                else if inp <= #"\n"
                  then yyQ4(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ4(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyQ2(strm', lastMatch)
            else if inp < #"["
              then if inp = #"\""
                  then yyQ5(strm', lastMatch)
                else if inp < #"\""
                  then if inp = #"\r"
                      then yyQ3(strm', lastMatch)
                    else if inp < #"\r"
                      then if inp = #"\n"
                          then yyQ4(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ3(strm', lastMatch)
                              else yyQ2(strm', lastMatch)
                          else yyQ2(strm', lastMatch)
                    else if inp = #" "
                      then yyQ3(strm', lastMatch)
                      else yyQ2(strm', lastMatch)
                else if inp = #":"
                  then yyQ8(strm', lastMatch)
                else if inp < #":"
                  then if inp = #")"
                      then yyQ7(strm', lastMatch)
                    else if inp < #")"
                      then if inp = #"("
                          then yyQ6(strm', lastMatch)
                          else yyQ2(strm', lastMatch)
                      else yyQ2(strm', lastMatch)
                else if inp = #"<"
                  then yyQ2(strm', lastMatch)
                else if inp < #"<"
                  then yyQ9(strm', lastMatch)
                else if inp <= #"@"
                  then yyQ2(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp = #"j"
              then yyQ10(strm', lastMatch)
            else if inp < #"j"
              then if inp = #"b"
                  then yyQ12(strm', lastMatch)
                else if inp < #"b"
                  then if inp = #"`"
                      then yyQ2(strm', lastMatch)
                    else if inp < #"`"
                      then if inp = #"_"
                          then yyQ10(strm', lastMatch)
                          else yyQ2(strm', lastMatch)
                      else yyQ11(strm', lastMatch)
                else if inp = #"g"
                  then yyQ10(strm', lastMatch)
                else if inp < #"g"
                  then if inp = #"f"
                      then yyQ13(strm', lastMatch)
                      else yyQ10(strm', lastMatch)
                else if inp = #"i"
                  then yyQ14(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp = #"u"
              then yyQ10(strm', lastMatch)
            else if inp < #"u"
              then if inp = #"n"
                  then yyQ10(strm', lastMatch)
                else if inp < #"n"
                  then if inp = #"m"
                      then yyQ15(strm', lastMatch)
                      else yyQ10(strm', lastMatch)
                else if inp = #"t"
                  then yyQ16(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp = #"|"
              then yyQ2(strm', lastMatch)
            else if inp < #"|"
              then if inp = #"{"
                  then yyQ17(strm', lastMatch)
                  else yyQ10(strm', lastMatch)
            else if inp = #"}"
              then yyQ18(strm', lastMatch)
              else yyQ2(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of INITIAL => yyQ0(!(yystrm), yyNO_MATCH)
    | STRING => yyQ1(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
