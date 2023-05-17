
structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

fun eof() = Tokens.EOF(0, 0)

val strValue = ref ""
val strStartPos = ref 0

%%

%header (functor FluxLexFun(structure Tokens: Flux_TOKENS));

%s STRING;

%%

<INITIAL> "type"                                => (Tokens.TYPE(yypos, yypos + 4));
<INITIAL> "method"                              => (Tokens.METHOD(yypos, yypos + 6));
<INITIAL> "field"                               => (Tokens.FIELD(yypos, yypos + 5));
<INITIAL> "object"                              => (Tokens.OBJECT(yypos, yypos + 6));
<INITIAL> "abstract"                            => (Tokens.ABSTRACT(yypos, yypos + 8));
<INITIAL> "immutable"                           => (Tokens.IMMUTABLE(yypos, yypos + 9));
<INITIAL> "mutable"                             => (Tokens.MUTABLE(yypos, yypos + 7));
<INITIAL> "builtin"                             => (Tokens.BUILTIN(yypos, yypos + 7));
<INITIAL> [0-9]+                                => (Tokens.INTEGER(Option.valOf(Int.fromString yytext), yypos, yypos + size yytext));
<INITIAL> [A-Za-z_][A-Za-z0-9_]*                => (Tokens.ID(yytext, yypos, yypos + size yytext));
<INITIAL> ";"                                   => (Tokens.SEMICOLON(yypos, yypos + 1));
<INITIAL> ":"                                   => (Tokens.COLON(yypos, yypos + 1));
<INITIAL> "("                                   => (Tokens.LPAREN(yypos, yypos + 1));
<INITIAL> ")"                                   => (Tokens.RPAREN(yypos, yypos + 1));
<INITIAL> "{"                                   => (Tokens.LBRACE(yypos, yypos + 1));
<INITIAL> "}"                                   => (Tokens.RBRACE(yypos, yypos + 1));
<INITIAL> "<"                                   => (Tokens.LANGLE(yypos, yypos + 1));
<INITIAL> ">"                                   => (Tokens.RANGLE(yypos, yypos + 1));
<INITIAL> "="                                   => (Tokens.EQUAL(yypos, yypos + 1));
<INITIAL> "|"                                   => (Tokens.OR(yypos, yypos + 1));
<INITIAL> "&"                                   => (Tokens.AND(yypos, yypos + 1));
<INITIAL> [ \t\r\n]+                            => (continue());
<INITIAL> .                                     => (Diagnostics.error; continue());

<INITIAL> "\""                                  => (YYBEGIN STRING; strValue := ""; strStartPos := yypos; continue());
<STRING> "\""                                   => (YYBEGIN INITIAL; Tokens.STRING(!strValue, !strStartPos, yypos + 1));
<STRING> [^"]+                                  => (strValue := !strValue ^ yytext; continue());

