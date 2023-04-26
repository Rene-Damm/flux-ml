type lexresult = Tokens.token
fun eof() = Tokens.EOF

%%

%s STRING;

%%

<INITIAL> "type"                                => (Tokens.TYPE(yypos));
<INITIAL> "method"                              => (Tokens.METHOD(yypos));
<INITIAL> "field"                               => (Tokens.FIELD(yypos));
<INITIAL> "abstract"                            => (Tokens.ABSTRACT(yypos));
<INITIAL> "immutable"                           => (Tokens.IMMUTABLE(yypos));
<INITIAL> "mutable"                             => (Tokens.MUTABLE(yypos));
<INITIAL> "builtin"                             => (Tokens.BUILTIN(yypos));
<INITIAL> [A-Za-z_][A-Za-z0-9_]*                => (Tokens.ID(yytext, yypos));
<INITIAL> "\""                                  => (YYBEGIN STRING; continue());
<INITIAL> ";"                                   => (Tokens.SEMICOLON(yypos));
<INITIAL> ":"                                   => (Tokens.COLON(yypos));
<INITIAL> "("                                   => (Tokens.LPAREN(yypos));
<INITIAL> ")"                                   => (Tokens.RPAREN(yypos));
<INITIAL> "{"                                   => (Tokens.LBRACE(yypos));
<INITIAL> "}"                                   => (Tokens.RBRACE(yypos));
<INITIAL> [ \t\r\n]+                            => (continue());
<INITIAL> .                                     => (Diagnostics.error; continue());

<STRING> "\""                                   => (YYBEGIN INITIAL; continue());

