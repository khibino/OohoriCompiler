(* ユーザー定義セクション *)

type lexresult = Token.token
val eof = fn () => Token.EOF
fun atoi s = valOf (Int.fromString s)

(* 補助定義セクション ↓ *)
%%

%structure CoreMLLex
alpha = [A-Za-z];
digit = [0-9];
id    = {alpha}({alpha}|{digit})*;
num   = {digit}+;
frac  = "."{num};
exp   = [eE](~?){num};
real  = (~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
int   = (~?){num};
ws    = "\ " | "\t" | "\r\n" | "\n" | "\r";

%%

\"[^"]*\" => (Token.STRING
              (String.substring
                 (yytext, 1, String.size yytext - 2)));
"_"       => (Token.UNDERBAR);
{id}      => (Token.ID yytext);
{real}    => (Token.REAL yytext);
{int}     => (Token.INT yytext);
{ws}      => (lex ());
.         => (Token.SPECIAL yytext);
