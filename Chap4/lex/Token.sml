structure Token =
struct

datatype token
  = EOF | COMMA | PERIOD | DOTS | COLON | SEMICOLON
  | EQ | GE | LBRACKET | RBRACKET | UNDERBAR | BAR
  | KEYWORD of string | ID of string | STRING of string
  | REAL of string | INT of string | SPECIAL of string

fun toString tk =
    case tk of
	EOF        => "EOF"
      | COMMA      => "COMMA"
      | PERIOD     => "PERIOD"
      | DOTS       => "DOTS"
      | COLON      => "COLON"
      | SEMICOLON  => "SEMICOLON"
      | EQ         => "EQ"
      | GE         => "GE"
      | LBRACKET   => "LBRACKET"
      | RBRACKET   => "RBRACKET"
      | UNDERBAR   => "UNDERBAR"
      | BAR        => "BAR"
      | KEYWORD kw => "KEYWORD " ^ kw
      | ID  name   => "ID " ^ name
      | STRING s   => "STRING \"" ^ s ^ "\""
      | REAL r     => "REAL " ^ r
      | INT i      => "INT " ^ i
      | SPECIAL sp => "SPECIAL " ^ sp

end
