structure Token =
struct

datatype token
  = EOF | UNDERBAR | ID of string | STRING of string
    | REAL of string | INT of string | SPECIAL of string

fun toString tk =
    case tk of
	EOF        => "EOF"
      | UNDERBAR   => "UNDERBAR"
      | ID  name   => "ID " ^ name
      | STRING s   => "STRING \"" ^ s ^ "\""
      | REAL r     => "REAL " ^ r
      | INT i      => "INT " ^ i
      | SPECIAL sp => "SPECIAL " ^ sp

end
