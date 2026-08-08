
structure TM =
struct

datatype D = R | L
datatype S = B | I | O
datatype Q = M | H | W
datatype A = Move of D | Write of S

(* type delta = ((Q * S) * (Q * S * D)) list *)
type delta = ((Q * S) * (Q * A)) list
type program = Q * delta
type tape = S list * S * S list

val P = (M, [((M, I), (W, Write O)),
	     ((M, O), (W, Write I)),
	     ((M, B), (W, Write I)),
	     ((W, O), (M, Move L)),
	     ((W, I), (H, Move L))
	])

(*
val P = (M, [((M, I), (M, O, L)),
	     ((M, O), (H, I, L)),
	     ((M, B), (H, I, L))])
 *)

end
