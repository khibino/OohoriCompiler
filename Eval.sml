
structure Eval =
struct

open TM

fun Hd nil = B
  | Hd (h :: _) = h

fun Tl nil = nil
  | Tl (_ :: tl) = tl

fun Cons (B, nil) = nil
  | Cons (h,t) =  h :: t

fun moveL (LList, h, RList) =
    (Tl LList, Hd LList, Cons (h, RList))
fun moveR (LList, h, RList) =
    (Cons (h, LList), Hd RList, Tl RList)

fun move L tape = moveL tape
  | move R tape = moveR tape

fun exec delta (q, tape as (LList, h, RList)) =
    case List.find (fn (x,y) => x = (q, h)) delta of
	NONE => (LList, h, RList)
      | SOME (x, (q', Move d)) =>
        let val _ = Dynamic.pp tape in
        exec delta (q', move d tape)
	end
      | SOME (x, (q', Write s)) =>
        let val _ = Dynamic.pp tape in
	exec delta (q', (LList, s, RList))
        end

fun eval (state, delta) tape = exec delta (state, tape)

end
