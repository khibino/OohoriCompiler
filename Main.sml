open TM
val T = ([I, I, I], I, nil)
val T2_2 = (nil, B, [I, I, I, I])
(* val T2_2 = (nil, B, [I, O, I]) *)
(* val r = Eval.eval P T; *)
val r = Eval.eval P2_2 T2_2;
val _ = Dynamic.pp {T = T2_2, r = r}
