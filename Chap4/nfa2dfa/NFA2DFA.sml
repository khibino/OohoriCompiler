
structure NFA2DFA =
struct

type S = string
type Q = int
type delta = (Q * (S * Q list) list) list
type NFA = { Q     : Q list
           , S     : S list
	   , delta : delta
	   , q0    : Q
	   , F     : Q list
	   }

type state = Q list
type Delta = (state * (S * state) list) list
type DFA = { Q      : state list
	   , S      : S list
	   , Delta  : Delta
	   , Q0     : state
	   , F      : state list
	   }

type Omega = (S * state) list


exception TODO

fun nub [] = []
  | nub (x :: xs : ''a list) : ''a list =
      x :: nub (List.filter (fn y => y <> x) xs)

fun intersect (xs : ''a list) =
    let fun irec  []        = []
	  | irec (y :: ys)  =
	    if List.exists (fn x => x = y) xs
	    then y :: irec ys
	    else irec ys
    in irec
    end

fun for (xs : 'a list) (f : 'a -> 'b)
    : 'b list = List.map f xs

fun concatMap (f : 'a -> 'b list) (xs : 'a list)
    : 'b list = List.concat (List.map f xs)

fun lookup (k : ''a) (xxs : (''a * 'b) list)
    : 'b option =
    case xxs of
	[]  => NONE
      | (k1, v1) :: xs =>
	  if k1 = k
	  then SOME v1
	  else lookup k xs

infix 5 +:

datatype 'a slist
  = SNil
  | +: of 'a slist * 'a

fun deltaLookup (d : delta) (q : Q) (s : S)
    : Q list =
    case lookup q d of
       NONE       =>               []
     | SOME stbl  => case lookup s stbl of
			NONE    => []
		      | SOME qs => qs

fun nfa2dfa (nfa as {S = sigma, delta = deltaTable, ...} : NFA)
    : DFA =
    let
	val delta : Q -> S -> Q list =
	    deltaLookup deltaTable

	val epsilonStep = concatMap (fn p => delta p "")
	fun mfix xs =
	    let val ys = nub (xs @ epsilonStep xs)
	    in if xs = ys then xs else mfix ys
	    end
	fun Cl (P : Q list) : Q list = mfix P

	fun deltaHat (p : Q) : S slist -> Q list =
	    let fun drec (SNil   : S slist)
		    : Q list  = Cl [p]
		  | drec (w +: a : S slist)
		    : Q list  = Cl (concatMap (fn q => delta q a) (drec w))
	    in drec
	    end

	fun deltaD (P : Q list) (a : S) =
	    concatMap (fn p => deltaHat p (SNil +: a)) P

	fun addS ((A, s) : state * S)
		 ((Q1, Q2, omega) : state list * state list * Omega)
	    : state list * Omega =
	    let val A' = deltaD A s
		val qlist = A :: (Q1 @ Q2)
		val Q1' = if List.exists (fn x => x = A') qlist
			  then Q1
			  else A' :: Q1
	    in (Q1', (s, A') :: omega)
	    end

	fun addQ (A : Q list)
		 ((Q1, Q2, Delta) : state list * state list * Delta)
	    : state list * state list * Delta =
	    let fun add (s, (Q1', omg)) =
		    addS (A, s) (Q1', Q2, omg)
		val (Q1n, omgn) = List.foldl add (Q1, []) sigma
	    in  (Q1n, A :: Q2, (A , omgn) :: Delta)
	    end

	fun subsets ([],      Q2, Delta) = (Q2, Delta)
	  | subsets (A :: Q1, Q2, Delta) = subsets (addQ A (Q1, Q2, Delta))

	fun toDFA (nfa as {Q = Q, S = sigma, q0 = q0, F = F, ...} : NFA)
	    : DFA =
	    let val A = Cl [q0] : state
		val (Q', Delta) = subsets ([A], [], [])
		val F' = concatMap
			     (fn A' =>
				 if intersect A' F = []
				 then []
				 else [A'])
			     Q'
	    in  { Q = Q', S = sigma, Delta = Delta, Q0 = A, F = F' }
	    end

    in  toDFA nfa
    end (* end of let *)

end (* end of nfa2dfa *)
