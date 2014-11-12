(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals xs =
    List.filter (Char.isUpper o (fn s => String.sub(s, 0))) xs

(* val only_capitals = List.filter (fn s => Char.isUpper (String.sub(s,0))) *)

fun longest_string1 xs =
    foldl (fn (acc, x) =>
		   if String.size acc > String.size x then acc
		   else x
	       ) "" xs

(* Better style
val longest_string1 = 
    List.foldl (fn (s,sofar) => if String.size s > String.size sofar
				then s
				else sofar) 
	       ""
*)

fun longest_string2 xs =
    foldl (fn (acc, x) =>
		   if String.size acc >= String.size x then acc
		   else x
	       ) "" xs

(* Better style
val longest_string2 = 
    List.foldl (fn (s,sofar) => if String.size s >= String.size sofar
				then s
				else sofar) 
	       ""
*)

fun longest_string_helper f xs=
    foldl (fn (acc, x) => 
	      if f (String.size acc, String.size x) then acc
	      else x
	  ) "" xs

(* Better style
fun longest_string_helper f = 
    List.foldl (fn (s,sofar) => if f(String.size s,String.size sofar)
				then s
				else sofar) 
	       ""
*)

val longest_string3 = longest_string_helper (fn (a, b) => a > b)

val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

val longest_capitalized = longest_string1 o only_capitals

(* At this step, the qualification of the functions are not needed*)
(* I explicitly write them out for the sake of clarity *)
val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      NONE => first_answer f xs'
		    | SOME y => y


fun all_answers f xs =
    let 
	fun help_answer [] acc = SOME acc
	  | help_answer (x::xs') acc =
	    case f x of
		NONE => NONE
	      | SOME v => help_answer xs' (acc @ v)
    in
	help_answer xs []
    end

val count_wildcards = g (fn () => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

fun count_some_var (s, p) = g (fn () => 0) (fn s' => if s = s' then 1 else 0) p


(* Sample answer *)
fun check_pat pat = 
    let
	fun get_vars pat =
	    case pat of
		Variable s => [s]
	      | TupleP ps  => List.foldl (fn (p,vs) => get_vars p @ vs) [] ps
	      | ConstructorP(_,p) => get_vars p
	      | _          => []
	fun unique xs = 
	    case xs of
		[]     => true
	      | x::xs' => (not (List.exists (fn y => y=x) xs'))
			  andalso unique xs'
    in 
	unique (get_vars pat)
    end


fun match vp =
    case vp of
        (_,Wildcard) => SOME []
      | (v, Variable s) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const a, ConstP a') => if a = a'
				then SOME []
				else NONE
      | (Tuple ps, TupleP vs) =>
        if (length ps) = (length vs)
        then all_answers match (ListPair.zip(ps, vs))
        else NONE
      | (Constructor(s1, p), ConstructorP(s2, v)) => if s1 = s2
						     then match (p, v)
                                                     else NONE
      | _ => NONE;

(* Sample answer
fun match (valu,pat) =
    case (valu,pat) of
	(_,Wildcard)    => SOME []
      | (_,Variable(s)) => SOME [(s,valu)]
      | (Unit,UnitP)    => SOME []
      | (Const i, ConstP j)    => if i=j then SOME [] else NONE
      | (Tuple(vs),TupleP(ps)) => if length vs = length ps
				  then all_answers match (ListPair.zip(vs,ps))
				  else NONE
      | (Constructor(s1,v), ConstructorP(s2,p)) => if s1=s2
						   then match(v,p)
						   else NONE
      | _ => NONE
*)

fun first_match v ps = 
    SOME (first_answer (fn p => match (v, p)) ps)
    handle NoAnswer => NONE;
