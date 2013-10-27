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

fun longest_string1 xs =
    foldl (fn (acc, x) =>
		   if String.size acc > String.size x then acc
		   else x
	       ) "" xs

fun longest_string2 xs =
    foldl (fn (acc, x) =>
		   if String.size acc >= String.size x then acc
		   else x
	       ) "" xs

fun longest_string_helper f xs=
    foldl (fn (acc, x) => 
	      if f (String.size acc, String.size x) then acc
	      else x
	  ) "" xs

val longest_string3 = longest_string_helper (fn (a, b) => a > b)

val longest_string4 = longest_string_helper (fn (a, b) => a >= b)

val longest_capitalized = longest_string1 o only_capitals

(* At this step, the qualification of the functions are not needed*)
(* I explicitly write them out for the sake of clarity *)
val rev_string = String.implode o List.rev o String.explode
