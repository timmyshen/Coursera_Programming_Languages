(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s, strlst) =
    case strlst of
	[] => NONE
      | h::xs => if same_string(s, h) then SOME xs
		 else
		     case all_except_option(s, xs) of
			 NONE => NONE
		       | SOME xs' => SOME (h::xs')

fun get_substitutions1(substitutions, s) =
    case substitutions of
	[]=>[]
      | h::subs' => case all_except_option(s, h) of
			NONE => get_substitutions1(subs', s)
		      | SOME xs => xs @ get_substitutions1(subs', s)

fun get_substitutions2(substitutions, s) =
    let
	fun aux(subs, str, acc) =
	    case subs of
		[] => acc
	      | h::subs' => case all_except_option(str, h) of
				NONE => aux(subs', str, acc)
			      | SOME lst => aux(subs', str, acc @ lst)
    in
	aux(substitutions, s, [])
    end
			

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
