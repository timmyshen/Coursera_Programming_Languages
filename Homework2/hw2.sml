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
	fun aux(subs, acc) =
	    case subs of
		[] => acc
	      | h::subs' => case all_except_option(s, h) of
				NONE => aux(subs', acc)
			      | SOME lst => aux(subs', acc @ lst)
    in
	aux(substitutions, [])
    end
			
fun similar_names(substitutions, {first=f, middle=m, last=l}) =
    let
	fun create_similar_name(sub_first) =
	    {first=sub_first, middle=m, last=l}

	fun aux(subs, acc) =
	    case subs of
		[] => acc
	      | h::subs' => aux(subs',acc) @ [create_similar_name(h)]

	val subs_first = get_substitutions2(substitutions, f)
    in
	case subs_first of
	    [] => {first = f, middle = m, last = l}
	  | h::subs' => [{first = f, middle = m, last = l}, {first = h, middle = m, last = l}] @ 
	(*aux(substitutions, {first=f, middle=m, last=l})*)
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
