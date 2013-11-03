fun match xs =
    let fun s_need_one xs =
	    case xs of
		[] => true
	      | 1::xs' => s_need_two xs'
	      | _ => false
	and s_need_two xs =
	    case xs of
		[] => false
	      | 2::xs' => s_need_one xs'
	      | _ => false
    in
	s_need_one xs
    end

datatype t1 = Foo of int | Bar of t2
     and t2 = Baz of string | Quux of t1

fun no_zeros_or_empty_strings_t1 x =
    case x of
	Foo i => i <> 0
      | Bar y => no_zeros_or_empty_strings_t2 y
and no_zeros_or_empty_strings_t2 x =
    case x of
	Baz s => size s > 0
      | Quux y => no_zeros_or_empty_strings_t1 y

fun no_zeros_or_empty_strings_t3 (f, x) =
    case x of
	Foo i => i <> 0
      | Bar y => f y

fun no_zeros_or_empty_strings_t4 x =
    case x of
	Baz s => size s > 0
      | Quux y => no_zeros_or_empty_strings_t3(no_zeros_or_empty_strings_t4, y)

signature MYSIG =
sig
    val name : string list
    val sex : string
    (* val base = 1 *)
end

structure MyStr :> MYSIG =
struct
val name = "Ben"::[]
val age = 25
val sex = "male"
val base = 2;
end
(*
signature DIGIT = 
sig
    type digit = int
    val make_digit : int -> digit
    val increment : digit -> digit
    val decrement : digit -> digit
    val down_and_up : digit -> digit
    val test : digit -> unit
end
*)

(*
signature DIGIT = 
sig
    type digit = int
    val make_digit : int -> digit
    val increment : digit -> digit
    val decrement : digit -> digit
    val down_and_up : digit -> digit
end
*)

(*
signature DIGIT = 
sig
    type digit = int
    val make_digit : int -> digit
    val increment : digit -> digit
    val decrement : digit -> digit
    val test : digit -> unit
end
*)

(*
signature DIGIT = 
sig
    type digit
    val make_digit : int -> digit
    val increment : digit -> digit
    val decrement : digit -> digit
    val down_and_up : digit -> digit
    val test : digit -> unit
end
*)


signature DIGIT = 
sig
    type digit
    val increment : digit -> digit
    val decrement : digit -> digit
    val down_and_up : digit -> digit
    val test : digit -> unit
end


structure Digit :> DIGIT =
struct
type digit = int
exception BadDigit
exception FailTest
fun make_digit i = if i < 0 orelse i > 9 then raise BadDigit else i
fun increment d = if d=9 then 0 else d+1
fun decrement d = if d=0 then 9 else d-1
val down_and_up = increment o decrement (* recall o is function composition *)
fun test d = if down_and_up d = d then () else raise FailTest
end
