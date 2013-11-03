fun mystery f = fn xs =>
		   let
		       fun g xs =
			   case xs of
			       [] => NONE
			     | x::xs' => if f x then SOME x else g xs'
		   in
		       case xs of
			   [] => NONE
			 | x::xs' => if f x then g xs' else mystery f xs'
		   end

(* fun null xs = case xs of [] => true | _ => false *)
(* fun null xs = xs=[] *)
(* fun null xs = if null xs then true else false *)
(* fun null xs = ((fn z => false) (hd xs)) handle List.Empty => true *)

(* 11 *)
(*
signature COUNTER =
sig
    type t = int
    val newCounter : int -> t
    val increment : t -> t
    val first_larger : t * t -> bool
end
*)

(* 12 *)
(*
signature COUNTER =
sig
    type t = int
    val newCounter : int -> t
    val first_larger : t * t -> bool
end
*)

(* 13 *)
(*
signature COUNTER =
sig
    type t
    val newCounter : int -> int
    val increment : t -> t
    val first_larger : t * t -> bool
end
*)

(* 14 *)
(*
signature COUNTER =
sig
    type t
    val newCounter : int -> t
    val increment : t -> t
    val first_larger : t * t -> bool
end
*)

(* 15 *)
signature COUNTER =
sig
    type t = int
    val newCounter : int -> t
    val increment : t -> t
end


structure NoNegativeCounter :> COUNTER = 
struct

exception InvariantViolated
	      
type t = int

fun newCounter i = if i <= 0 then 1 else i (* fn: int -> int *)

fun increment i = i + 1

fun first_larger (i1,i2) = (* int*int -> bool *)
    if i1 <= 0 orelse i2 <= 0
    then raise InvariantViolated
    else (i1 - i2) > 0
			 
end
