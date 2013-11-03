val x = 50
val y = 3
val z = 10
val f = fn z => z
val a = 
    let 
	val x = 3*x
	val z = y*z
    in
	x*z
    end

fun f x z = x + y + z (* Inside the body, x is locally binded. *)

fun maybeEven x = 
	if x = 0 
	then true
	else
	if x = 50
	then false
	else maybeOdd (x-1)

and maybeOdd y =
	if y = 0
	then false
	else 
	if y = 99
	then true
	else maybeEven (y-1)

(* For input x > 50, maybeEven always returns false. *)
(* This is not true. Consider maybeEven 100. *)

