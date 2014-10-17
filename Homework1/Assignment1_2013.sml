fun is_older (date1 : int * int *int, date2 : int * int * int) =
    (* Name the parameters to increase readability*)
    let (* Use let binding to avoid repetition*)
	val y1 = #1 date1
        val m1 = #2 date1
        val d1 = #3 date1
        val y2 = #1 date2
        val m2 = #2 date2
        val d2 = #3 date2
    in
	(* This is more concise *)
	y1 < y2
        orelse (y1 = y2 andalso m1 < m2)
        orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)
    end

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates then 0
    else
	if #2 (hd dates) = month then 1 + number_in_month(tl dates, month)
	else number_in_month(tl dates, month)

fun number_in_months (dates : (int * int * int) list, months: int list) =
    if null months then 0
    else
	number_in_month(dates, hd months) +  number_in_months(dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates then []
    else
	if #2 (hd dates) = month
	then (hd dates) :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months then []
    else
	dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (strlist : string list, n : int) =
    if n = 1 then hd strlist
    else get_nth(tl strlist, n - 1)

fun date_to_string (date : int * int * int) =
    let val  month_names = ["January", "February", "March",
			    "April", "May", "June",
			    "July", "August", "September",
			    "October", "November", "December"]
        val  month_of_date = get_nth(month_names, #2 date)
    in 
	month_of_date ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, xs : int list) =
    let
        val rem_sum  = sum - hd xs
    in
        if (rem_sum > 0)
	then 1 + number_before_reaching_sum(rem_sum, tl xs)
	else 0
    end
(*
    if sum <= hd lst
    then 0
    else 1 + number_before_reaching_sum(sum - hd lst, tl lst)
*)    

fun what_month (day : int) =
    let
	val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	1 + number_before_reaching_sum(day, days_in_months)
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2 then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dates : (int * int * int) list) =
    if null dates then NONE
    else
	let val tail_oldest = oldest(tl dates)
            val head_dates = hd dates
        in
	    if isSome tail_oldest
            then
		if is_older(head_dates, valOf tail_oldest) then SOME head_dates
                else tail_oldest
            else SOME head_dates
        end
(* arguably better alternate solution avoiding isSome / valOf *)
(*fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else let fun f dates =
		 if null (tl dates)
		 then hd dates
		 else let val ans = f (tl dates)
		      in if is_older(ans, hd dates)
			 then ans
			 else hd dates
		      end
	 in SOME(f dates) end 
*)
