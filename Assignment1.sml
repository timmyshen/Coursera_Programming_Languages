fun is_older (x : int * int *int, y : int * int * int) =
    if #1 x < #1 y then true
    else if #1 x > #1 y then false
    else if #2 x < #2 y then true
    else if #2 x > #2 y then false
    else if #3 x < #3 y then true
    else false

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates then 0
    else
	if #2 (hd(dates)) = month then 1 + number_in_month(tl dates, month)
	else number_in_month(tl dates, month)

fun number_in_months (dates : (int * int * int) list, months: int list) =
    if null months then 0
    else
	number_in_month(dates, hd months) +  number_in_months(dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates then []
    else
	if #2 (hd dates) = month then [hd dates] @ dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months then []
    else
	dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth (strlist : string list, n : int) =
    if n = 1 then hd strlist
    else get_nth(tl strlist, n - 1)

fun date_to_string (date : int * int * int) =
    let val  month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
        val  month_of_date = get_nth(month_names, #2 date)
    in 
    month_of_date ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum : int, xs : int list) =
    let
        val rem_sum  = sum - hd xs
    in
        if (rem_sum > 0) then 1 + number_before_reaching_sum(rem_sum, tl xs) else 0
    end
    

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
