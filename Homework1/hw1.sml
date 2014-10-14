(*Assignment 1*)

(*
    The function takes two dates and evaluates to true or false.
    It evaluates to true if the first argument is a date that 
    comes before the second argument.
    (If the two dates are the same, the result is false.)
    *)
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


(*
    The function takes a list of dates and a month (i.e., an int) and
    returns how many dates in the list are in the given month.
    *)
fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates then 0
    else
        let
            val sub_number_in_month = number_in_month(tl dates, month)
        in
            if #2 (hd dates) = month
            then
                sub_number_in_month + 1
            else
                sub_number_in_month
        end


(*
    The function takes a list of dates and a list of months
    (i.e., an int list) and returns the number of dates in the list of dates
    that are in any of the months in the list of months.
    Assume the list of months has no number repeated.
    Hint: Use your answer to the previous problem.
    *)
fun number_in_months (dates : (int * int * int) list, months: int list) =
    if null months then 0
    else
        number_in_month(dates, hd months) +  number_in_months(dates, tl months)


(*
    The function takes a list of dates and a month (i.e., an int) and
    returns a list holding the dates from the argument list of dates that
    are in the month. The returned list should contain dates
    in the order they were originally given.
    *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates then []
    else
        if #2 (hd dates) = month
        then
            (hd dates) :: dates_in_month(tl dates, month)
        else
            dates_in_month(tl dates, month)


(*
    The function takes a list of dates and a list of months (i.e., an int list)
    and returns a list holding the dates from the argument list of dates
    that are in any of the months in the list of months.
    Assume the list of months has no number repeated.
    Hint: Use your answer to the previous problem and
    SML’s list-append operator (@).
    *)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months then []
    else
        dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


(*
    The function takes a list of strings and an int n and
    returns the n th element of the list where the head of the list is 1 st .
    Do not worry about the case where the list has too few elements:
    your function may apply hd or tl to the empty list in this case,
    which is okay.
    *)
fun get_nth (strlist : string list, n : int) =
    if n = 1 then hd strlist
    else get_nth(tl strlist, n - 1)


(*
    The function takes a date and returns a string of the form January 20, 2013
    (for example). Use the operator ^ for concatenating strings and the library
    function Int.toString for converting an int to a string.
    For producing the month part, do not use a bunch of conditionals.
    Instead, use a list holding 12 strings and
    your answer to the previous problem. For consistency,
    put a comma following the day and use capitalized English month names:
    January, February, March, April, May, June,
    July, August, September, October, November, December.
    *)
fun date_to_string (date : int * int * int) =
    let
        val  month_names =
            ["January", "February", "March",
            "April", "May", "June",
            "July", "August", "September",
            "October", "November", "December"]
        val  month_of_date = get_nth(month_names, #2 date)
    in 
        month_of_date ^ " " ^
        Int.toString(#3 date) ^ ", " ^
        Int.toString(#1 date)
    end


(*
    The function takes an int called sum, which you can assume
    is positive, and an int list, which you can assume contains
    all positive numbers, and returns an int.
    You should return an int n such that the first n elements of the list
    add to less than sum, but the first n + 1 elements of the list
    add to sum or more.
    Assume the entire list sums to more than the passed in value;
    it is okay for an exception to occur if this is not the case.
    *)
fun number_before_reaching_sum (sum : int, xs : int list) =
    let
        val rem_sum  = sum - hd xs
    in
        if rem_sum > 0
        then 1 + number_before_reaching_sum(rem_sum, tl xs)
        else 0
    end


(*
    The function takes a day of year (i.e., an int between 1 and 365) and
    returns what month that day is in (1 for January, 2 for February, etc.).
    Use a list holding 12 integers and your answer to the previous problem.
    *)
fun what_month (day : int) =
    let
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        1 + number_before_reaching_sum(day, days_in_months)
    end


(*
    The function takes two days of the year day1 and day2 and returns
    an int list [m1,m2,...,mn] where m1 is the month of day1,
    m2 is the month of day1+1, ..., and mn is the month of day day2.
    Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.
    *)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2 then []
    else what_month(day1) :: month_range(day1 + 1, day2)


(*
    The function takes a list of dates and evaluates to an (int*int*int) option.
    It evaluates to NONE if the list has no dates and
    SOME d if the date d is the oldest date in the list.
    *)
fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
        let
            val tail_oldest = oldest(tl dates)
            val head_dates = hd dates
        in
            if isSome tail_oldest
            then
                if is_older(head_dates, valOf tail_oldest)
                then SOME head_dates
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


(*
    Write functions number_in_months_challenge and dates_in_months_challenge
    that are like your solutions to problems 3 and 5 except
    having a month in the second argument multiple times has
    no more effect than having it once.
    (Hint: Remove duplicates, then use previous work.)
    *)
fun list_contains (xs : int list, x : int) =
    if null xs then false
    else
        (hd xs) = x orelse list_contains(tl xs, x)


fun remove_duplicates (xs : int list) =
    let
        fun helper (xs : int list, results : int list) =
            if null xs then results
            else
                if list_contains(results, hd xs)
                then helper(tl xs, results)
                else helper(tl xs, results @ [hd xs])
    in
        helper(xs, [])
    end


fun number_in_months_challenge
    (dates : (int * int * int) list, months : int list) =
    let
        val unique_months = remove_duplicates(months)
    in
        number_in_months(dates, unique_months)
    end


fun dates_in_months_challenge
    (dates : (int * int * int) list, months : int list) =
    let
        val unique_months = remove_duplicates(months)
    in
        dates_in_months(dates, unique_months)
    end

(*
    Write a function reasonable_date that takes a date and determines if it
    describes a real date in the common era. A “real date” has a positive year
    (year 0 did not exist), a month between 1 and 12, and a day appropriate
    for the month. Solutions should properly handle leap years.
    Leap years are years that are either divisible by 400 or divisible by 4
    but not divisible by 100.
    (Do not worry about days possibly lost in the conversion to the
    Gregorian calendar in the Late 1500s.)
    *)
fun is_leap_year (year : int) =
    if year <= 0 then false
    else
        (year mod 400 = 0) orelse
        (year mod 4 = 0 andalso not (year mod 100 = 0))
        

fun get_nth_int (intlist : int list, n : int) =
    if n = 1 then hd intlist
    else get_nth_int (tl intlist, n - 1)


fun reasonable_date (date : int * int * int) =
    let
        val year = #1 date
        val month = #2 date
        val date = #3 date
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val days_in_months_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        if not (year > 0 andalso month >= 1 andalso month <= 12)
        then false
        else
            if is_leap_year(year)
            then (*todo: deal with leap year*)
                date <= get_nth_int(days_in_months_leap, month)
                andalso date >= 1
            else (*todo: deal with normal year*)
                date <= get_nth_int(days_in_months, month)
                andalso date >= 1
    end