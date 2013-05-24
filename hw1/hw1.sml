fun is_older(first : int*int*int, second : int*int*int)=
    if #1 first < #1 second
    then true
    else if #1 first = #1 second andalso #2 first < #2 second
    then true
    else if #1 first = #1 second andalso #2 first = #2 second
    andalso #3 first < #3 second
    then true
    else false

fun number_in_month(dates : (int*int*int) list, month : int)=
    if null dates then 0
    else if #2 (hd dates) = month
    then   1+number_in_month((tl dates), month)
    else number_in_month((tl dates),month)

fun number_in_months(dates : (int*int*int) list, months : int list)=
    if null months
    then 0
    else number_in_month(dates, hd months) +
                number_in_months(dates, tl months)

fun dates_in_month(dates : (int*int*int) list, month : int)=
    if null dates then []
    else if (#2 (hd dates)) = month
    then hd dates :: dates_in_month((tl dates), month)
    else dates_in_month((tl dates), month)

fun dates_in_months(dates : (int*int*int) list, months : int list)=
    if null months then []
    else dates_in_month(dates, hd months) @
                                    dates_in_months(dates, tl months)

fun get_nth(strings : string list, n : int)=
    if n=1
    then hd strings
    else get_nth((tl strings), n-1)

fun date_to_string(date : int*int*int)=
    let
        val months=["January","February","March","April","May","June",
                    "July","August","September","October","November",
                    "December"]
        val month = get_nth(months, #2 date)
    in
        month^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum : int, sum_list : int list)=
    let
        fun calculate(count : int , sum : int, sum_list : int list)=
            if sum -(hd sum_list) <= 0
            then count
            else calculate(count+1, (sum - hd sum_list), tl sum_list)
    in
        calculate(0, sum, sum_list)
    end

fun number_before_reaching_sum_helper(sum : int, sum_list : int list)=
    let
        fun calculate(count : int , sum : int, sum_list : int list)=
            if sum -(hd sum_list) <= 0
            then count
            else calculate(count+1, (sum - hd sum_list), tl sum_list)
    in
        calculate(1, sum, sum_list)
    end

fun what_month(day : int)=
    let
        val month_list=[31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum_helper(day, month_list)
    end

fun month_range(day1 : int, day2 : int)=
    if day1 > day2 then []
    else
        let
            fun calculate(day_start : int, day_end : int)=
                if day_start > day_end then []
                else
                    what_month(day_start)::
                            calculate(day_start+1,day_end)
        in
            calculate(day1, day2)
        end

fun oldest(dates : (int*int*int) list)=
    if null dates
    then NONE
    else
        let
            val oldest_date = hd dates
        in
            let
                fun calculate(most : int*int*int, dates : (int*int*int) list)=
                    if null dates then SOME most
                    else if (#1 most > #1(hd dates)) orelse
                       (#1 most = #1(hd dates) andalso #2 most > #2(hd dates))
                        orelse
                    (#1 most = #1(hd dates) andalso #2 most = #2(hd dates)
                        andalso #3 most > #3(hd dates))
                    then
                        let
                            val most = hd dates
                        in
                            calculate(most, tl dates)
                        end
                    else calculate(most, tl dates)
            in
                calculate(oldest_date, tl dates)
            end
        end

fun already_exit(month : int, months : int list)=
    if null months
    then false
    else if hd months = month
    then true
    else already_exit(month, tl months)

fun single(months : int list)=
    if null months
    then []
    else if already_exit(hd months, tl months)
    then single(tl months)
    else hd months :: single(tl months)

fun number_in_months_challenge(dates : (int*int*int) list, months : int list)=
    number_in_months(dates, single(months))

fun dates_in_months_challenge(dates : (int*int*int) list, months : int list)=
    dates_in_months(dates, single(months))

fun is_leap_year(year : int)=
    (year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0)

fun reasonable_date(date : int*int*int )=
    if #1 date <= 0
    then false
    else if #2 date > 12 orelse #2 date < 1
    then false
    else
        let
            val month_list = [1,3,5,7,8,10,12]
        in
            let
                fun find_month(month : int, months : int list)=
                    if null months
                    then false
                    else if hd months = month
                    then true
                    else find_month(month, tl months)
            in
                if find_month(#2 date, month_list)
                then
                    if #3 date < 1 orelse #3 date > 31
                    then false
                    else true
                else if #2 date = 2
                then
                    if is_leap_year(#1 date)
                    then
                        if #3 date < 1 orelse #3 date > 29
                        then false
                        else true
                    else
                        if #3 date < 1 orelse #3 date > 28
                        then false
                        else true
                else
                    if #3 date < 1 orelse #3 date > 30
                    then false
                    else true
            end
        end

