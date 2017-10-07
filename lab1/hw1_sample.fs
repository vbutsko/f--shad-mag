module hw1

let year (a, _, _) = a
let month (_, a, _) = a
let day (_, _, a) = a

(*
Add your code here
*)

let is_older ( date1 : (int * int * int) , date2 : (int * int * int) ) : bool = 
    if year(date1) <> year(date2) 
    then year(date1) < year(date2)
    elif month(date1) <> month(date2)
    then month(date1) < month(date2)
    else day(date1) < day(date2)

let rec number_in_month(dates : (int * int * int) list , month_num : int) : int =
    if dates.IsEmpty
    then 0
    else 
        if month(dates.Head) = month_num 
        then 1 
        else 0 
        + number_in_month(dates.Tail , month_num) 
    
let rec number_in_months (dates : (int * int * int) list, month_nums : int list) : int =
    if month_nums.IsEmpty
    then 0
    else number_in_month(dates, month_nums.Head) + number_in_months(dates, month_nums.Tail)
    
let rec dates_in_month(dates : (int * int * int) list , month_num : int) : (int * int * int) list =
    if dates.IsEmpty
    then list.Empty
    else 
        if month(dates.Head) = month_num 
        then dates.Head::dates_in_month(dates.Tail, month_num) 
        else dates_in_month(dates.Tail, month_num)
       
let rec dates_in_months(dates : (int * int * int) list, month_nums : int list) : (int * int * int) list  =
    if month_nums.IsEmpty
    then list.Empty
    else dates_in_month(dates, month_nums.Head) @ dates_in_months(dates, month_nums.Tail)
    
let rec get_nth(str_list: string list, n: int) : string =
    if n = 1
    then str_list.Head
    else get_nth(str_list.Tail, n - 1)

let date_to_string(date : int * int * int ) : string = 
    let months: string list = [ "January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"; ]
    get_nth(months, month(date))+ " " + day(date).ToString() + ", " + year(date).ToString()                                                 

let rec number_before_reaching_sum(sum: int, nums: int list) : int = 
    if sum <= nums.Head
    then 0
    else 1 + number_before_reaching_sum(sum - nums.Head, nums.Tail)
    
let what_month(day_in_year: int) : int =
    1 + number_before_reaching_sum(day_in_year, [31;28;31;30;31;30;31;31;30;31;30;31])
    
let rec month_range(day1 : int, day2 : int) : int list =
    if day1 < day2
    then what_month(day1)::month_range(day1 + 1, day2)
    else what_month(day1)::list.Empty
    
let rec oldest(dates : (int * int * int) list): (int * int * int) option =
    if dates.IsEmpty
    then None
    else 
        let result : (int * int * int) option = oldest( dates.Tail )
        in if result.IsSome && is_older(result.Value, dates.Head)
           then result
           else Some(dates.Head)

let number_in_months_2(dates : (int * int * int) list, month_nums : int list) : int =
    let rec is_member(x : int, xs : int list) : bool =
        if xs.IsEmpty
        then false
        elif xs.Head = x
        then true
        else is_member(x, xs.Tail)
            
    let rec remove_dublicates(xs : int list) : int list =
        if xs.IsEmpty
        then list.Empty
        else
        let xs_temp : int list = remove_dublicates(xs.Tail)
        in if is_member(xs.Head, xs_temp)
           then xs_temp
           else xs.Head::xs_temp
    
    number_in_months(dates, remove_dublicates(month_nums))
    
let dates_in_months_2(dates : (int * int * int) list, month_nums : int list) : (int * int * int) list =
    let rec is_member(x : int, xs : int list) : bool =
        if xs.IsEmpty
        then false
        elif xs.Head = x
        then true
        else is_member(x, xs.Tail)
    
    let rec remove_dublicates(xs : int list) : int list =
        if xs.IsEmpty
        then list.Empty
        else
        let xs_temp : int list = remove_dublicates(xs.Tail)
        in if is_member(xs.Head, xs_temp)
           then xs_temp
           else xs.Head::xs_temp
    
    dates_in_months(dates, remove_dublicates(month_nums))

let reasonable_date(date : (int * int * int)) : bool = 
    let month_length1 : string list = ["31";"28";"31";"30";"31";"30";"31";"31";"30";"31";"30";"31"]
    let month_length2 : string list = ["31";"29";"31";"30";"31";"30";"31";"31";"30";"31";"30";"31"]
    if year(date) < 1 || month(date) > 12 || month(date) < 1 || day(date) < 1
    then false
    elif year(date) % 400 = 0 || (year(date) % 100 > 0 && year(date) % 4 = 0)
    then day(date) <= System.Int32.Parse(get_nth(month_length2, month(date)))
    else day(date) <= System.Int32.Parse(get_nth(month_length1, month(date)))