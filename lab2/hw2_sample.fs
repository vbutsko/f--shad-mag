module hw2

type Name = {first: string; middle: string; last: string}



type suit = Clubs | Diamonds | Hearts | Spades
type rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

type color = Red | Black
type move = Discard of card | Draw

exception IllegalMove

let rec all_except_option(name: string, name_list: string list)=
    match name_list with
        | [] -> None
        | first_name::name_list' -> 
            if first_name = name
            then Some(name_list') 
            else match all_except_option(name, name_list') with         
                | Some names -> Some(first_name::names)
                | None -> None

let rec get_substitutions1(substitutions: string list list, str: string) : string list =
    match substitutions with
        | [] -> []
        | sub::substitutions' -> 
            match all_except_option(str, sub) with 
                | None -> get_substitutions1(substitutions', str)
                | Some(result) -> result@get_substitutions1(substitutions', str)
            
let get_substitutions2(substitutions: string list list, str: string) : string list =
    let rec aux(substitutions, result_list) = 
        match substitutions with
            | [] -> result_list
            | sub::substitutions' -> 
                match all_except_option(str, sub) with
                    | None -> aux(substitutions', result_list)
                    | Some(result) -> aux(substitutions', result_list@result)
    in aux(substitutions, [])
    
let rec similar_names(substitutions, full_name) =
    match full_name with
        |{first = f; middle = m; last = l} ->
            let rec aux(names : string list, result_list : Name list) =
                match names with
                    | [] -> result_list
                    | name::names' -> aux(names', result_list@[{first = name; middle = m; last = l}])
            in aux(get_substitutions2(substitutions, f), [{first = f; middle = m; last = l}])
            
                
(*
let card_color( card_1: card ) =
    match card_1 with 
        |suit rank
        |
    *)
    
