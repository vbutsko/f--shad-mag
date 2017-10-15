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
            
                

let card_color( (suit, rank) : card) =
    match suit with 
        | Clubs | Spades -> Black
        | Diamonds | Hearts -> Red
        
let card_value((suit, rank) : card) =
    match rank with
        |Jack | Queen | King -> 10
        |Ace -> 11
        |Num(x) -> x 
        
let rec remove_card(cs: card list, (suit, rank) : card, e) =
    match cs with
        | [] -> raise e
        | (s,r)::cs' -> 
            if s = suit && r = rank
            then
                cs'
            else
                (s,r)::remove_card(cs', (suit, rank), e)
                
let rec all_same_color(cs) = 
    match cs with 
    | [] -> true
    | c::cs ->
        let colr = card_color(c)
        let rec same_color(cs_) =
            match cs_ with
                | [] -> true
                | c::cs_ -> 
                    if card_color(c) = colr
                    then same_color(cs_)
                    else false
        in same_color(cs)
    
let sum_cards(cs) =
    let rec aux(cs, sum) =
        match cs with
            | [] -> sum
            | c::cs -> aux(cs, sum + card_value(c))
    in aux(cs, 0)
    
let score(held_cards, goal) =
    let sum = sum_cards(held_cards)
    let total_score = 
        if sum > goal 
        then 3 * (sum - goal)
        else goal - sum
    in  if all_same_color(held_cards)
        then total_score / 2
        else total_score
        
let officiate(cs, ml, goal) =
    let rec make_move(held_cards, moves) =
        match moves with
            | [] -> score(held_cards, goal)
            | mv::moves ->
                match mv with
                    | Discard c ->
                        make_move(remove_card(held_cards, c, IllegalMove), moves)
                    | Draw ->
                        match cs with 
                            | [] -> score(held_cards, goal)
                            | c::cs -> 
                                if sum_cards(c::held_cards) > goal
                                then score(c::held_cards, goal)
                                else make_move(c::held_cards, moves)
    in make_move([], ml)
        