module hw3
open System

exception NoAnswer

type pattern = Wildcard | Variable of string | UnitP | ConstP of int
              | TupleP of pattern list | ConstructorP of string * pattern

type value = Const of int | Unit | Tuple of value list
              | Constructor of string * value

let rec g f1 f2 p =
    let r = g f1 f2
    in match p with
      | Wildcard          -> f1 ()
      | Variable x        -> f2 x
      | TupleP ps         -> List.foldBack (fun p i -> (r p) + i) ps 0
      | ConstructorP(_,p) -> r p
      | _                 -> 0

(**** for the challenge problem only ****)

type typ =
      | Anything
      | UnitT
      | IntT
      | TupleT of typ list
      | Type of string


let only_capitals(capitals : string list)  : string list =
    let check_upper (word : string) : bool =
        if word = ""
        then false
        else Char.IsUpper(word.[0])
    List.filter check_upper capitals
    
let longest_string1(strings : string list) : string =
    List.fold (fun (acc:string) (elem: string) -> if acc.Length < elem.Length then elem else acc) "" strings
    
let longest_string2(strings : string list) : string =
    List.fold (fun (acc:string) (elem: string) -> if acc.Length <= elem.Length then elem else acc) "" strings
    
(*  OR
let longest_string2(strings : string list) : string =
    List.foldBack (fun (elem: string) (acc:string) -> if acc.Length < elem.Length then elem else acc) strings ""
*)
    
let longest_string_helper(f, strings : string list) : string =
    List.fold (fun (acc:string) (elem: string) -> if f acc.Length elem.Length then elem else acc) "" strings
    
let longest_string3 (strings : string list) : string = 
    longest_string_helper((fun (x:int) (y:int) -> x < y), strings)
    
let longest_string4 (strings : string list) : string = 
    longest_string_helper((fun (x:int) (y:int) -> x <= y), strings)

let longest_capitalized (strings : string list) : string = 
    (only_capitals >> longest_string1) strings
    
let rev_string(str : string) : string = 
     String(Array.rev <| str.ToCharArray())
   
let rec first_answer f xs = 
    if xs = []
    then raise NoAnswer
    else match f(xs.Head) with
        | None -> first_answer f xs.Tail
        | Some(v) -> v
                
let rec all_answers f xs =
    if xs = []
    then Some []
    else match f(xs.Head) with
        | None -> None
        | Some(v) -> match all_answers f xs.Tail with
            | None -> None
            | Some(lst) -> Some(xs.Head::lst)
            
let rec count_wildcards (p : pattern) : int = 
    g (fun x -> 1) (fun x -> 0) p 

let rec count_wild_and_variable_lengths (p : pattern) : int = 
    g (fun x -> 1) (fun x -> x.Length) p 

let rec count_some_var (s : string, p : pattern) : int = 
     g (fun x -> 0) (fun x -> if x = s then 1 else 0) p