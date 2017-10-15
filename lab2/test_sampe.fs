module test
open hw2;;

(* 
Some trivial tests are allredy done for you. You should add more tests here.
Although tests won't be graded, make sure that you solution is tested well.
*)

exception NoSuchCard

let tests () =
(* Task 1*)
    printfn "all_except_option:"
    printfn "Test 1: %b" (all_except_option("", []) = None)
    printfn "Test 2: %b" (all_except_option("name", ["Bob"; "Jon"; "Loky"]) = None)
    printfn "Test 3: %b" (all_except_option("Bob", ["Bob"; "Jon"; "Loky"]) = Some(["Jon";"Loky"]))
    printfn "Test 4: %b" (all_except_option("Jon", ["Bob"; "Jon"; "Loky"]) = Some(["Bob";"Loky"]))
    printfn "Test 5: %b" (all_except_option("Loky", ["Bob"; "Jon"; "Loky"]) = Some(["Bob";"Jon"]))
    printfn "get_substitutions1:"
    printfn "Test 1: %b" (get_substitutions1([["Fred"; "Fredrick"]; ["Elizabeth";"Betty"]; ["Freddie";"Fred";"F"]], "Fred") = ["Fredrick"; "Freddie"; "F"])
    printfn "Test 2: %b" (get_substitutions1([["Fred"; "Fredrick"]; ["Elizabeth";"Betty";"Fred"]; ["Freddie";"Fred";"F"]], "Fred") = ["Fredrick";"Elizabeth";"Betty";"Freddie"; "F"])
    printfn "Test 3: %b" (get_substitutions1([["Fred"; "Fredrick"]; ["Elizabeth";"Betty";]; ["Freddie";"Fred";"F"]], "Betty") = ["Elizabeth"])
    printfn "Test 4: %b" (get_substitutions1([["Fred"; "Fredrick"]; ["Elizabeth";"Betty";]; ["Freddie";"Fred";"F"]], "Jon") = [])
    printfn "Test 5: %b" (get_substitutions1([["Fredrick"]; ["Elizabeth";"Betty";]; ["Freddie";"Fred";"F"]], "Fredrick") = [])
    printfn "Test 6: %b" (get_substitutions1([[]; []], "Fredrick") = [])
    printfn "Test 7: %b" (get_substitutions1([[]; []], "") = [])
    printfn "get_substitutions2:"
    printfn "Test 1: %b" (get_substitutions2([["Fred"; "Fredrick"]; ["Elizabeth";"Betty"]; ["Freddie";"Fred";"F"]], "Fred") = ["Fredrick"; "Freddie"; "F"])
    printfn "Test 2: %b" (get_substitutions2([["Fred"; "Fredrick"]; ["Elizabeth";"Betty";"Fred"]; ["Freddie";"Fred";"F"]], "Fred") = ["Fredrick";"Elizabeth";"Betty";"Freddie"; "F"])
    printfn "Test 3: %b" (get_substitutions2([["Fred"; "Fredrick"]; ["Elizabeth";"Betty";]; ["Freddie";"Fred";"F"]], "Betty") = ["Elizabeth"])
    printfn "Test 4: %b" (get_substitutions2([["Fred"; "Fredrick"]; ["Elizabeth";"Betty";]; ["Freddie";"Fred";"F"]], "Jon") = [])
    printfn "Test 5: %b" (get_substitutions2([["Fredrick"]; ["Elizabeth";"Betty";]; ["Freddie";"Fred";"F"]], "Fredrick") = [])
    printfn "Test 6: %b" (get_substitutions2([[]; []], "Fredrick") = [])
    printfn "Test 7: %b" (get_substitutions2([[]; []], "") = [])
    printfn "similar_names:"
    printfn "Test 1: %b" (similar_names([["Fred"; "Fredrick"]; ["Elizabeth";"Betty"]; ["Freddie"; "Fred"; "F"]], {first="Fred"; middle="W"; last="Smith"}) = 
                            [{first="Fred"; last="Smith"; middle="W"}; {first="Fredrick"; last="Smith"; middle="W"};
                            {first="Freddie"; last="Smith"; middle="W"};
                            {first="F"; last="Smith"; middle="W"}])
(* Task 2*)
    (*printfn "card_color:"
    printfn "Test 1: %b" (card_color(Clubs, Jack) = Black)
    printfn "card_value:"
    printfn "Test 1: %b" (card_value(Clubs, Num 7) = 7)
    printfn "remove_card:"
    printfn "Test 1: %b" (remove_card([(Clubs, Jack)], (Clubs, Jack), NoSuchCard) = [])
    printfn "Test 2: %b" (try ignore (remove_card([], (Clubs, Jack), NoSuchCard)); false
                          with NoSuchCard -> true | _ -> false)
    printfn "all_same_color:"
    printfn "Test1: %b" (all_same_color([]) = true)
    printfn "sum_cards:"
    printfn "Test1: %b" (sum_cards([(Clubs, Num 7)]) = 7)
    printfn "score:"
    printfn "Test1: %b" (score([(Clubs, Num 7)], 7) = 0)
    printfn "Test2: %b" (score([(Hearts, Num 2); (Clubs, Num 4)], 10) = 4)
    printfn "officiate:"
    printfn "Test1: %b" (officiate ([(Hearts, Num 2); (Clubs, Num 4)], [Draw], 15) = 6)
    printfn "Test2: %b" (officiate([(Clubs,Ace); (Spades,Ace); (Clubs,Ace); (Spades,Ace)],
                        [Draw; Draw; Draw; Draw; Draw], 42) =
                        3)
    printfn "Test3: %b" (try 
                            ignore (officiate([(Clubs, Jack); (Spades, Num(8))], [Draw; Discard(Hearts, Jack)],42));
                            false
                         with IllegalMove -> true | _ -> false) *)
                         
tests()