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
    printfn "Test 2: %b" (similar_names([["Fred"; "Fredrick"]; ["Elizabeth";"Betty"]; ["Freddie"; "Fred"; "F"]], {first="Elizabeth"; middle="W"; last="Smith"}) = 
                                [{first="Elizabeth"; last="Smith"; middle="W"}; {first="Betty"; last="Smith"; middle="W"}])
    printfn "Test 3: %b" (similar_names([["Fred"; "Fredrick"]; ["Betty";"Elizabeth"]; ["Freddie"; "Fred"; "F"]], {first="Elizabeth"; middle="W"; last="Smith"}) = 
                                    [{first="Elizabeth"; last="Smith"; middle="W"}; {first="Betty"; last="Smith"; middle="W"}])
    printfn "Test 4: %b" (similar_names([["Fred"; "Fredrick"]; ["Elizabeth";"Betty"]; ["Freddie"; "Fred"; "F"]], {first="Jon"; middle="W"; last="Smith"}) = 
                                 [{first="Jon"; last="Smith"; middle="W"}])                               
(* Task 2*)
    printfn "card_color:"
    printfn "Test1: %b" (card_color(Clubs, Jack) = Black)
    printfn "Test2: %b" (card_color(Diamonds, Jack) = Red)
    printfn "Test3: %b" (card_color(Hearts, Jack) = Red)
    printfn "Test4: %b" (card_color(Spades, Jack) = Black)
    printfn "card_value:"
    printfn "Test1: %b" (card_value(Clubs, Num 6) = 6)
    printfn "Test2: %b" (card_value(Clubs, Num 7) = 7)
    printfn "Test3: %b" (card_value(Clubs, Num 8) = 8)
    printfn "Test4: %b" (card_value(Clubs, Num 9) = 9)
    printfn "Test5: %b" (card_value(Clubs, Num 10) = 10)
    printfn "Test6: %b" (card_value(Clubs, Jack) = 10)
    printfn "Test7: %b" (card_value(Clubs, Queen) = 10)
    printfn "Test8: %b" (card_value(Clubs, King) = 10)
    printfn "Test9: %b" (card_value(Clubs, Ace) = 11)
    printfn "remove_card:"
    printfn "Test1: %b" (remove_card([(Clubs, Jack)], (Clubs, Jack), NoSuchCard) = [])
    printfn "Test2: %b" (remove_card([(Clubs, Ace); (Clubs, Jack); (Hearts, Jack); (Clubs, Jack)], (Clubs, Jack), NoSuchCard) = [(Clubs, Ace);(Hearts, Jack); (Clubs, Jack)])
    printfn "Test3: %b" (try ignore (remove_card([], (Clubs, Jack), NoSuchCard)); false
                          with NoSuchCard -> true | _ -> false)
    printfn "Test4: %b" (try ignore (remove_card([(Clubs, Ace); (Hearts, Jack)], (Clubs, Jack), NoSuchCard)); false
                          with NoSuchCard -> true | _ -> false)
    printfn "all_same_color:"
    printfn "Test1: %b" (all_same_color([]) = true)
    printfn "Test2: %b" (all_same_color([(Clubs, Ace)]) = true)
    printfn "Test3: %b" (all_same_color([(Clubs, Ace); (Hearts, Ace)]) = false)
    printfn "Test4: %b" (all_same_color([(Clubs, Ace); (Clubs, Ace)]) = true)
    printfn "sum_cards:"
    printfn "Test1: %b" (sum_cards([(Clubs, Num 7)]) = 7)
    printfn "Test2: %b" (sum_cards([]) = 0)
    printfn "Test3: %b" (sum_cards([(Clubs, Ace); (Clubs, Ace)]) = 22)
    printfn "Test4: %b" (sum_cards([(Clubs, King); (Clubs, Ace)]) = 21)
    printfn "Test5: %b" (sum_cards([(Clubs, King); (Clubs, Queen)]) = 20)
    printfn "Test6: %b" (sum_cards([(Clubs, King); (Clubs, Queen); (Clubs, Num 10)]) = 30)
    printfn "Test7: %b" (sum_cards([(Clubs, Jack); (Clubs, Queen); (Clubs, Num 10)]) = 30)
    printfn "score:"
    printfn "Test1: %b" (score([(Clubs, Num 7)], 7) = 0)
    printfn "Test2: %b" (score([(Hearts, Num 2); (Clubs, Num 4)], 10) = 4)
    printfn "Test3: %b" (score([(Hearts, Num 2); (Clubs, Num 4)], 2) = 12)
    printfn "Test4: %b" (score([(Hearts, Num 2); (Hearts, Num 4)], 2) = 6)
    printfn "Test5: %b" (score([(Hearts, Num 2); (Hearts, Ace)], 2) = 16)
    printfn "officiate:"
    printfn "Test1: %b" (officiate ([(Hearts, Num 2); (Clubs, Num 4)], [Draw], 15) = 6)
    printfn "Test2: %b" (officiate([(Clubs,Ace); (Spades,Ace); (Clubs,Ace); (Spades,Ace)],
                        [Draw; Draw; Draw; Draw; Draw], 42) =
                        3)
    printfn "Test3: %b" (try 
                            ignore (officiate([(Clubs, Jack); (Spades, Num(8))], [Draw; Discard(Hearts, Jack)],42));
                            false
                         with IllegalMove -> true | _ -> false) 
                         
tests()