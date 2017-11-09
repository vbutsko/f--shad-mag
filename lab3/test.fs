module test

open hw3

(* 
Some trivial tests are allredy done for you. You should add more tests here.
Although tests won't be graded, make sure that you solution is tested well.
*)


let tests () =
    printfn "only_capitals:"
    printfn "Test 1: %b" (only_capitals(["A"; "b"]) = ["A"])
    printfn "Test 2: %b" (only_capitals(["a"; "b"]) = [])
    printfn "Test 3: %b" (only_capitals(["A"; "B"]) = ["A"; "B"])
    printfn "Test 4: %b" (only_capitals(["aA"; "bB"]) = [])
    printfn "Test 5: %b" (only_capitals(["Ab"; "aB"]) = ["Ab"])
    printfn "Test 6: %b" (only_capitals(["aaa"; "BBB"]) = ["BBB"])
    printfn "Test 7: %b" (only_capitals(["AbA"; "aBa"]) = ["AbA"])
    printfn "longest_string1:"
    printfn "Test 1: %b" (longest_string1(["A"; "b"]) = "A")
    printfn "Test 2: %b" (longest_string1(["Aa"; "b"]) = "Aa")
    printfn "Test 3: %b" (longest_string1(["Aa"; "bbb"; "Aba"; "aa"]) = "bbb")
    printfn "Test 4: %b" (longest_string1([""]) = "")
    printfn "Test 5: %b" (longest_string1([]) = "")
    printfn "longest_string2:"
    printfn "Test 1: %b" (longest_string2(["A"; "b"]) = "b")
    printfn "Test 2: %b" (longest_string2(["Aa"; "b"]) = "Aa")
    printfn "Test 3: %b" (longest_string2(["Aa"; "bbb"; "aba"; "aa"]) = "aba")
    printfn "Test 4: %b" (longest_string2([""]) = "")
    printfn "Test 5: %b" (longest_string2([]) = "")
    printfn "longest_string3:"
    printfn "Test 1: %b" (longest_string3(["A"; "b"]) = "A")
    printfn "Test 2: %b" (longest_string3(["Aa"; "b"]) = "Aa")
    printfn "Test 3: %b" (longest_string3(["Aa"; "bbb"; "Aba"; "aa"]) = "bbb")
    printfn "Test 4: %b" (longest_string3([""]) = "")
    printfn "Test 5: %b" (longest_string3([]) = "")
    printfn "longest_string4:"
    printfn "Test 1: %b" (longest_string4(["A"; "b"]) = "b")
    printfn "Test 2: %b" (longest_string4(["Aa"; "b"]) = "Aa")
    printfn "Test 3: %b" (longest_string4(["Aa"; "bbb"; "aba"; "aa"]) = "aba")
    printfn "Test 4: %b" (longest_string4([""]) = "")
    printfn "Test 5: %b" (longest_string4([]) = "")
    printfn "longest_capitallized:"
    printfn "Test 1: %b" (longest_capitalized(["A"; "b"]) = "A")
    printfn "Test 2: %b" (longest_capitalized(["Aa"; "b"]) = "Aa")
    printfn "Test 3: %b" (longest_capitalized(["aaa"; "Ab"]) = "Ab")
    printfn "Test 4: %b" (longest_capitalized(["aaa"; "Aab"]) = "Aab")
    printfn "Test 5: %b" (longest_capitalized([""; "A"]) = "A")
    printfn "Test 6: %b" (longest_capitalized([""; "a"]) = "")
    printfn "Test 7: %b" (longest_capitalized(["a"]) = "")
    printfn "Test 8: %b" (longest_capitalized([]) = "")
    printfn "rev_string:"
    printfn "Test 1: %b" (rev_string "AbC" = "CbA")
    printfn "Test 2: %b" (rev_string "A" = "A")
    printfn "Test 3: %b" (rev_string "" = "")
    printfn "Test 4: %b" (rev_string "A B" = "B A")
    printfn "Test 5: %b" (rev_string "AB" = "BA")
    printfn "first_answer:"
    printfn "Test 1: %b" (first_answer (fun x -> if x > 1 then Some x else None) [3] = 3)
    printfn "Test 2: %b" (try ignore (first_answer (fun x -> if x > 1 then Some x else None) [0] ); false
                          with NoAnswer -> true | _ -> false)
    printfn "all_answers:"
    printfn "Test 1: %b" (all_answers (fun x -> if x > 1 then Some [x] else None) [] = Some [])
    printfn "Test 2: %b" (all_answers (fun x -> if x > 1 then Some [x] else None) [3; 4] = Some [3; 4])
    printfn "count_wildcards:"
    printfn "Test 1: %b" (count_wildcards Wildcard = 1)
    printfn "Test 2: %b" (count_wildcards (TupleP [Wildcard; ConstP 42]) = 1)
    (*printfn "count_wild_and_variable_lengths:"
    printfn "Test 1: %b" (count_wild_and_variable_lengths Wildcard = 1)
    printfn "Test 2: %b" (count_wild_and_variable_lengths (TupleP [Wildcard; Variable "xy"]) = 3)
    printfn "count_some_var:"
    printfn "Test 1: %b" (count_some_var ("x", Variable "x") = 1)
    printfn "check_pat:"
    printfn "Test 1: %b" (check_pat (TupleP [Variable "x"; Variable "x"]) = false)
    printfn "match_pat:"
    printfn "Test 1: %b" (match_pat (Tuple [Const 42], TupleP [Wildcard]) = Some [] )
    printfn "Test 2: %b" (match_pat (Tuple [Const 42], Variable "x") = Some ["x", Tuple [Const 42]] )
    printfn "first_match:"
    printfn "Test 1: %b" (first_match (Tuple [Const 42]) [TupleP [Wildcard]] = Some [] )*)
    
tests()