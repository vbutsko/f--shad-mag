module hw2

type Name = {first: string; middle: string; last: string}



type suit = Clubs | Diamonds | Hearts | Spades
type rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

type color = Red | Black
type move = Discard of card | Draw

exception IllegalMove
