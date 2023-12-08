open! Base
open! Stdio

let print_sexp s = Sexp.to_string_hum s |> printf "%s\n"

let tap x ~f =
  f x;
  x

(*--- Day 7: Camel Cards ---
  Your all-expenses-paid trip turns out to be a one-way, five-minute ride in an airship. (At least it's a cool airship!) It drops you off at the edge of a vast desert and descends back to Island Island.

  "Did you bring the parts?"

  You turn around to see an Elf completely covered in white clothing, wearing goggles, and riding a large camel.

  "Did you bring the parts?" she asks again, louder this time. You aren't sure what parts she's looking for; you're here to figure out why the sand stopped.

  "The parts! For the sand, yes! Come with me; I will show you." She beckons you onto the camel.

  After riding a bit across the sands of Desert Island, you can see what look like very large rocks covering half of the horizon. The Elf explains that the rocks are all along the part of Desert Island that is directly above Island Island, making it hard to even get there. Normally, they use big machines to move the rocks and filter the sand, but the machines have broken down because Desert Island recently stopped receiving the parts they need to fix the machines.

  You've already assumed it'll be your job to figure out why the parts stopped when she asks if you can help. You agree automatically.

  Because the journey will take a few days, she offers to teach you the game of Camel Cards. Camel Cards is sort of similar to poker except it's designed to be easier to play while riding a camel.

  In Camel Cards, you get a list of hands, and your goal is to order them based on the strength of each hand. A hand consists of five cards labeled one of A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2. The relative strength of each card follows this order, where A is the highest and 2 is the lowest.

  Every hand is exactly one type. From strongest to weakest, they are:

  Five of a kind, where all five cards have the same label: AAAAA
  Four of a kind, where four cards have the same label and one card has a different label: AA8AA
  Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
  Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
  Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
  One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
  High card, where all cards' labels are distinct: 23456
  Hands are primarily ordered based on type; for example, every full house is stronger than any three of a kind.

  If two hands have the same type, a second ordering rule takes effect. Start by comparing the first card in each hand. If these cards are different, the hand with the stronger first card is considered stronger. If the first card in each hand have the same label, however, then move on to considering the second card in each hand. If they differ, the hand with the higher second card wins; otherwise, continue with the third card in each hand, then the fourth, then the fifth.

  So, 33332 and 2AAAA are both four of a kind hands, but 33332 is stronger because its first card is stronger. Similarly, 77888 and 77788 are both a full house, but 77888 is stronger because its third card is stronger (and both hands have the same first and second card).

  To play Camel Cards, you are given a list of hands and their corresponding bid (your puzzle input). For example:

  32T3K 765
  T55J5 684
  KK677 28
  KTJJT 220
  QQQJA 483
  This example shows five hands; each hand is followed by its bid amount. Each hand wins an amount equal to its bid multiplied by its rank, where the weakest hand gets rank 1, the second-weakest hand gets rank 2, and so on up to the strongest hand. Because there are five hands in this example, the strongest hand will have rank 5 and its bid will be multiplied by 5.

  So, the first step is to put the hands in order of strength:

  32T3K is the only one pair and the other hands are all a stronger type, so it gets rank 1.
  KK677 and KTJJT are both two pair. Their first cards both have the same label, but the second card of KK677 is stronger (K vs T), so KTJJT gets rank 2 and KK677 gets rank 3.
  T55J5 and QQQJA are both three of a kind. QQQJA has a stronger first card, so it gets rank 5 and T55J5 gets rank 4.
  Now, you can determine the total winnings of this set of hands by adding up the result of multiplying each hand's bid with its rank (765 * 1 + 220 * 2 + 28 * 3 + 684 * 4 + 483 * 5). So the total winnings in this example are 6440.

  Find the rank of every hand in your set. What are the total winnings?
*)
module Part1 = struct
  type row = {
    hand : int list;
    bid : int;
  }
  [@@deriving sexp]

  let card_rank = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | '9' -> 9
  | '8' -> 8
  | '7' -> 7
  | '6' -> 6
  | '5' -> 5
  | '4' -> 4
  | '3' -> 3
  | '2' -> 2
  | _ -> assert false

  let hand_rank hand =
    match List.sort_and_group ~compare:[%compare: int] hand with
    | [_] -> 6
    | [x; y] -> if List.length x = 4 || List.length y == 4 then 5 else 4
    | [x; y; z] ->
      if List.length x = 3 || List.length y = 3 || List.length z = 3 then 3
      else 2
    | [_; _; _; _] -> 1
    | [_; _; _; _; _] -> 0
    | _ -> assert false

  let compare_hand x y =
    [%compare: int * int list] (hand_rank x, x) (hand_rank y, y)

  let parse input : row list =
    String.split_lines input
    |> List.map ~f:(fun row ->
           String.split row ~on:' '
           |> function
           | [hand; bid] ->
             assert (String.length hand = 5);
             let hand = String.to_list hand |> List.map ~f:card_rank in
             let bid = Int.of_string bid in
             { hand; bid }
           | _ -> assert false
       )

  let%expect_test _ =
    Inputs.example_input
    |> parse
    |> List.map ~f:[%sexp_of: row]
    |> List.iter ~f:print_sexp;
    [%expect
      {|
      ((hand (3 2 10 3 13)) (bid 765))
      ((hand (10 5 5 11 5)) (bid 684))
      ((hand (13 13 6 7 7)) (bid 28))
      ((hand (13 10 11 11 10)) (bid 220))
      ((hand (12 12 12 11 14)) (bid 483)) |}]

  let solve input =
    input
    |> parse
    |> List.sort ~compare:(fun { hand = x } { hand = y } -> compare_hand x y)
    |> List.foldi ~init:0 ~f:(fun i acc { bid } -> acc + ((i + 1) * bid))
    |> Int.to_string

  let%expect_test _ =
    Inputs.example_input |> solve |> printf "%s\n";
    [%expect {| 6440 |}]

  let%expect_test _ =
    Inputs.puzzle_input |> solve |> printf "%s\n";
    [%expect {| 253910319 |}]
end

(*
   --- Part Two ---
   To make things a little more interesting, the Elf introduces one additional rule. Now, J cards are jokers - wildcards that can act like whatever card would make the hand the strongest type possible.

   To balance this, J cards are now the weakest individual cards, weaker even than 2. The other cards stay in the same order: A, K, Q, T, 9, 8, 7, 6, 5, 4, 3, 2, J.

   J cards can pretend to be whatever card is best for the purpose of determining hand type; for example, QJJQ2 is now considered four of a kind. However, for the purpose of breaking ties between two hands of the same type, J is always treated as J, not the card it's pretending to be: JKKK2 is weaker than QQQQ2 because J is weaker than Q.

   Now, the above example goes very differently:

   32T3K 765
   T55J5 684
   KK677 28
   KTJJT 220
   QQQJA 483
   32T3K is still the only one pair; it doesn't contain any jokers, so its strength doesn't increase.
   KK677 is now the only two pair, making it the second-weakest hand.
   T55J5, KTJJT, and QQQJA are now all four of a kind! T55J5 gets rank 3, QQQJA gets rank 4, and KTJJT gets rank 5.
   With the new joker rule, the total winnings in this example are 5905.

   Using the new joker rule, find the rank of every hand in your set. What are the new total winnings?
*)
module Part2 = struct
  type row = {
    hand : int list;
    bid : int;
  }
  [@@deriving sexp]

  let card_rank = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 1
  | 'T' -> 10
  | '9' -> 9
  | '8' -> 8
  | '7' -> 7
  | '6' -> 6
  | '5' -> 5
  | '4' -> 4
  | '3' -> 3
  | '2' -> 2
  | _ -> assert false

  let hand_rank hand =
    let jokers, hand = List.partition_tf hand ~f:(( = ) (card_rank 'J')) in
    let j = List.length jokers in
    match List.sort_and_group ~compare:[%compare: int] hand with
    | [] | [_] -> 6
    | [x; y] -> if List.length x + j = 4 || List.length y + j == 4 then 5 else 4
    | [x; y; z] ->
      if List.length x + j = 3 || List.length y + j = 3 || List.length z + j = 3
      then 3
      else 2
    | [_; _; _; _] -> 1
    | [_; _; _; _; _] -> 0
    | _ -> assert false

  let compare_hand x y =
    [%compare: int * int list] (hand_rank x, x) (hand_rank y, y)

  let parse input : row list =
    String.split_lines input
    |> List.map ~f:(fun row ->
           String.split row ~on:' '
           |> function
           | [hand; bid] ->
             assert (String.length hand = 5);
             let hand = String.to_list hand |> List.map ~f:card_rank in
             let bid = Int.of_string bid in
             { hand; bid }
           | _ -> assert false
       )

  let%expect_test _ =
    Inputs.example_input
    |> parse
    |> List.map ~f:[%sexp_of: row]
    |> List.iter ~f:print_sexp;
    [%expect
      {|
      ((hand (3 2 10 3 13)) (bid 765))
      ((hand (10 5 5 1 5)) (bid 684))
      ((hand (13 13 6 7 7)) (bid 28))
      ((hand (13 10 1 1 10)) (bid 220))
      ((hand (12 12 12 1 14)) (bid 483)) |}]

  let solve input =
    input
    |> parse
    |> List.sort ~compare:(fun { hand = x } { hand = y } -> compare_hand x y)
    |> List.foldi ~init:0 ~f:(fun i acc { bid } -> acc + ((i + 1) * bid))
    |> Int.to_string

  let%expect_test _ =
    Inputs.example_input |> solve |> printf "%s\n";
    [%expect {| 5905 |}]

  let%expect_test _ =
    Inputs.puzzle_input |> solve |> printf "%s\n";
    [%expect {| 254083736 |}]
end
