open! Base
open! Stdio

(*
--- Day 2: Cube Conundrum ---
You're launched high into the atmosphere! The apex of your trajectory just barely reaches the surface of a large island floating in the sky. You gently land in a fluffy pile of leaves. It's quite cold, but you don't see much snow. An Elf runs over to greet you.

The Elf explains that you've arrived at Snow Island and apologizes for the lack of snow. He'll be happy to explain the situation, but it's a bit of a walk, so you have some time. They don't get many visitors up here; would you like to play a game in the meantime?

As you walk, the Elf shows you a small bag and some cubes which are either red, green, or blue. Each time you play this game, he will hide a secret number of cubes of each color in the bag, and your goal is to figure out information about the number of cubes.

To get information, once a bag has been loaded with cubes, the Elf will reach into the bag, grab a handful of random cubes, show them to you, and then put them back in the bag. He'll do this a few times per game.

You play several games and record the information from each game (your puzzle input). Each game is listed with its ID number (like the 11 in Game 11: ...) followed by a semicolon-separated list of subsets of cubes that were revealed from the bag (like 3 red, 5 green, 4 blue).

For example, the record of a few games might look like this:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
In game 1, three sets of cubes are revealed from the bag (and then put back again). The first set is 3 blue cubes and 4 red cubes; the second set is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes.

The Elf would first like to know which games would have been possible if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

In the example above, games 1, 2, and 5 would have been possible if the bag had been loaded with that configuration. However, game 3 would have been impossible because at one point the Elf showed you 20 red cubes at once; similarly, game 4 would also have been impossible because the Elf showed you 15 blue cubes at once. If you add up the IDs of the games that would have been possible, you get 8.

Determine which games would have been possible if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the IDs of those games?
*)

type color =
  | Red
  | Green
  | Blue
[@@deriving sexp, compare, equal, hash]

type hand = color -> int

let sexp_of_hand (h : hand) : Sexp.t =
  [Red, h Red; Green, h Green; Blue, h Blue]
  |> List.filter ~f:(fun (_, n) -> n <> 0)
  |> [%sexp_of: (color * int) list]

type game = {
  id : int;
  hands : hand list;
}
[@@deriving sexp_of]

let empty_hand : hand = fun _ -> 0

let parse_color_exn = function
| "red" -> Red
| "green" -> Green
| "blue" -> Blue
| s -> failwith (Printf.sprintf "invalid color: \"%s\"" s)

let parse_game_header_exn (header : string) : int =
  header
  |> String.strip
  |> String.chop_prefix_exn ~prefix:"Game "
  |> String.strip
  |> Int.of_string

let merge_disjoint_hands_exn (h1 : hand) (h2 : hand) =
  let merge_disjoint n m =
    match n, m with
    | x, 0 | 0, x -> x
    | _ -> failwith "not disjoint"
  in
  let red = merge_disjoint (h1 Red) (h2 Red) in
  let green = merge_disjoint (h1 Green) (h2 Green) in
  let blue = merge_disjoint (h1 Blue) (h2 Blue) in
  function
  | Red -> red
  | Green -> green
  | Blue -> blue

let parse_hand_subset_exn (subset : string) : hand =
  subset
  |> String.strip
  |> String.split ~on:' '
  |> function
  | [num; color] ->
    let num = num |> String.strip |> Int.of_string in
    let color = color |> String.strip |> parse_color_exn in
    fun c -> if [%equal: color] c color then num else 0
  | _ -> failwith "invalid"

let parse_hand_exn (hand : string) : hand =
  hand
  |> String.strip
  |> String.split ~on:','
  |> List.map ~f:parse_hand_subset_exn
  |> List.fold ~init:empty_hand ~f:merge_disjoint_hands_exn

let parse_game_exn (game_record : string) : game =
  match String.split game_record ~on:':' with
  | [header; hands] ->
    let id = parse_game_header_exn header in
    let hands = String.split hands ~on:';' |> List.map ~f:parse_hand_exn in
    { id; hands }
  | _ -> Printf.sprintf "invalid game_record: \"%s\"" game_record |> failwith

let parse_games_record_exn (games_record : string) : game list =
  games_record |> String.split_lines |> List.map ~f:parse_game_exn

let example_record =
  {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}

let%expect_test _ =
  example_record
  |> parse_games_record_exn
  |> [%sexp_of: game list]
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect
    {|
    (((id 1)
      (hands (((Red 4) (Blue 3)) ((Red 1) (Green 2) (Blue 6)) ((Green 2)))))
     ((id 2)
      (hands
       (((Green 2) (Blue 1)) ((Red 1) (Green 3) (Blue 4)) ((Green 1) (Blue 1)))))
     ((id 3)
      (hands
       (((Red 20) (Green 8) (Blue 6)) ((Red 4) (Green 13) (Blue 5))
        ((Red 1) (Green 5)))))
     ((id 4)
      (hands
       (((Red 3) (Green 1) (Blue 6)) ((Red 6) (Green 3))
        ((Red 14) (Green 3) (Blue 15)))))
     ((id 5) (hands (((Red 6) (Green 3) (Blue 1)) ((Red 1) (Green 2) (Blue 2)))))) |}]

let is_legal_game (game : game) ~(max_hand : hand) : bool =
  List.for_all game.hands ~f:(fun hand ->
      hand Red <= max_hand Red
      && hand Green <= max_hand Green
      && hand Blue <= max_hand Blue
  )

let example_max_hand = function
| Red -> 12
| Green -> 13
| Blue -> 14

let%test _ =
  {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green|}
  |> parse_game_exn
  |> is_legal_game ~max_hand:example_max_hand

let%test _ =
  {|Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue|}
  |> parse_game_exn
  |> is_legal_game ~max_hand:example_max_hand

let%test _ =
  {|Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red|}
  |> parse_game_exn
  |> is_legal_game ~max_hand:example_max_hand
  |> not

let%test _ =
  {|Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red|}
  |> parse_game_exn
  |> is_legal_game ~max_hand:example_max_hand
  |> not

let%test _ =
  {|Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}
  |> parse_game_exn
  |> is_legal_game ~max_hand:example_max_hand

let solve ~(record : string) ~(max_hand : hand) : int =
  record
  |> parse_games_record_exn
  |> List.filter ~f:(is_legal_game ~max_hand)
  |> List.map ~f:(fun { id; _ } -> id)
  |> List.fold ~init:0 ~f:( + )

let%test_unit _ =
  [%test_eq: int] (solve ~record:example_record ~max_hand:example_max_hand) 8

let personal_puzzle_input =
  {|Game 1: 4 blue, 7 red, 5 green; 3 blue, 4 red, 16 green; 3 red, 11 green
Game 2: 20 blue, 8 red, 1 green; 1 blue, 2 green, 8 red; 9 red, 4 green, 18 blue; 2 green, 7 red, 2 blue; 10 blue, 2 red, 5 green
Game 3: 2 red, 5 green, 1 blue; 3 blue, 5 green; 8 blue, 13 green, 2 red; 9 green, 3 blue; 12 green, 13 blue; 3 green, 3 blue, 1 red
Game 4: 1 red, 6 green, 4 blue; 3 green, 1 blue, 1 red; 7 blue, 1 red, 2 green
Game 5: 2 green, 9 blue, 1 red; 3 green, 1 blue, 3 red; 1 red, 4 blue, 9 green
Game 6: 2 blue, 5 red, 7 green; 5 blue, 8 red, 3 green; 2 red, 9 blue, 2 green
Game 7: 7 green, 7 blue, 2 red; 2 red, 7 green, 16 blue; 17 blue, 3 green, 3 red; 2 blue, 5 green, 3 red
Game 8: 4 red, 3 green; 9 green, 2 red, 2 blue; 1 red, 3 blue, 6 green
Game 9: 5 red, 3 green, 13 blue; 11 red, 15 blue, 1 green; 7 red, 2 blue
Game 10: 15 red, 3 green; 7 green, 4 blue, 11 red; 13 red, 13 blue; 2 blue, 5 green, 8 red
Game 11: 7 red, 3 green; 7 blue, 16 red, 4 green; 6 green, 6 blue, 12 red; 11 red, 4 green, 4 blue; 10 red, 6 blue, 2 green; 3 green, 7 red, 6 blue
Game 12: 1 blue, 2 red; 2 green, 15 blue; 6 green, 5 blue; 6 blue, 4 green; 5 blue, 3 green; 3 red, 3 blue, 10 green
Game 13: 10 red, 4 green; 9 red, 2 blue, 3 green; 6 red, 7 green, 1 blue; 9 red, 7 green, 1 blue; 3 blue; 3 blue, 3 red, 8 green
Game 14: 12 blue, 3 red, 4 green; 3 green, 1 red; 6 green, 16 blue
Game 15: 2 green, 3 red, 2 blue; 14 blue, 1 red, 17 green; 13 blue, 11 green, 10 red; 5 green, 7 red, 5 blue; 2 green, 3 blue, 6 red; 9 green, 2 blue, 5 red
Game 16: 2 blue, 1 red; 1 red, 2 green, 3 blue; 4 green, 9 blue, 3 red; 1 green, 4 red, 8 blue; 7 blue, 11 red, 1 green
Game 17: 9 green, 8 blue, 6 red; 8 red, 18 green, 1 blue; 18 red, 19 green, 1 blue
Game 18: 1 green, 4 red, 5 blue; 10 green, 8 blue; 12 green, 10 blue
Game 19: 3 red, 11 green, 12 blue; 16 green, 1 red, 20 blue; 9 green, 2 red, 14 blue; 5 blue, 2 green, 2 red; 20 blue, 3 red, 10 green; 4 green, 3 blue
Game 20: 17 red, 3 blue, 9 green; 6 green, 1 red, 7 blue; 6 red, 2 blue; 1 blue, 4 green, 5 red; 6 green, 5 red; 10 blue, 11 green, 2 red
Game 21: 9 red, 4 blue, 6 green; 14 red, 9 green; 1 red, 1 blue, 12 green
Game 22: 5 green, 4 red; 1 green, 1 red, 2 blue; 5 red, 4 green, 4 blue; 2 green, 2 blue, 5 red; 8 green, 4 blue, 16 red; 15 red, 3 green
Game 23: 5 green, 14 red; 6 blue, 2 green, 14 red; 4 blue, 8 red, 4 green; 4 blue, 9 red, 8 green; 9 blue, 3 green
Game 24: 13 blue, 9 green, 13 red; 11 blue, 14 red, 10 green; 12 green, 5 blue, 14 red
Game 25: 11 green, 1 blue; 12 red, 8 green, 5 blue; 1 blue, 8 green, 6 red
Game 26: 4 blue, 1 green; 1 green, 5 red, 6 blue; 8 green, 5 blue, 6 red; 2 blue, 2 red, 8 green; 8 green, 2 red, 4 blue; 7 red, 2 blue, 7 green
Game 27: 8 red, 1 blue, 8 green; 5 red, 2 green; 2 blue, 9 green, 9 red; 2 blue
Game 28: 2 green, 1 blue; 2 green; 1 blue; 1 blue, 1 red; 1 blue; 1 green
Game 29: 12 red, 8 green, 13 blue; 13 green, 15 red; 12 red, 18 green, 10 blue; 7 green, 20 red, 5 blue; 20 red, 7 green, 10 blue; 9 green, 13 blue
Game 30: 5 red, 3 blue; 2 red; 2 green, 6 blue, 7 red; 5 red
Game 31: 14 red, 7 blue, 2 green; 1 green, 11 red, 9 blue; 3 red, 2 green, 5 blue; 1 green, 9 blue, 8 red; 8 blue, 8 red, 1 green
Game 32: 2 green, 6 blue, 2 red; 2 blue, 4 red; 1 green, 9 blue, 1 red; 3 red, 13 blue, 1 green
Game 33: 6 green, 8 blue, 7 red; 3 blue, 1 green, 8 red; 6 red, 11 blue; 10 blue, 3 red, 7 green; 1 blue, 3 red, 6 green
Game 34: 1 red, 1 blue, 8 green; 5 blue, 10 red, 11 green; 2 green, 10 red, 2 blue
Game 35: 2 blue, 15 green; 3 red, 3 blue, 6 green; 13 green, 17 red, 3 blue; 18 green, 1 blue, 18 red; 16 green, 3 blue; 11 green, 15 red
Game 36: 16 red, 4 green, 1 blue; 8 red, 2 blue, 5 green; 5 green, 2 blue, 9 red
Game 37: 3 green, 7 blue; 8 blue, 5 red, 6 green; 5 blue, 1 red, 13 green
Game 38: 6 green, 6 blue; 11 blue, 8 green, 1 red; 5 blue, 16 green
Game 39: 2 red, 4 blue, 5 green; 1 red, 2 green, 8 blue; 16 green, 15 blue, 2 red; 6 green, 16 blue, 1 red; 16 green, 18 blue, 1 red
Game 40: 3 green, 6 blue, 7 red; 1 blue, 17 red; 4 green, 6 red; 13 red
Game 41: 6 red, 5 green, 6 blue; 4 green, 2 blue; 6 red, 1 blue, 4 green; 4 blue, 13 green; 3 blue, 2 red; 2 blue, 5 red, 3 green
Game 42: 8 red, 5 blue; 15 blue, 13 red, 3 green; 6 red, 18 blue, 4 green
Game 43: 5 red, 1 green, 1 blue; 2 red, 2 green, 3 blue; 4 blue, 3 red, 1 green
Game 44: 6 blue, 12 green; 7 blue, 12 red, 11 green; 12 green, 2 blue, 13 red; 8 green, 8 blue, 12 red
Game 45: 18 blue, 15 red, 8 green; 17 red, 3 blue; 1 green, 2 red, 15 blue
Game 46: 3 blue, 2 green, 5 red; 11 blue, 2 green, 19 red; 3 green, 19 red, 13 blue
Game 47: 9 green, 2 red; 7 red, 10 green; 2 blue, 9 green, 1 red; 5 blue
Game 48: 8 blue, 8 green; 1 red, 17 green; 9 green, 6 red, 8 blue; 13 green, 3 red, 1 blue
Game 49: 17 blue, 2 red, 1 green; 12 blue, 1 green, 4 red; 1 green, 2 red, 13 blue
Game 50: 4 red, 2 blue, 9 green; 8 green, 2 blue, 6 red; 9 green, 2 blue, 14 red
Game 51: 6 red, 3 green, 8 blue; 5 green, 16 blue, 1 red; 2 green, 13 red, 14 blue; 14 red, 12 green, 19 blue; 19 blue, 13 green, 9 red; 6 red, 15 blue, 7 green
Game 52: 18 blue, 2 red, 5 green; 2 green, 5 red; 6 red, 10 green, 3 blue; 3 green, 6 blue, 6 red
Game 53: 11 red, 4 green; 2 blue, 3 red; 3 blue, 13 red, 11 green; 11 blue, 8 red, 5 green
Game 54: 4 green, 1 red, 7 blue; 4 green, 8 red, 8 blue; 4 red, 5 green; 8 blue, 4 green, 2 red; 4 green, 3 blue; 3 blue, 3 green, 3 red
Game 55: 9 red, 1 green, 1 blue; 1 green, 8 red; 4 red; 7 blue, 7 green; 6 blue, 5 green, 6 red; 5 blue, 8 red, 4 green
Game 56: 1 blue; 3 red, 2 blue; 1 red, 2 green
Game 57: 7 green, 2 red, 5 blue; 6 green, 1 red; 1 green, 6 red; 1 red, 20 green; 1 green, 4 red, 2 blue; 15 green, 7 red
Game 58: 3 green, 8 red, 5 blue; 2 red, 3 green; 2 blue, 2 green, 12 red; 1 blue, 3 green, 16 red; 4 blue, 9 red, 3 green
Game 59: 2 red, 5 blue, 1 green; 2 red, 3 green; 12 red, 5 blue; 7 green, 3 blue, 4 red; 1 green, 5 blue, 14 red; 8 red, 11 green, 2 blue
Game 60: 12 blue, 3 red, 2 green; 2 green, 6 blue, 1 red; 1 blue, 2 red, 3 green; 7 green, 1 blue, 2 red
Game 61: 6 blue, 6 red, 7 green; 2 green, 5 red, 5 blue; 1 blue, 3 green, 15 red; 6 blue, 8 green, 14 red
Game 62: 1 blue, 6 red, 2 green; 5 green, 5 red, 11 blue; 5 red, 6 green, 8 blue; 2 green, 17 blue; 2 red, 7 green, 5 blue; 3 blue, 5 green, 8 red
Game 63: 6 red, 1 green, 9 blue; 7 red, 1 green, 11 blue; 3 green, 4 red; 4 green, 10 blue, 7 red; 13 blue, 11 green, 5 red; 14 green
Game 64: 13 green, 11 red, 1 blue; 1 red, 2 green; 3 blue, 9 green, 19 red
Game 65: 2 blue, 11 red, 3 green; 5 green, 6 red; 2 blue, 9 green, 9 red; 1 green, 5 blue, 3 red; 4 red, 4 blue, 6 green; 2 blue, 7 green, 1 red
Game 66: 4 red, 7 blue, 3 green; 1 green, 6 blue, 7 red; 1 green, 1 red, 1 blue
Game 67: 1 green, 8 red; 4 green, 1 blue, 3 red; 8 red, 3 green
Game 68: 3 blue, 4 red; 1 blue, 1 green; 2 blue, 6 red, 3 green; 1 blue, 1 green, 3 red; 7 red, 1 blue, 4 green; 1 green, 2 red, 3 blue
Game 69: 6 green, 2 blue, 3 red; 3 blue, 3 red; 1 green; 1 blue, 2 red, 8 green; 1 green, 1 red
Game 70: 7 blue, 15 green, 3 red; 8 green, 6 blue, 5 red; 7 blue, 1 red, 3 green
Game 71: 4 green, 3 blue, 7 red; 6 red, 6 green, 10 blue; 3 red, 9 green; 7 blue, 1 red, 13 green; 3 blue, 5 red, 11 green; 8 blue, 8 red, 5 green
Game 72: 10 green, 4 blue; 4 blue, 8 green, 2 red; 2 red, 6 green, 6 blue; 1 red, 5 blue; 13 green, 5 blue; 8 green, 3 blue, 2 red
Game 73: 9 blue, 1 red, 13 green; 2 red, 16 green, 6 blue; 1 red, 8 blue, 17 green; 7 green, 1 blue; 8 blue, 1 green, 1 red
Game 74: 2 green, 2 red; 1 red, 5 blue; 7 blue, 3 green; 7 blue, 3 green, 7 red
Game 75: 3 green, 5 blue; 2 green, 1 red, 9 blue; 17 green, 13 blue, 3 red; 3 blue, 2 red, 8 green; 7 green, 2 red, 8 blue; 1 green, 14 blue
Game 76: 19 red; 2 blue, 20 red; 3 blue, 3 red; 20 red, 3 blue; 6 red, 4 blue, 1 green
Game 77: 2 red, 5 green; 2 red, 2 green; 4 green; 4 green, 3 red, 3 blue; 2 red
Game 78: 4 green, 16 red; 5 green, 2 red, 2 blue; 4 green, 2 blue, 11 red; 1 blue, 1 green, 6 red; 2 blue, 7 red
Game 79: 8 blue, 2 green; 3 red, 3 green; 3 red, 9 blue, 4 green; 1 red, 2 blue, 4 green; 8 green, 6 red, 9 blue; 2 red, 10 blue, 9 green
Game 80: 9 red, 17 blue, 2 green; 5 red, 1 green, 6 blue; 2 red, 20 blue; 6 red, 12 blue
Game 81: 5 red, 4 blue, 1 green; 15 green, 8 blue, 2 red; 5 blue, 2 red, 9 green; 11 green, 1 blue, 3 red; 15 green, 1 red, 3 blue
Game 82: 2 blue, 12 green; 12 blue, 12 green, 14 red; 4 blue, 16 green, 7 red
Game 83: 6 blue, 7 red, 11 green; 2 red, 6 green, 4 blue; 6 blue, 1 red; 7 blue, 12 red, 13 green; 10 green, 6 blue, 10 red; 6 red, 4 green
Game 84: 2 green, 5 red, 1 blue; 4 green, 3 blue, 2 red; 2 green, 1 red, 1 blue; 5 red, 4 blue, 4 green
Game 85: 1 blue; 1 green, 2 red; 3 red, 11 green; 6 green, 14 red, 1 blue
Game 86: 3 green, 1 blue, 3 red; 3 red, 6 blue, 2 green; 4 blue, 1 red; 5 blue, 4 green, 3 red; 2 blue, 3 red, 4 green; 7 blue, 2 green, 3 red
Game 87: 1 green, 5 red, 5 blue; 6 red, 4 green, 1 blue; 2 green, 4 red, 1 blue; 7 red, 4 green, 5 blue; 3 green, 4 red, 1 blue
Game 88: 3 blue, 18 red, 14 green; 11 red, 14 green; 2 blue, 10 red, 4 green
Game 89: 5 red, 4 green; 3 red, 2 blue, 1 green; 2 blue, 4 green, 3 red; 2 green, 2 blue, 2 red
Game 90: 14 blue, 10 red, 2 green; 11 blue, 3 red, 1 green; 5 blue, 2 green, 14 red
Game 91: 9 blue, 4 red, 4 green; 4 red, 1 blue; 3 blue, 20 red
Game 92: 3 red, 2 green, 7 blue; 2 green, 10 red, 8 blue; 9 red, 5 blue, 5 green; 1 blue, 2 green, 3 red; 10 red, 13 blue, 9 green; 11 blue, 7 red
Game 93: 9 red, 2 blue, 1 green; 6 red, 2 blue, 11 green; 1 green, 1 blue, 10 red; 9 red, 8 green
Game 94: 18 green, 3 red; 2 blue, 4 green, 12 red; 5 red, 1 blue, 13 green; 2 blue, 15 green, 7 red
Game 95: 12 green; 1 red, 3 green, 1 blue; 13 green, 2 red, 1 blue; 9 green; 2 green, 1 blue; 1 blue, 4 green, 1 red
Game 96: 5 red, 4 green, 2 blue; 10 red, 3 blue, 5 green; 14 blue, 11 green, 4 red; 14 green, 7 blue, 13 red; 17 red, 9 green, 6 blue; 8 red, 4 blue, 13 green
Game 97: 3 green, 7 blue; 7 red, 4 blue; 5 blue, 6 red, 2 green
Game 98: 9 green; 8 green, 4 blue; 6 blue, 2 red, 1 green; 4 green, 1 blue; 5 blue, 2 green, 2 red
Game 99: 3 red, 1 green, 5 blue; 1 red; 3 blue, 4 red; 3 blue, 1 green, 5 red
Game 100: 3 red, 3 blue, 10 green; 3 green, 1 blue, 6 red; 5 red, 4 green, 7 blue|}

let%expect_test "puzzle_solution" =
  solve ~record:personal_puzzle_input ~max_hand:example_max_hand
  |> printf "%d\n";
  [%expect {| 2512 |}]

(*
  --- Part Two ---
The Elf says they've stopped producing snow because they aren't getting any water! He isn't sure why the water stopped; however, he can show you how to get to the water source to check it out for yourself. It's just up ahead!

As you continue your walk, the Elf poses a second question: in each game you played, what is the fewest number of cubes of each color that could have been in the bag to make the game possible?

Again consider the example games from earlier:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
In game 1, the game could have been played with as few as 4 red, 2 green, and 6 blue cubes. If any color had even one fewer cube, the game would have been impossible.
Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
Game 4 required at least 14 red, 3 green, and 15 blue cubes.
Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.
The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied together. The power of the minimum set of cubes in game 1 is 48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding up these five powers produces the sum 2286.

For each game, find the minimum set of cubes that must have been present. What is the sum of the power of these sets?   
*)

let get_min_hand { hands; _ } : hand =
 fun c ->
  List.map hands ~f:(fun h -> h c)
  |> List.max_elt ~compare:Int.compare
  |> Option.value ~default:0

let%expect_test _ =
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  |> parse_game_exn
  |> get_min_hand
  |> [%sexp_of: hand]
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {| ((Red 4) (Green 2) (Blue 6)) |}]

let%expect_test _ =
  "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
  |> parse_game_exn
  |> get_min_hand
  |> [%sexp_of: hand]
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {| ((Red 1) (Green 3) (Blue 4)) |}]

let%expect_test _ =
  "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
  |> parse_game_exn
  |> get_min_hand
  |> [%sexp_of: hand]
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {| ((Red 20) (Green 13) (Blue 6)) |}]

let%expect_test _ =
  "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
  |> parse_game_exn
  |> get_min_hand
  |> [%sexp_of: hand]
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {| ((Red 14) (Green 3) (Blue 15)) |}]

let%expect_test _ =
  "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  |> parse_game_exn
  |> get_min_hand
  |> [%sexp_of: hand]
  |> Sexp.to_string_hum
  |> print_endline;
  [%expect {| ((Red 6) (Green 3) (Blue 2)) |}]

let get_power_of_game game =
  let hand = get_min_hand game in
  hand Red * hand Green * hand Blue

let solve_power (record : string) =
  record
  |> String.split_lines
  |> List.map ~f:parse_game_exn
  |> List.map ~f:get_power_of_game
  |> List.fold ~init:0 ~f:( + )

let%expect_test _ =
  example_record |> solve_power |> printf "%d\n";
  [%expect {| 2286 |}]

let puzzle_input =
  {|Game 1: 4 blue, 7 red, 5 green; 3 blue, 4 red, 16 green; 3 red, 11 green
Game 2: 20 blue, 8 red, 1 green; 1 blue, 2 green, 8 red; 9 red, 4 green, 18 blue; 2 green, 7 red, 2 blue; 10 blue, 2 red, 5 green
Game 3: 2 red, 5 green, 1 blue; 3 blue, 5 green; 8 blue, 13 green, 2 red; 9 green, 3 blue; 12 green, 13 blue; 3 green, 3 blue, 1 red
Game 4: 1 red, 6 green, 4 blue; 3 green, 1 blue, 1 red; 7 blue, 1 red, 2 green
Game 5: 2 green, 9 blue, 1 red; 3 green, 1 blue, 3 red; 1 red, 4 blue, 9 green
Game 6: 2 blue, 5 red, 7 green; 5 blue, 8 red, 3 green; 2 red, 9 blue, 2 green
Game 7: 7 green, 7 blue, 2 red; 2 red, 7 green, 16 blue; 17 blue, 3 green, 3 red; 2 blue, 5 green, 3 red
Game 8: 4 red, 3 green; 9 green, 2 red, 2 blue; 1 red, 3 blue, 6 green
Game 9: 5 red, 3 green, 13 blue; 11 red, 15 blue, 1 green; 7 red, 2 blue
Game 10: 15 red, 3 green; 7 green, 4 blue, 11 red; 13 red, 13 blue; 2 blue, 5 green, 8 red
Game 11: 7 red, 3 green; 7 blue, 16 red, 4 green; 6 green, 6 blue, 12 red; 11 red, 4 green, 4 blue; 10 red, 6 blue, 2 green; 3 green, 7 red, 6 blue
Game 12: 1 blue, 2 red; 2 green, 15 blue; 6 green, 5 blue; 6 blue, 4 green; 5 blue, 3 green; 3 red, 3 blue, 10 green
Game 13: 10 red, 4 green; 9 red, 2 blue, 3 green; 6 red, 7 green, 1 blue; 9 red, 7 green, 1 blue; 3 blue; 3 blue, 3 red, 8 green
Game 14: 12 blue, 3 red, 4 green; 3 green, 1 red; 6 green, 16 blue
Game 15: 2 green, 3 red, 2 blue; 14 blue, 1 red, 17 green; 13 blue, 11 green, 10 red; 5 green, 7 red, 5 blue; 2 green, 3 blue, 6 red; 9 green, 2 blue, 5 red
Game 16: 2 blue, 1 red; 1 red, 2 green, 3 blue; 4 green, 9 blue, 3 red; 1 green, 4 red, 8 blue; 7 blue, 11 red, 1 green
Game 17: 9 green, 8 blue, 6 red; 8 red, 18 green, 1 blue; 18 red, 19 green, 1 blue
Game 18: 1 green, 4 red, 5 blue; 10 green, 8 blue; 12 green, 10 blue
Game 19: 3 red, 11 green, 12 blue; 16 green, 1 red, 20 blue; 9 green, 2 red, 14 blue; 5 blue, 2 green, 2 red; 20 blue, 3 red, 10 green; 4 green, 3 blue
Game 20: 17 red, 3 blue, 9 green; 6 green, 1 red, 7 blue; 6 red, 2 blue; 1 blue, 4 green, 5 red; 6 green, 5 red; 10 blue, 11 green, 2 red
Game 21: 9 red, 4 blue, 6 green; 14 red, 9 green; 1 red, 1 blue, 12 green
Game 22: 5 green, 4 red; 1 green, 1 red, 2 blue; 5 red, 4 green, 4 blue; 2 green, 2 blue, 5 red; 8 green, 4 blue, 16 red; 15 red, 3 green
Game 23: 5 green, 14 red; 6 blue, 2 green, 14 red; 4 blue, 8 red, 4 green; 4 blue, 9 red, 8 green; 9 blue, 3 green
Game 24: 13 blue, 9 green, 13 red; 11 blue, 14 red, 10 green; 12 green, 5 blue, 14 red
Game 25: 11 green, 1 blue; 12 red, 8 green, 5 blue; 1 blue, 8 green, 6 red
Game 26: 4 blue, 1 green; 1 green, 5 red, 6 blue; 8 green, 5 blue, 6 red; 2 blue, 2 red, 8 green; 8 green, 2 red, 4 blue; 7 red, 2 blue, 7 green
Game 27: 8 red, 1 blue, 8 green; 5 red, 2 green; 2 blue, 9 green, 9 red; 2 blue
Game 28: 2 green, 1 blue; 2 green; 1 blue; 1 blue, 1 red; 1 blue; 1 green
Game 29: 12 red, 8 green, 13 blue; 13 green, 15 red; 12 red, 18 green, 10 blue; 7 green, 20 red, 5 blue; 20 red, 7 green, 10 blue; 9 green, 13 blue
Game 30: 5 red, 3 blue; 2 red; 2 green, 6 blue, 7 red; 5 red
Game 31: 14 red, 7 blue, 2 green; 1 green, 11 red, 9 blue; 3 red, 2 green, 5 blue; 1 green, 9 blue, 8 red; 8 blue, 8 red, 1 green
Game 32: 2 green, 6 blue, 2 red; 2 blue, 4 red; 1 green, 9 blue, 1 red; 3 red, 13 blue, 1 green
Game 33: 6 green, 8 blue, 7 red; 3 blue, 1 green, 8 red; 6 red, 11 blue; 10 blue, 3 red, 7 green; 1 blue, 3 red, 6 green
Game 34: 1 red, 1 blue, 8 green; 5 blue, 10 red, 11 green; 2 green, 10 red, 2 blue
Game 35: 2 blue, 15 green; 3 red, 3 blue, 6 green; 13 green, 17 red, 3 blue; 18 green, 1 blue, 18 red; 16 green, 3 blue; 11 green, 15 red
Game 36: 16 red, 4 green, 1 blue; 8 red, 2 blue, 5 green; 5 green, 2 blue, 9 red
Game 37: 3 green, 7 blue; 8 blue, 5 red, 6 green; 5 blue, 1 red, 13 green
Game 38: 6 green, 6 blue; 11 blue, 8 green, 1 red; 5 blue, 16 green
Game 39: 2 red, 4 blue, 5 green; 1 red, 2 green, 8 blue; 16 green, 15 blue, 2 red; 6 green, 16 blue, 1 red; 16 green, 18 blue, 1 red
Game 40: 3 green, 6 blue, 7 red; 1 blue, 17 red; 4 green, 6 red; 13 red
Game 41: 6 red, 5 green, 6 blue; 4 green, 2 blue; 6 red, 1 blue, 4 green; 4 blue, 13 green; 3 blue, 2 red; 2 blue, 5 red, 3 green
Game 42: 8 red, 5 blue; 15 blue, 13 red, 3 green; 6 red, 18 blue, 4 green
Game 43: 5 red, 1 green, 1 blue; 2 red, 2 green, 3 blue; 4 blue, 3 red, 1 green
Game 44: 6 blue, 12 green; 7 blue, 12 red, 11 green; 12 green, 2 blue, 13 red; 8 green, 8 blue, 12 red
Game 45: 18 blue, 15 red, 8 green; 17 red, 3 blue; 1 green, 2 red, 15 blue
Game 46: 3 blue, 2 green, 5 red; 11 blue, 2 green, 19 red; 3 green, 19 red, 13 blue
Game 47: 9 green, 2 red; 7 red, 10 green; 2 blue, 9 green, 1 red; 5 blue
Game 48: 8 blue, 8 green; 1 red, 17 green; 9 green, 6 red, 8 blue; 13 green, 3 red, 1 blue
Game 49: 17 blue, 2 red, 1 green; 12 blue, 1 green, 4 red; 1 green, 2 red, 13 blue
Game 50: 4 red, 2 blue, 9 green; 8 green, 2 blue, 6 red; 9 green, 2 blue, 14 red
Game 51: 6 red, 3 green, 8 blue; 5 green, 16 blue, 1 red; 2 green, 13 red, 14 blue; 14 red, 12 green, 19 blue; 19 blue, 13 green, 9 red; 6 red, 15 blue, 7 green
Game 52: 18 blue, 2 red, 5 green; 2 green, 5 red; 6 red, 10 green, 3 blue; 3 green, 6 blue, 6 red
Game 53: 11 red, 4 green; 2 blue, 3 red; 3 blue, 13 red, 11 green; 11 blue, 8 red, 5 green
Game 54: 4 green, 1 red, 7 blue; 4 green, 8 red, 8 blue; 4 red, 5 green; 8 blue, 4 green, 2 red; 4 green, 3 blue; 3 blue, 3 green, 3 red
Game 55: 9 red, 1 green, 1 blue; 1 green, 8 red; 4 red; 7 blue, 7 green; 6 blue, 5 green, 6 red; 5 blue, 8 red, 4 green
Game 56: 1 blue; 3 red, 2 blue; 1 red, 2 green
Game 57: 7 green, 2 red, 5 blue; 6 green, 1 red; 1 green, 6 red; 1 red, 20 green; 1 green, 4 red, 2 blue; 15 green, 7 red
Game 58: 3 green, 8 red, 5 blue; 2 red, 3 green; 2 blue, 2 green, 12 red; 1 blue, 3 green, 16 red; 4 blue, 9 red, 3 green
Game 59: 2 red, 5 blue, 1 green; 2 red, 3 green; 12 red, 5 blue; 7 green, 3 blue, 4 red; 1 green, 5 blue, 14 red; 8 red, 11 green, 2 blue
Game 60: 12 blue, 3 red, 2 green; 2 green, 6 blue, 1 red; 1 blue, 2 red, 3 green; 7 green, 1 blue, 2 red
Game 61: 6 blue, 6 red, 7 green; 2 green, 5 red, 5 blue; 1 blue, 3 green, 15 red; 6 blue, 8 green, 14 red
Game 62: 1 blue, 6 red, 2 green; 5 green, 5 red, 11 blue; 5 red, 6 green, 8 blue; 2 green, 17 blue; 2 red, 7 green, 5 blue; 3 blue, 5 green, 8 red
Game 63: 6 red, 1 green, 9 blue; 7 red, 1 green, 11 blue; 3 green, 4 red; 4 green, 10 blue, 7 red; 13 blue, 11 green, 5 red; 14 green
Game 64: 13 green, 11 red, 1 blue; 1 red, 2 green; 3 blue, 9 green, 19 red
Game 65: 2 blue, 11 red, 3 green; 5 green, 6 red; 2 blue, 9 green, 9 red; 1 green, 5 blue, 3 red; 4 red, 4 blue, 6 green; 2 blue, 7 green, 1 red
Game 66: 4 red, 7 blue, 3 green; 1 green, 6 blue, 7 red; 1 green, 1 red, 1 blue
Game 67: 1 green, 8 red; 4 green, 1 blue, 3 red; 8 red, 3 green
Game 68: 3 blue, 4 red; 1 blue, 1 green; 2 blue, 6 red, 3 green; 1 blue, 1 green, 3 red; 7 red, 1 blue, 4 green; 1 green, 2 red, 3 blue
Game 69: 6 green, 2 blue, 3 red; 3 blue, 3 red; 1 green; 1 blue, 2 red, 8 green; 1 green, 1 red
Game 70: 7 blue, 15 green, 3 red; 8 green, 6 blue, 5 red; 7 blue, 1 red, 3 green
Game 71: 4 green, 3 blue, 7 red; 6 red, 6 green, 10 blue; 3 red, 9 green; 7 blue, 1 red, 13 green; 3 blue, 5 red, 11 green; 8 blue, 8 red, 5 green
Game 72: 10 green, 4 blue; 4 blue, 8 green, 2 red; 2 red, 6 green, 6 blue; 1 red, 5 blue; 13 green, 5 blue; 8 green, 3 blue, 2 red
Game 73: 9 blue, 1 red, 13 green; 2 red, 16 green, 6 blue; 1 red, 8 blue, 17 green; 7 green, 1 blue; 8 blue, 1 green, 1 red
Game 74: 2 green, 2 red; 1 red, 5 blue; 7 blue, 3 green; 7 blue, 3 green, 7 red
Game 75: 3 green, 5 blue; 2 green, 1 red, 9 blue; 17 green, 13 blue, 3 red; 3 blue, 2 red, 8 green; 7 green, 2 red, 8 blue; 1 green, 14 blue
Game 76: 19 red; 2 blue, 20 red; 3 blue, 3 red; 20 red, 3 blue; 6 red, 4 blue, 1 green
Game 77: 2 red, 5 green; 2 red, 2 green; 4 green; 4 green, 3 red, 3 blue; 2 red
Game 78: 4 green, 16 red; 5 green, 2 red, 2 blue; 4 green, 2 blue, 11 red; 1 blue, 1 green, 6 red; 2 blue, 7 red
Game 79: 8 blue, 2 green; 3 red, 3 green; 3 red, 9 blue, 4 green; 1 red, 2 blue, 4 green; 8 green, 6 red, 9 blue; 2 red, 10 blue, 9 green
Game 80: 9 red, 17 blue, 2 green; 5 red, 1 green, 6 blue; 2 red, 20 blue; 6 red, 12 blue
Game 81: 5 red, 4 blue, 1 green; 15 green, 8 blue, 2 red; 5 blue, 2 red, 9 green; 11 green, 1 blue, 3 red; 15 green, 1 red, 3 blue
Game 82: 2 blue, 12 green; 12 blue, 12 green, 14 red; 4 blue, 16 green, 7 red
Game 83: 6 blue, 7 red, 11 green; 2 red, 6 green, 4 blue; 6 blue, 1 red; 7 blue, 12 red, 13 green; 10 green, 6 blue, 10 red; 6 red, 4 green
Game 84: 2 green, 5 red, 1 blue; 4 green, 3 blue, 2 red; 2 green, 1 red, 1 blue; 5 red, 4 blue, 4 green
Game 85: 1 blue; 1 green, 2 red; 3 red, 11 green; 6 green, 14 red, 1 blue
Game 86: 3 green, 1 blue, 3 red; 3 red, 6 blue, 2 green; 4 blue, 1 red; 5 blue, 4 green, 3 red; 2 blue, 3 red, 4 green; 7 blue, 2 green, 3 red
Game 87: 1 green, 5 red, 5 blue; 6 red, 4 green, 1 blue; 2 green, 4 red, 1 blue; 7 red, 4 green, 5 blue; 3 green, 4 red, 1 blue
Game 88: 3 blue, 18 red, 14 green; 11 red, 14 green; 2 blue, 10 red, 4 green
Game 89: 5 red, 4 green; 3 red, 2 blue, 1 green; 2 blue, 4 green, 3 red; 2 green, 2 blue, 2 red
Game 90: 14 blue, 10 red, 2 green; 11 blue, 3 red, 1 green; 5 blue, 2 green, 14 red
Game 91: 9 blue, 4 red, 4 green; 4 red, 1 blue; 3 blue, 20 red
Game 92: 3 red, 2 green, 7 blue; 2 green, 10 red, 8 blue; 9 red, 5 blue, 5 green; 1 blue, 2 green, 3 red; 10 red, 13 blue, 9 green; 11 blue, 7 red
Game 93: 9 red, 2 blue, 1 green; 6 red, 2 blue, 11 green; 1 green, 1 blue, 10 red; 9 red, 8 green
Game 94: 18 green, 3 red; 2 blue, 4 green, 12 red; 5 red, 1 blue, 13 green; 2 blue, 15 green, 7 red
Game 95: 12 green; 1 red, 3 green, 1 blue; 13 green, 2 red, 1 blue; 9 green; 2 green, 1 blue; 1 blue, 4 green, 1 red
Game 96: 5 red, 4 green, 2 blue; 10 red, 3 blue, 5 green; 14 blue, 11 green, 4 red; 14 green, 7 blue, 13 red; 17 red, 9 green, 6 blue; 8 red, 4 blue, 13 green
Game 97: 3 green, 7 blue; 7 red, 4 blue; 5 blue, 6 red, 2 green
Game 98: 9 green; 8 green, 4 blue; 6 blue, 2 red, 1 green; 4 green, 1 blue; 5 blue, 2 green, 2 red
Game 99: 3 red, 1 green, 5 blue; 1 red; 3 blue, 4 red; 3 blue, 1 green, 5 red
Game 100: 3 red, 3 blue, 10 green; 3 green, 1 blue, 6 red; 5 red, 4 green, 7 blue|}

let%expect_test _ =
  puzzle_input |> solve_power |> printf "%d\n";
  [%expect {| 67335 |}]
