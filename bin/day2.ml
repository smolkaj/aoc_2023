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

let example_record =
  {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}

type color = Red | Green | Blue [@@deriving sexp, compare, equal, hash]
type hand = color -> int

let sexp_of_hand (h : hand) : Sexp.t =
  [ (Red, h Red); (Green, h Green); (Blue, h Blue) ]
  |> List.filter ~f:(fun (_, n) -> n <> 0)
  |> [%sexp_of: (color * int) list]

type game = Game of { id : int; hands : hand list } [@@deriving sexp_of]

let empty_hand : hand = fun _ -> 0

let parse_color_exn = function
  | "red" -> Red
  | "green" -> Green
  | "blue" -> Blue
  | s -> failwith (Printf.sprintf "invalid color: \"%s\"" s)

let parse_game_header_exn (header : string) : int =
  header |> String.strip
  |> String.chop_prefix_exn ~prefix:"Game "
  |> String.strip |> Int.of_string

let merge_disjoint_hands_exn (h1 : hand) (h2 : hand) =
  let merge_disjoint n m =
    match (n, m) with x, 0 | 0, x -> x | _ -> failwith "not disjoint"
  in
  let red = merge_disjoint (h1 Red) (h2 Red) in
  let green = merge_disjoint (h1 Green) (h2 Green) in
  let blue = merge_disjoint (h1 Blue) (h2 Blue) in
  function Red -> red | Green -> green | Blue -> blue

let parse_hand_subset_exn (subset : string) : hand =
  subset |> String.strip |> String.split ~on:' ' |> function
  | [ num; color ] ->
      let num = num |> String.strip |> Int.of_string in
      let color = color |> String.strip |> parse_color_exn in
      fun c -> if [%equal: color] c color then num else 0
  | _ -> failwith "invalid"

let parse_hand_exn (hand : string) : hand =
  hand |> String.strip |> String.split ~on:','
  |> List.map ~f:parse_hand_subset_exn
  |> List.fold ~init:empty_hand ~f:merge_disjoint_hands_exn

let parse_game_exn (game_record : string) : game =
  match String.split game_record ~on:':' with
  | [ header; hands ] ->
      let id = parse_game_header_exn header in
      let hands = String.split hands ~on:';' |> List.map ~f:parse_hand_exn in
      Game { id; hands }
  | _ -> Printf.sprintf "invalid game_record: \"%s\"" game_record |> failwith

let parse_games_record (games_record : string) : game list =
  games_record |> String.split_lines |> List.map ~f:parse_game_exn

let test_parse_games_record () =
  example_record |> parse_games_record |> [%sexp_of: game list]
  |> Sexp.to_string_hum |> printf "%s\n"

let () =
  printf "\n------------------------------------------\n%s\n" example_record;
  test_parse_games_record ()
