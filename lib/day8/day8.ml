open! Base
open! Stdio

let print_sexp s = Sexp.to_string_hum s |> printf "%s\n"

let tap x ~f =
  f x;
  x

(*
--- Day 8: Haunted Wasteland ---
You're still riding a camel across Desert Island when you spot a sandstorm quickly approaching. When you turn to warn the Elf, she disappears before your eyes! To be fair, she had just finished warning you about ghosts a few minutes ago.

One of the camel's pouches is labeled "maps" - sure enough, it's full of documents (your puzzle input) about how to navigate the desert. At least, you're pretty sure that's what they are; one of the documents contains a list of left/right instructions, and the rest of the documents seem to describe some kind of network of labeled nodes.

It seems like you're meant to use the left/right instructions to navigate the network. Perhaps if you have the camel follow the same instructions, you can escape the haunted wasteland!

After examining the maps for a bit, two nodes stick out: AAA and ZZZ. You feel like AAA is where you are now, and you have to follow the left/right instructions until you reach ZZZ.

This format defines each node of the network individually. For example:

RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
Starting with AAA, you need to look up the next element based on the next left/right instruction in your input. In this example, start with AAA and go right (R) by choosing the right element of AAA, CCC. Then, L means to choose the left element of CCC, ZZZ. By following the left/right instructions, you reach ZZZ in 2 steps.

Of course, you might not find ZZZ right away. If you run out of left/right instructions, repeat the whole sequence of instructions as necessary: RL really means RLRLRLRLRLRLRLRL... and so on. For example, here is a situation that takes 6 steps to reach ZZZ:

LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
Starting at AAA, follow the left/right instructions. How many steps are required to reach ZZZ?
*)
module Part1 = struct
  type row = {
    node : string;
    left : string;
    right : string;
  }
  [@@deriving sexp]

  type game = {
    instructions : char list;
    start : string;
    graph : row Map.M(String).t;
  }
  [@@deriving sexp]

  let parse_row row : row =
    String.split row ~on:'='
    |> function
    | [node; lr] -> (
      let node = String.strip node in
      String.split lr ~on:','
      |> List.map
           ~f:
             (String.strip ~drop:(function
               | ' ' | '(' | ')' -> true
               | _ -> false
               )
               )
      |> function
      | [left; right] -> { node; left; right }
      | _ -> assert false
    )
    | _ -> assert false

  let parse input : game =
    String.split_lines input
    |> function
    | instructions :: _ :: rows ->
      let instructions = String.to_list instructions in
      let rows = List.map rows ~f:parse_row in
      let graph =
        List.fold rows
          ~init:(Map.empty (module String))
          ~f:(fun m row -> Map.add_exn m ~key:row.node ~data:row)
      in
      { start = "AAA"; instructions; graph }
    | _ -> assert false

  let%expect_test _ =
    Inputs.example_input |> parse |> [%sexp_of: game] |> print_sexp;
    [%expect
      {|
      ((instructions (R L)) (start AAA)
       (graph
        ((AAA ((node AAA) (left BBB) (right CCC)))
         (BBB ((node BBB) (left DDD) (right EEE)))
         (CCC ((node CCC) (left ZZZ) (right GGG)))
         (DDD ((node DDD) (left DDD) (right DDD)))
         (EEE ((node EEE) (left EEE) (right EEE)))
         (GGG ((node GGG) (left GGG) (right GGG)))
         (ZZZ ((node ZZZ) (left ZZZ) (right ZZZ)))))) |}]

  let solve input =
    let game = parse input in
    let rec loop state i instructions =
      let { left; right } = Map.find_exn game.graph state in
      match state, instructions with
      | "ZZZ", _ -> i
      | _, [] -> loop state i game.instructions
      | _, 'L' :: instructions -> loop left (i + 1) instructions
      | _, 'R' :: instructions -> loop right (i + 1) instructions
      | _ -> assert false
    in
    loop game.start 0 game.instructions |> Int.to_string

  let%expect_test _ =
    Inputs.example_input |> solve |> printf "%s\n";
    [%expect {| 2 |}]

  let%expect_test _ =
    Inputs.example_input2 |> solve |> printf "%s\n";
    [%expect {| 6 |}]

  let%expect_test _ =
    Inputs.puzzle_input |> solve |> printf "%s\n";
    [%expect {| 22199 |}]
end

(*--- Part Two ---
  The sandstorm is upon you and you aren't any closer to escaping the wasteland. You had the camel follow the instructions, but you've barely left your starting position. It's going to take significantly more steps to escape!

  What if the map isn't for people - what if the map is for ghosts? Are ghosts even bound by the laws of spacetime? Only one way to find out.

  After examining the maps a bit longer, your attention is drawn to a curious fact: the number of nodes with names ending in A is equal to the number ending in Z! If you were a ghost, you'd probably just start at every node that ends with A and follow all of the paths at the same time until they all simultaneously end up at nodes that end with Z.

  For example:

  LR

  11A = (11B, XXX)
  11B = (XXX, 11Z)
  11Z = (11B, XXX)
  22A = (22B, XXX)
  22B = (22C, 22C)
  22C = (22Z, 22Z)
  22Z = (22B, 22B)
  XXX = (XXX, XXX)
  Here, there are two starting nodes, 11A and 22A (because they both end with A). As you follow each left/right instruction, use that instruction to simultaneously navigate away from both nodes you're currently on. Repeat this process until all of the nodes you're currently on end with Z. (If only some of the nodes you're on end with Z, they act like any other node and you continue as normal.) In this example, you would proceed as follows:

  Step 0: You are at 11A and 22A.
  Step 1: You choose all of the left paths, leading you to 11B and 22B.
  Step 2: You choose all of the right paths, leading you to 11Z and 22C.
  Step 3: You choose all of the left paths, leading you to 11B and 22Z.
  Step 4: You choose all of the right paths, leading you to 11Z and 22B.
  Step 5: You choose all of the left paths, leading you to 11B and 22C.
  Step 6: You choose all of the right paths, leading you to 11Z and 22Z.
  So, in this example, you end up entirely on nodes that end in Z after 6 steps.

  Simultaneously start on every node that ends with A. How many steps does it take before you're only on nodes that end with Z?
*)
module Part2 = struct
  type row = {
    node : string;
    left : string;
    right : string;
  }
  [@@deriving sexp]

  type game = {
    instructions : char list;
    graph : row Map.M(String).t;
  }
  [@@deriving sexp]

  let parse_row row : row =
    String.split row ~on:'='
    |> function
    | [node; lr] -> (
      let node = String.strip node in
      String.split lr ~on:','
      |> List.map
           ~f:
             (String.strip ~drop:(function
               | ' ' | '(' | ')' -> true
               | _ -> false
               )
               )
      |> function
      | [left; right] -> { node; left; right }
      | _ -> assert false
    )
    | _ -> assert false

  let parse input : game =
    String.split_lines input
    |> function
    | instructions :: _ :: rows ->
      let instructions = String.to_list instructions in
      let rows = List.map rows ~f:parse_row in
      let graph =
        List.fold rows
          ~init:(Map.empty (module String))
          ~f:(fun m row -> Map.add_exn m ~key:row.node ~data:row)
      in
      { instructions; graph }
    | _ -> assert false

  let%expect_test _ =
    Inputs.example_input |> parse |> [%sexp_of: game] |> print_sexp;
    [%expect
      {|
      ((instructions (R L))
       (graph
        ((AAA ((node AAA) (left BBB) (right CCC)))
         (BBB ((node BBB) (left DDD) (right EEE)))
         (CCC ((node CCC) (left ZZZ) (right GGG)))
         (DDD ((node DDD) (left DDD) (right DDD)))
         (EEE ((node EEE) (left EEE) (right EEE)))
         (GGG ((node GGG) (left GGG) (right GGG)))
         (ZZZ ((node ZZZ) (left ZZZ) (right ZZZ)))))) |}]

  let solve input =
    let game = parse input in
    let state =
      Map.to_sequence game.graph
      |> Sequence.filter_map ~f:(fun (node, _) ->
             if String.is_suffix node "A" then Some node else None
         )
      |> Sequence.to_array
    in
    let is_done () =
      Array.for_all state ~f:(fun node -> String.is_suffix node "Z")
    in
    let rec loop i instructions =
      if is_done () || i > 10000 then i
      else begin
        match instructions with
        | [] -> loop i game.instructions
        | 'L' :: instructions ->
          Array.map_inplace state ~f:(fun node ->
              (Map.find_exn game.graph node).left
          );
          loop (i + 1) instructions
        | 'R' :: instructions ->
          Array.map_inplace state ~f:(fun node ->
              (Map.find_exn game.graph node).right
          );
          loop (i + 1) instructions
      end
    in
    loop 0 game.instructions |> Int.to_string

  let%expect_test _ =
    Inputs.example_input3 |> solve |> printf "%s\n";
    [%expect {|
      6 |}]

  let%expect_test _ =
    Inputs.puzzle_input |> solve |> printf "%s\n";
    [%expect {|
      10001 |}]
end
