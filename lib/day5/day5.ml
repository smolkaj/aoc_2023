open! Base
open! Stdio

(*
--- Day 5: If You Give A Seed A Fertilizer ---
You take the boat and find the gardener right where you were told he would be: managing a giant "garden" that looks more to you like a farm.

"A water source? Island Island is the water source!" You point out that Snow Island isn't receiving any water.

"Oh, we had to stop the water because we ran out of sand to filter it with! Can't make snow with dirty water. Don't worry, I'm sure we'll get more sand soon; we only turned off the water a few days... weeks... oh no." His face sinks into a look of horrified realization.

"I've been so busy making sure everyone here has food that I completely forgot to check why we stopped getting more sand! There's a ferry leaving soon that is headed over in that direction - it's much faster than your boat. Could you please go check it out?"

You barely have time to agree to this request when he brings up another. "While you wait for the ferry, maybe you can help us with our food production problem. The latest Island Island Almanac just arrived and we're having trouble making sense of it."

The almanac (your puzzle input) lists all of the seeds that need to be planted. It also lists what type of soil to use with each kind of seed, what type of fertilizer to use with each kind of soil, what type of water to use with each kind of fertilizer, and so on. Every type of seed, soil, fertilizer and so on is identified with a number, but numbers are reused by each category - that is, soil 123 and fertilizer 123 aren't necessarily related to each other.

For example:

seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
The almanac starts by listing which seeds need to be planted: seeds 79, 14, 55, and 13.

The rest of the almanac contains a list of maps which describe how to convert numbers from a source category into numbers in a destination category. That is, the section that starts with seed-to-soil map: describes how to convert a seed number (the source) to a soil number (the destination). This lets the gardener and his team know which soil to use with which seeds, which water to use with which fertilizer, and so on.

Rather than list every source number and its corresponding destination number one by one, the maps describe entire ranges of numbers that can be converted. Each line within a map contains three numbers: the destination range start, the source range start, and the range length.

Consider again the example seed-to-soil map:

50 98 2
52 50 48
The first line has a destination range start of 50, a source range start of 98, and a range length of 2. This line means that the source range starts at 98 and contains two values: 98 and 99. The destination range is the same length, but it starts at 50, so its two values are 50 and 51. With this information, you know that seed number 98 corresponds to soil number 50 and that seed number 99 corresponds to soil number 51.

The second line means that the source range starts at 50 and contains 48 values: 50, 51, ..., 96, 97. This corresponds to a destination range starting at 52 and also containing 48 values: 52, 53, ..., 98, 99. So, seed number 53 corresponds to soil number 55.

Any source numbers that aren't mapped correspond to the same destination number. So, seed number 10 corresponds to soil number 10.

So, the entire list of seed numbers and their corresponding soil numbers looks like this:

seed  soil
0     0
1     1
...   ...
48    48
49    49
50    52
51    53
...   ...
96    98
97    99
98    50
99    51
With this map, you can look up the soil number required for each initial seed number:

Seed number 79 corresponds to soil number 81.
Seed number 14 corresponds to soil number 14.
Seed number 55 corresponds to soil number 57.
Seed number 13 corresponds to soil number 13.
The gardener and his team want to get started as soon as possible, so they'd like to know the closest location that needs a seed. Using these maps, find the lowest location number that corresponds to any of the initial seeds. To do this, you'll need to convert each seed number through other categories until you can find its corresponding location number. In this example, the corresponding types are:

Seed 79, soil 81, fertilizer 81, water 81, light 74, temperature 78, humidity 78, location 82.
Seed 14, soil 14, fertilizer 53, water 49, light 42, temperature 42, humidity 43, location 43.
Seed 55, soil 57, fertilizer 57, water 53, light 46, temperature 82, humidity 82, location 86.
Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34, humidity 35, location 35.
So, the lowest location number in this example is 35.

What is the lowest location number that corresponds to any of the initial seed numbers?   
*)
module Part1 = struct
  type maplet = {
    dst : int;
    src : int;
    len : int;
  }

  type map = maplet list

  type game = {
    seeds : int list;
    seed_to_soil : map;
    soil_to_fertilizer : map;
    fertilizer_to_water : map;
    water_to_light : map;
    light_to_temperature : map;
    temperature_to_humidity : map;
    humidity_to_location : map;
  }

  let parse_seeds (input : string list) : game =
    let input =
      match input with
      | [input] -> input
      | _ -> assert false
    in
    input
    |> String.split ~on:':'
    |> function
    | ["seeds"; nums] ->
      nums
      |> String.strip
      |> String.split ~on:' '
      |> List.map ~f:Int.of_string
      |> fun seeds ->
      {
        seeds;
        seed_to_soil = [];
        soil_to_fertilizer = [];
        fertilizer_to_water = [];
        water_to_light = [];
        light_to_temperature = [];
        temperature_to_humidity = [];
        humidity_to_location = [];
      }
    | _ -> failwith "invalid seeds line"

  type map_type =
    | SS
    | SF
    | FW
    | WL
    | LT
    | TH
    | HL

  let get_map game ~map_type : map =
    match map_type with
    | SS -> game.seed_to_soil
    | SF -> game.soil_to_fertilizer
    | FW -> game.fertilizer_to_water
    | WL -> game.water_to_light
    | LT -> game.light_to_temperature
    | TH -> game.temperature_to_humidity
    | HL -> game.humidity_to_location

  let set_map game ~map_type ~map : game =
    match map_type with
    | SS -> { game with seed_to_soil = map }
    | SF -> { game with soil_to_fertilizer = map }
    | FW -> { game with fertilizer_to_water = map }
    | WL -> { game with water_to_light = map }
    | LT -> { game with light_to_temperature = map }
    | TH -> { game with temperature_to_humidity = map }
    | HL -> { game with humidity_to_location = map }

  let parse_header (header : string) : map_type =
    match header with
    | "seed-to-soil map:" -> SS
    | "soil-to-fertilizer map:" -> SF
    | "fertilizer-to-water map:" -> FW
    | "water-to-light map:" -> WL
    | "light-to-temperature map:" -> LT
    | "temperature-to-humidity map:" -> TH
    | "humidity-to-location map:" -> HL
    | _ -> failwith ("unknown header: \"" ^ header ^ "\"")

  let parse_maplet (maplet : string) ~(map : map) : map =
    match String.split maplet ~on:' ' |> List.map ~f:Int.of_string with
    | [dst; src; len] -> { dst; src; len } :: map
    | _ -> failwith "invalid"

  let%expect_test _ =
    Sequence.range 0 5
    |> Sequence.to_list
    |> [%sexp_of: int list]
    |> Sexp.to_string_hum
    |> printf "%s\n";
    [%expect {| (0 1 2 3 4) |}]

  let parse_map (game : game) (input : string list) : game =
    (* input |> [%sexp_of: string list] |> Sexp.to_string_hum |> printf "%s\n"; *)
    match input with
    | _ :: header :: maplets ->
      let map_type = parse_header header in
      let map =
        List.fold maplets ~init:(get_map game ~map_type) ~f:(fun map maplet ->
            parse_maplet maplet ~map
        )
      in
      set_map game ~map_type ~map
    | _ -> failwith "invalid"

  let parse_input (input : string) : game =
    input
    |> String.split_lines
    |> List.group ~break:(fun _ s -> String.is_empty s)
    |> function
    | seeds :: maps ->
      let game = parse_seeds seeds in
      List.fold maps ~init:game ~f:parse_map
    | _ -> failwith "invalid input"

  let lookup (map : map) (key : int) : int =
    let exception Found of int in
    try
      List.iter map ~f:(fun { dst; src; len } ->
          if src <= key && key < src + len then raise (Found (dst + key - src))
      );
      key
    with Found value -> value

  let solve (input : string) : int =
    let game = parse_input input in
    List.map game.seeds ~f:(fun seed ->
        seed
        |> lookup game.seed_to_soil
        |> lookup game.soil_to_fertilizer
        |> lookup game.fertilizer_to_water
        |> lookup game.water_to_light
        |> lookup game.light_to_temperature
        |> lookup game.temperature_to_humidity
        |> lookup game.humidity_to_location
    )
    |> List.fold ~init:Int.max_value ~f:min

  let%expect_test _ =
    let game = Inputs.example_input |> parse_input in
    game.seeds |> [%sexp_of: int list] |> Sexp.to_string_hum |> printf "%s\n";
    [%expect {|
    (79 14 55 13) |}]

  let%expect_test _ =
    solve Inputs.example_input |> printf "%d\n";
    [%expect {| 35 |}]

  let%expect_test _ =
    solve Inputs.puzzle_input |> printf "%d\n";
    [%expect {| 600279879 |}]
end

(*
--- Part Two ---
Everyone will starve if you only plant such a small number of seeds. Re-reading the almanac, it looks like the seeds: line actually describes ranges of seed numbers.

The values on the initial seeds: line come in pairs. Within each pair, the first value is the start of the range and the second value is the length of the range. So, in the first line of the example above:

seeds: 79 14 55 13
This line describes two ranges of seed numbers to be planted in the garden. The first range starts with seed number 79 and contains 14 values: 79, 80, ..., 91, 92. The second range starts with seed number 55 and contains 13 values: 55, 56, ..., 66, 67.

Now, rather than considering four seed numbers, you need to consider a total of 27 seed numbers.

In the above example, the lowest location number can be obtained from seed number 82, which corresponds to soil 84, fertilizer 84, water 84, light 77, temperature 45, humidity 46, and location 46. So, the lowest location number is 46.

Consider all of the initial seed numbers listed in the ranges on the first line of the almanac. What is the lowest location number that corresponds to any of the initial seed numbers?   
*)

(*
   let%expect_test _ =
     solve Inputs.example_input |> printf "%d\n";
     [%expect {| |}]

   let%expect_test _ =
     solve Inputs.puzzle_input |> printf "%d\n";
     [%expect {| |}] 
*)

type interval = {
  start : int;
  len : int;
}
[@@deriving sexp]

let compare_interval x y = Int.compare x.start y.start

type maplet = {
  src_interval : interval;
  dst_start : int;
}
[@@deriving sexp]

let compare_maplet x y = compare_interval x.src_interval y.src_interval

type map = maplet list [@@deriving sexp]

type game = {
  seeds : interval list;
  seed_to_soil : map;
  soil_to_fertilizer : map;
  fertilizer_to_water : map;
  water_to_light : map;
  light_to_temperature : map;
  temperature_to_humidity : map;
  humidity_to_location : map;
}
[@@deriving sexp]

let empty_game =
  {
    seeds = [];
    seed_to_soil = [];
    soil_to_fertilizer = [];
    fertilizer_to_water = [];
    water_to_light = [];
    light_to_temperature = [];
    temperature_to_humidity = [];
    humidity_to_location = [];
  }

let print_sexp s = Sexp.to_string_hum s |> printf "%s\n"

let tap x ~f =
  f x;
  x

let parse_seeds (input : string list) : interval list =
  let input =
    match input with
    | [input] -> input
    | _ -> assert false
  in
  input
  |> String.split ~on:':'
  |> function
  | ["seeds"; nums] ->
    let break = ref false in
    nums
    |> String.strip
    |> String.split ~on:' '
    |> List.map ~f:Int.of_string
    |> List.group ~break:(fun _ _ ->
           let br = !break in
           break := not br;
           br
       )
    |> List.map ~f:(function
         | [start; len] -> { start; len }
         | _ -> failwith "invalid"
         )
    |> List.sort ~compare:compare_interval
  | _ -> assert false

type map_type =
  | SS
  | SF
  | FW
  | WL
  | LT
  | TH
  | HL

let get_map game ~map_type : map =
  match map_type with
  | SS -> game.seed_to_soil
  | SF -> game.soil_to_fertilizer
  | FW -> game.fertilizer_to_water
  | WL -> game.water_to_light
  | LT -> game.light_to_temperature
  | TH -> game.temperature_to_humidity
  | HL -> game.humidity_to_location

let set_map game ~map_type ~map : game =
  match map_type with
  | SS -> { game with seed_to_soil = map }
  | SF -> { game with soil_to_fertilizer = map }
  | FW -> { game with fertilizer_to_water = map }
  | WL -> { game with water_to_light = map }
  | LT -> { game with light_to_temperature = map }
  | TH -> { game with temperature_to_humidity = map }
  | HL -> { game with humidity_to_location = map }

let parse_header (header : string) : map_type =
  match header with
  | "seed-to-soil map:" -> SS
  | "soil-to-fertilizer map:" -> SF
  | "fertilizer-to-water map:" -> FW
  | "water-to-light map:" -> WL
  | "light-to-temperature map:" -> LT
  | "temperature-to-humidity map:" -> TH
  | "humidity-to-location map:" -> HL
  | _ -> failwith ("unknown header: \"" ^ header ^ "\"")

let parse_maplet (maplet : string) : maplet =
  match String.split maplet ~on:' ' |> List.map ~f:Int.of_string with
  | [dst; src; len] -> { src_interval = { start = src; len }; dst_start = dst }
  | _ -> failwith "invalid"

let%expect_test _ =
  Sequence.range 0 5
  |> Sequence.to_list
  |> [%sexp_of: int list]
  |> Sexp.to_string_hum
  |> printf "%s\n";
  [%expect {| (0 1 2 3 4) |}]

let parse_map (input : string list) : map_type * map =
  match input with
  | _ :: header :: maplets ->
    List.map maplets ~f:parse_maplet
    |> List.sort ~compare:compare_maplet
    |> fun map -> parse_header header, map
  | _ -> failwith "invalid"

let parse_input (input : string) : game =
  input
  |> String.split_lines
  |> List.group ~break:(fun _ s -> String.is_empty s)
  |> function
  | seeds :: maps ->
    let seeds = parse_seeds seeds in
    List.map ~f:parse_map maps
    |> List.fold ~init:{ empty_game with seeds } ~f:(fun game (map_type, map) ->
           set_map game ~map_type ~map
       )
  | _ -> failwith "invalid input"

let make_interval (start : int) (end_exclusive : int) : interval =
  let len = end_exclusive - start in
  if len > 0 then { start; len } else { start = 0; len = 0 }

let interval_end_excl (i : interval) = i.start + i.len

let reduce_interval inerval delta =
  if delta >= inerval.len then { start = 0; len = 0 }
  else { start = inerval.start + delta; len = inerval.len - delta }

let maplet_lookup (key : interval) (maplet : maplet) : interval * interval list
    =
  (* printf "-----------------------------------------------\nmaplet: ";
     print_sexp ([%sexp_of: maplet] maplet);
     printf "key: ";
     print_sexp ([%sexp_of: interval] key); *)
  let i1 =
    make_interval key.start
      (min (interval_end_excl key) maplet.src_interval.start)
  in
  (* printf "i1: ";
     print_sexp ([%sexp_of: interval] i1); *)
  let key = reduce_interval key i1.len in
  (* printf "remaining key: ";
     print_sexp ([%sexp_of: interval] key); *)
  assert (key.start >= maplet.src_interval.start || key.len = 0);
  let offset = key.start - maplet.src_interval.start in
  let len = min key.len (reduce_interval maplet.src_interval offset).len in
  let i2 = { start = maplet.dst_start + offset; len } in
  (* printf "i2: ";
     print_sexp ([%sexp_of: interval] i2); *)
  let key = reduce_interval key i2.len in
  (* printf "remaining key: ";
     print_sexp ([%sexp_of: interval] key); *)
  let values = [i1; i2] in
  key, values

let lookup (map : map) (key : interval) : interval list =
  (* printf "==================================================\nmap: ";
     print_sexp ([%sexp_of: map] map); *)
  let remainder, results = List.fold_map map ~init:key ~f:maplet_lookup in
  List.concat results
  |> List.cons remainder
  |> List.filter ~f:(fun i -> i.len > 0)
  |> List.sort ~compare:compare_interval
(* |> tap ~f:(fun is ->
       printf "-------------------------------------------------\nresult: ";
       print_intervals is
   ) *)

let solve (input : string) : int =
  let game = parse_input input in
  game.seeds
  (* |> tap ~f:(fun is -> print_sexp ([%sexp_of: interval list] is)) *)
  |> List.concat_map ~f:(lookup game.seed_to_soil)
  |> List.concat_map ~f:(lookup game.soil_to_fertilizer)
  |> List.concat_map ~f:(lookup game.fertilizer_to_water)
  |> List.concat_map ~f:(lookup game.water_to_light)
  |> List.concat_map ~f:(lookup game.light_to_temperature)
  |> List.concat_map ~f:(lookup game.temperature_to_humidity)
  |> List.concat_map ~f:(lookup game.humidity_to_location)
  |> List.map ~f:(fun { start; _ } -> start)
  |> List.fold ~init:Int.max_value ~f:min

let%expect_test _ =
  solve Inputs.example_input |> printf "%d\n";
  [%expect {|
    46 |}]

let%expect_test _ =
  solve Inputs.puzzle_input |> printf "%d\n";
  [%expect {| 20191102 |}]
