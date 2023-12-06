open! Base
open! Stdio

let print_sexp s = Sexp.to_string_hum s |> printf "%s\n"

let tap x ~f =
  f x;
  x

(*

*)
module Part1 = struct
  let parse input = input

  let%expect_test _ =
    Inputs.example_input |> parse |> printf "%s\n";
    [%expect {| |}]

  let solve input = input |> parse

  let%expect_test _ =
    Inputs.example_input |> solve |> printf "%s\n";
    [%expect {| |}]

  let%expect_test _ =
    Inputs.puzzle_input |> solve |> printf "%s\n";
    [%expect {| |}]
end

(*
--- Part Two ---

*)
module Part2 = struct
  let parse input = input

  let%expect_test _ =
    Inputs.example_input |> parse |> printf "%s\n";
    [%expect {| |}]

  let solve input = input |> parse

  let%expect_test _ =
    Inputs.example_input |> solve |> printf "%s\n";
    [%expect {| |}]

  let%expect_test _ =
    Inputs.puzzle_input |> solve |> printf "%s\n";
    [%expect {| |}]
end
