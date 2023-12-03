open! Base
open! Stdio

let () =
  printf "Hello, %s!\n" "Steffen";
  List.iter [ 1; 2; 3 ] ~f:(fun i -> printf "Hello, number %d" i);
  ()
