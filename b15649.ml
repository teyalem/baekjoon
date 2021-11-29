open List
open Printf

let rec pick m ns =
  if m = 1
  then map (fun n -> [n]) ns
  else concat_map
      (fun n -> map (fun ns -> n :: ns)
        @@ pick (m-1)
        @@ filter ((<>) n) ns)
      ns

let () =
  Scanf.scanf "%d %d" (fun n m -> pick m @@ init n (Int.add 1))
  |> iter (fun ns ->
      iter (printf "%d ") @@ ns;
      printf "\n")
