let to_num = function
  | 'A' | 'B' | 'C' -> 2
  | 'D' | 'E' | 'F' -> 3
  | 'G' | 'H' | 'I' -> 4
  | 'J' | 'K' | 'L' -> 5
  | 'M' | 'N' | 'O' -> 6
  | 'P' | 'Q' | 'R' | 'S' -> 7
  | 'T' | 'U' | 'V' -> 8
  | 'W' | 'X' | 'Y' | 'Z' -> 9
  | _ -> assert false

let () =
  read_line ()
  |> String.to_seq
  |> Seq.map (fun c -> to_num c + 1)
  |> Seq.fold_left Int.add 0
  |> print_int
