let rusa = function
  | 'B' -> "v"
  | 'E' -> "ye"
  | 'H' -> "n"
  | 'P' -> "r"
  | 'C' -> "s"
  | 'Y' -> "u"
  | 'X' -> "h"
  | c -> String.make 1 (Char.lowercase_ascii c)

let () =
  read_line ()
  |> String.to_seq
  |> Seq.map rusa
  |> List.of_seq
  |> String.concat ""
  |> print_endline
