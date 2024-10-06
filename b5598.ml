let () =
  read_line ()
  |> String.map (fun c ->
      let n = Char.(code c - code 'A') in
      Char.(chr @@ code 'A' + (n+23) mod 26))
  |> print_string
