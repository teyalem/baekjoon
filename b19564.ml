let () =
  read_line ()
  |> String.to_seq
  |> Seq.fold_left (fun (acc, p) c ->
      if p >= c then
        acc+1, c
      else
        acc, c)
    (0, 'z')
  |> fst
  |> print_int
