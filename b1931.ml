let () =
  let n = Scanf.scanf " %d" Fun.id in
  let ns = List.init n
      (fun _ -> Scanf.scanf " %d %d" (fun a b -> a, b))
  in
  List.sort (fun (a, b) (c, d) ->
      match compare a c with
      | 0 -> compare b d
      | v -> v) ns
  |> List.fold_left
    (fun (i, e) (a, b) -> if a > e then i+1, b else i, e)
    (0, 0)
  |> fst
  |> print_int
