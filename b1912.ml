let solve arr =
  (* Kadane's algorithm *)
  Array.fold_left (fun (cs, ms) n ->
      let cs = max n (cs+n) in
      cs, max ms cs)
    (0, min_int)
    arr
  |> snd

let () =
  let len = Scanf.scanf " %d" Fun.id in
  let arr = Array.init len (fun _ -> Scanf.scanf " %d" Fun.id) in
  solve arr |> print_int
