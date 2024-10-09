module U = struct
  let make n = Array.init n (fun i -> i, 1)

  let parent sets n = fst sets.(n)
  let size sets n = snd sets.(n)

  let find sets n =
    let rec aux n =
      if parent sets n = n then n
      else aux @@ parent sets n
    in
    aux n

  let same sets a b =
    find sets a = find sets b

  let union sets n m =
    let n = find sets n
    and m = find sets m in

    let n, m =
      if size sets n >= size sets m then
        n, m
      else
        m, n
    in

    sets.(n) <- n, size sets n + size sets m;
    sets.(m) <- n, size sets m;
    n

end

let solve sets parties ts =
  List.iter (fun ps ->
      let h = List.hd ps in
      List.iter (fun p -> ignore @@ U.union sets h p) @@ List.tl ps)
    parties;

  parties
  |> List.filter (fun ps ->
      not @@ List.exists (fun p -> U.same sets ts p) ps)
  |> List.length

let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let m = read () in
  let sets = U.make (n+1) in
  let ts =
    List.init (read ()) (fun _ -> read ())
    |> List.fold_left (fun ts n -> U.union sets ts n)
      0
  in
  let parties =
    List.init m (fun _ ->
        List.init (read ()) (fun _ -> read ()))
  in
  solve sets parties ts
  |> print_int
