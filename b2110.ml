let check d c ns =
  let rec aux last n = function
    | [] -> n = 0
    | x :: xs ->
      if n = 0 then true
      else if x >= last + d then
        aux x (n-1) xs
      else
        aux last n xs
  in
  aux min_int c ns

let solve c ns =
  let rec aux l h =
    if l = h then l
    else
      let m = (l+h+1)/2 in
      if check m c ns then
        aux m h
      else
        aux l (m-1)
  in
  aux 1 @@ List.fold_left max min_int ns

let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let c = read () in
  let ns =
    List.init n (fun _ -> read ())
    |> List.sort Int.compare
  in
  print_int @@ solve c ns
