let solve cs n =
  let check l =
    let nc =
      List.map (fun c -> c/l) cs
      |> List.fold_left Int.add 0
    in
    nc >= n
  in
  let rec aux l h =
    if l = h then l
    else
      let m = (l+h+1)/2 in
      if check m then
        aux m h
      else
        aux l (m-1)
  in
  aux 1 @@ List.fold_left max min_int cs

let read () = Scanf.scanf " %d" Fun.id

let () =
  let k = read () in
  let n = read () in
  let cs = List.init k (fun _ -> read ()) in
  print_int @@ solve cs n
