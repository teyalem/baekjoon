let solve ts m =
  let check l =
    let lt = ref 0 in
    List.iter (fun t ->
        if t > l then lt := !lt + (t-l))
      ts;
    !lt >= m
  in

  let rec aux l h =
    if l = h then h
    else
      let m = (l+h+1)/2 in
      if check m then
        aux m h
      else
        aux l (m-1)
  in
  aux 0 1_000_000_000

let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let m = read () in
  let ts = List.init n (fun _ -> read ()) in
  print_int @@ solve ts m
