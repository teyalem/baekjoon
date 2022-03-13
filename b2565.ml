let solve arr len =
  Array.sort (fun a b -> compare (fst a) (fst b)) arr;
  let module H = Hashtbl in
  let m = H.create 100 in
  let mem k = H.mem m k
  and get k = H.find m k
  and set k v = H.add m k v in
  let rec aux i (ll, rl as last) =
    if i >= len then 0
    else if mem (i, last) then get (i, last)
    else
      let r =
        let l, r = arr.(i) in
        if r > rl
        then max (1 + aux (i+1) arr.(i)) (aux (i+1) last)
        else aux (i+1) last
      in
      begin set (i, last) r; r end
  in
  len - aux 0 (0, 0)

let () =
  let len = Scanf.scanf " %d" Fun.id in
  let arr = Array.init len
      (fun _ -> Scanf.scanf " %d %d" (fun x y -> x, y))
  in
  solve arr len |> print_int
