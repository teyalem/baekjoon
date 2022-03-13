let solve arr len =
  let memo = Hashtbl.create 100 in
  let rec aux ns last i =
    if i >= len then 0
    else if Hashtbl.mem memo (i, last) then
      Hashtbl.find memo (i, last)
    else if arr.(i) > last then
      let a = 1 + aux ((i, arr.(i))::ns) arr.(i) (i+1) (* max subseq with this *)
      and b = aux ns last (i+1) in (* without this *)
      let r = max a b in
      begin Hashtbl.add memo (i, last) r; r end
    else (* must skip *)
      aux ns last (i+1)
  in
  aux [] 0 0

let () =
  let n = Scanf.scanf " %d" Fun.id in
  let arr = Array.init n (fun _ -> Scanf.scanf " %d" Fun.id) in
  solve arr n |> print_int
