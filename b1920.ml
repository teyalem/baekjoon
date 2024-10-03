let find_mem n ns k =
  let rec aux l h =
    if l = h then ns.(l) = k
    else
      let m = (l+h)/2 in
      if ns.(m) >= k then
        aux l m
      else
        aux (m+1) h
  in
  aux 0 (n-1)

let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let ns = Array.init n (fun _ -> read ()) in
  Array.sort Int.compare ns;
  let m = read () in
  for _ = 1 to m do
    (if read () |> find_mem n ns
     then 1 else 0)
    |> Printf.printf "%d\n"
  done
