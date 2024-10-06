let solve n ns =
  let dp = Array.make (n+1) max_int in
  dp.(0) <- min_int;

  let rec aux k l h =
    if l = h then l
    else
      let m = (l+h)/2 in
      if dp.(m) < k then
        aux k (m+1) h
      else
        aux k l m
  in

  let maxlen = ref 0 in
  for i = 0 to n-1 do
    let a = ns.(i) in
    let l = aux a 0 n in
    maxlen := max !maxlen l;
    if dp.(l-1) < a && a < dp.(l) then
      dp.(l) <- a
  done;
  !maxlen

let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let ns =
    List.init n (fun _ -> read ())
    |> Array.of_list
  in
  solve n ns
  |> print_int
