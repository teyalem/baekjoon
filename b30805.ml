let find_index n idx ns =
  let rec aux i = function
    | [] -> None
    | m :: ns ->
      if n = m && i > idx then Some i
      else aux (i+1) ns
  in
  aux 0 ns

let find_index n idx ns =
  find_index n idx ns
  |> Option.value ~default: ~-1

let solve n m a b =
  let aux (out, h, ai, bi) (n, i) =
    let j = find_index n bi b in
    if n <= h && i > ai && j > bi then
      n::out, n, i, j
    else
      out, h, ai, bi
  in

  a
  |> List.mapi (fun i n -> n, i)
  |> List.sort (fun (a, i) (b, j) ->
      match Int.compare b a with
      | 0 -> Int.compare i j
      | v -> v)
  |> List.fold_left aux ([], max_int, -1, -1) 
  |> (fun (out, _, _, _) -> List.rev out)

let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let a = List.init n (fun _ -> read ()) in
  let m = read () in
  let b = List.init m (fun _ -> read ()) in

  let ks = solve n m a b in
  Printf.printf "%d\n" @@ List.length ks;
  List.iter (Printf.printf "%d ") ks
