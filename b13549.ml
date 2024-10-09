type ('a, 'b) t = Left of 'a | Right of 'b

let dfs n k =
  let visited = Hashtbl.create 100 in
  let rec round t q =
    let rec aux buf = function
      | [] -> Left buf
      | x :: xs ->
        if x = k then Right t
        else if (not @@ Hashtbl.mem visited x)
      || Hashtbl.find visited x > t
        then
          let _ = Hashtbl.add visited x t in
          let walk = List.filter (fun x -> 0 <= x && x <= 100_000) [x-1; x+1] in
          aux (walk @ buf) @@
          if 2*x <= 100_000 then 2*x :: xs
          else xs
        else
          aux buf xs
    in

    match aux [] q with
    | Left q -> round (t+1) q
    | Right t -> t
  in

  round 0 [n]

let () =
  Scanf.scanf "%d %d" @@ fun n k ->
  dfs n k |> print_int
