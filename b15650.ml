let gen n m =
  let rec aux i p =
    if i = 0 then [[]]
    else
      List.init (n-p+1) (fun k -> p+k)
      |> List.concat_map (fun k ->
          aux (i-1) k
          |> List.map (fun ns -> k :: ns))
  in
  aux m 1

let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let m = read () in
  gen n m
  |> List.iter (fun ns ->
      let open Printf in
      printf "%d" @@ List.hd ns;
      List.iter (printf " %d") @@ List.tl ns;
      printf "\n")
