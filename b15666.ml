let picks xs =
  let rec aux p = function
    | [] -> []
    | n :: ns ->
      if Some n = p then
        aux (Some n) ns
      else
        (n, n::ns) :: aux (Some n) ns
  in
  aux None xs

let rec gen m ns =
  if m = 0 then [[]]
  else
    picks ns
    |> List.concat_map (fun (n, ns) ->
        ns
        |> gen (m-1)
        |> List.map (fun xs -> n :: xs))

let () =
  Scanf.scanf "%d %d" @@ fun n m ->
  List.init n (fun _ ->
      Scanf.scanf " %d" Fun.id)
  |> List.sort Int.compare
  |> gen m
  |> List.iter (fun xs ->
      let open Printf in
      printf "%d" @@ List.hd xs;
      List.(iter (printf " %d") @@ tl xs);
      printf "\n")
