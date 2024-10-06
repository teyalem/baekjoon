let rec gen m ns =
  if m = 0 then [[]]
  else
    List.concat_map (fun n ->
        ns
        |> List.filter (fun k -> k <> n)
        |> gen (m-1)
        |> List.map (fun xs -> n :: xs))
      ns

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
