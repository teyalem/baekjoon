module M = Map.Make(Int)

let add_to_list k v g =
  M.update k (function
      | None -> Some [v]
      | Some vs -> Some (v::vs))
    g

let solve n r graph =
  let size = Array.make (n+1) 0 in

  let rec aux i p =
    M.find i graph
    |> List.filter (fun n -> n <> p)
    |> List.map (fun n -> aux n i)
    |> List.fold_left Int.add 1
    |> (fun s -> size.(i) <- s; s)
  in

  ignore @@ aux r 0;
  size

open Scanf

let () =
  scanf " %d %d %d" @@ fun n r q ->
  let g =
    List.init (n-1) (fun _ ->
        scanf " %d %d" @@ fun s e -> s, e)
  |> List.fold_left (fun g (s, e) ->
        g
        |> add_to_list s e
        |> add_to_list e s)
    M.empty
  in
  let size = solve n r g in
  for _ = 1 to q do
    scanf " %d" @@ fun i ->
    Printf.printf "%d\n" size.(i)
  done
