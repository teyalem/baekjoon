let bf n edges =
  let dis = Array.make (n+1) (max_int/2) in

  for i = 1 to n-1 do
    List.iter (fun (s, e, w) ->
        dis.(e) <- min dis.(e) (w + dis.(s)))
      edges
  done;

  if
    List.exists (fun (s, e, w) ->
        dis.(e) > w + dis.(s))
      edges
  then "YES" else "NO"

open Scanf
let () =
  scanf "%d" @@ fun tc ->
  for _ = 1 to tc do
    scanf " %d %d %d" @@ fun n m w ->
    let roads = List.init m (fun _ ->
        scanf " %d %d %d" @@ fun s e t ->
        s, e, t)
    in
    let revroads = List.map (fun (s, e, t) -> e, s, t) roads in
    let wormholes = List.init w (fun _ ->
        scanf " %d %d %d" @@ fun s e t ->
        s, e, ~-t)
    in
    let edges =
      List.rev_append wormholes @@
      List.rev_append revroads roads
    in

    bf n edges |> print_endline
  done
