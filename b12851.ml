module S = Set.Make(Int)
module M = Map.Make(Int)

let bfs n k =
  let rec round i visited ps =
    let aux p t (ks, visited, ps) =
      if S.mem p visited then
        ks, visited, ps
      else
        let visited = S.add p visited in

        let ps =
          [ p-1; p+1; 2*p ]
          |> List.filter (fun p ->
              0 <= p && p <= 100_000)
          |> List.fold_left (fun ps np ->
              M.update np (function
                  | None -> Some t
                  | Some o -> Some (o+t))
                ps)
            ps
        in

        let ks = ks + if p = k then t else 0 in
        ks, visited, ps
    in

    let ks, visited, ps =
      M.fold aux ps (0, visited, M.empty)
    in
    if ks > 0 then i, ks
    else round (i+1) visited ps
  in

  round 0 S.empty (M.singleton n 1)

let () =
  Scanf.scanf "%d %d" @@ fun n k ->
  let t, s = bfs n k in
  Printf.printf "%d\n%d" t s
