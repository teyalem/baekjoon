module M = Map.Make(Int)

let solve tree =
  let rec aux node =
    match M.find_opt node tree with
    | None -> 0, 0 (* longest arm, radius *)
    | Some cs ->
      let arms, radiuses =
        cs
        |> List.map (fun (w, c) ->
            let a, r = aux c in a+w, r)
        |> List.split
      in
      let arm, radius =
        match List.sort (Fun.flip Int.compare) arms with
        | a::b::ns -> a, a+b
        | [a] -> a, a
        | _ -> assert false
      in
      arm,
      List.fold_left max radius radiuses
  in

  snd @@ aux 1

let () =
  Scanf.scanf "%d" @@ fun n ->
  List.init (n-1) (fun _ ->
      Scanf.scanf " %d %d %d" (fun p c w -> p, c, w))
  |> List.fold_left (fun tree (p, c, w) ->
      M.update p (function
          | None -> Some [w, c]
          | Some cs -> Some ((w, c) :: cs))
        tree)
    M.empty
  |> solve
  |> print_int
