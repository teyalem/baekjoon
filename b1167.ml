module M = Map.Make(Int)

let solve tree root =
  let rec aux p node =
    match
      M.find node tree
      |> List.filter (fun (_, c) -> c <> p)
    with
    | [] -> 0, 0 (* longest arm, radius *)
    | cs ->
      let arms, radiuses =
        cs
        |> List.map (fun (w, c) ->
            let a, r = aux node c in a+w, r)
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

  snd @@ aux root root

let rec read_edges () =
  Scanf.scanf " %d" @@ fun v ->
  if v = -1 then []
  else
    Scanf.scanf " %d" @@ fun w ->
    (w, v) :: read_edges ()

let () =
  Scanf.scanf "%d" @@ fun v ->
  let tree =
    List.init v (fun _ ->
        Scanf.scanf " %d" @@ fun p ->
        p, read_edges ())
    |> List.to_seq
    |> M.of_seq
  in
  let root =
    M.fold (fun k v (x, l) ->
        let vl = List.length v in
        if vl > l then k, vl else x, l)
      tree
      (-1, -1)
    |> fst
  in

  solve tree root |> print_int
