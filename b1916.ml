module M = Map.Make(Int)

let add_to_list k v =
  M.update k (function
      | None -> Some [v]
      | Some vs -> Some (v::vs))

let dk g src dst =
  let v = Hashtbl.create 100 in
  let q = ref M.empty in

  let add (w, d) = q := add_to_list w d !q in
  let rec pop () =
    match M.min_binding_opt !q with
    | None -> assert false
    | Some (w, ds) ->
      begin match ds with
        | [] -> q := M.remove w !q; pop ()
        | d :: ds -> q := M.add w ds !q; w, d
      end
  in

  let rec aux () =
    let w, p = pop () in
    if p = dst then w
    else if Hashtbl.mem v p then aux ()
    else begin
      Hashtbl.add v p true;
      M.find_opt p g
      |> Option.value ~default: []
      |> List.map (fun (i, d) -> w+i, d)
      |> List.iter add;
      aux ()
    end
  in

  add (0, src);
  aux ()

let read () = Scanf.scanf " %d" Fun.id

let () =
  let _ = read () in
  let m = read () in

  let g = ref M.empty in
  for _ = 1 to m do
    Scanf.scanf " %d %d %d" @@ fun s d w ->
    g := add_to_list s (w, d) !g
  done;

  Scanf.scanf " %d %d" @@ fun s d ->
  print_int @@ dk !g s d
