module M = Map.Make(Int)

let add_to_list k v g =
  M.update k (function
      | None -> Some [v]
      | Some vs -> Some (v::vs))
    g

let dk graph n dest =
  let dist = Array.make (n+1) max_int in
  let queue = ref M.empty in

  let add w n = queue := add_to_list w n !queue in
  let rec pop () =
    match M.min_binding_opt !queue with
    | None -> None
    | Some (w, ns) ->
      begin match ns with
        | [] ->
          queue := M.remove w !queue;
          pop ()
        | n :: ns ->
          queue := M.add w ns !queue;
          Some (w, n)
      end
  in

  let rec aux () =
    match pop () with
    | None -> ()
    | Some (w, n) ->
      if dist.(n) <> max_int then aux ()
      else begin
        dist.(n) <- w;
        M.find_opt n graph
        |> Option.value ~default: []
        |> List.iter (fun (w0, d) ->
            add (w+w0) d);
        aux ()
      end
  in

  add 0 dest; aux (); dist

let reverse graph =
  M.to_seq graph
  |> Seq.fold_left (fun m (k, es) ->
      List.fold_left
        (fun m (w, e) -> add_to_list e (w, k) m)
        m es)
    M.empty

let solve graph n x =
  let dist = dk graph n x in
  let rdist = dk (reverse graph) n x in

  List.init n (fun i -> i+1)
  |> List.map (fun i ->
      dist.(i) + rdist.(i))
  |> List.fold_left max min_int

open Scanf

let () =
  scanf " %d %d %d" @@ fun n m x ->
  let graph =
    List.init m (fun _ ->
        scanf " %d %d %d" (fun a b l -> a, b, l))
    |> List.fold_left (fun g (a, b, l) ->
        add_to_list a (l, b) g)
      M.empty
  in
  solve graph n x |> print_int
