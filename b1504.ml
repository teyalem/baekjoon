module M = Map.Make(Int)

let add_to_list k v g =
  M.update k (function
      | None -> Some [v]
      | Some vs -> Some (v::vs))
    g

let dk graph n a b =
  let new_flags flags v =
    let va = if v = a then 1 else 0
    and vb = if v = b then 2 else 0 in
    flags lor va lor vb
  in

  let edges node flags =
    M.find_opt node graph
    |> Option.value ~default: []
    |> List.map (fun (w, v) ->
        w, (v, new_flags flags v))
  in

  let visited = Array.init (n+1) (fun _ ->
      Array.make 4 false)
  in
  let queue = ref M.empty in

  let add w n =
    queue := add_to_list w n !queue
  in
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
    | None -> -1
    | Some (w, (node, flags)) ->
      if node = n && flags = 3 then
        w
      else if visited.(node).(flags) then
        aux ()
      else begin
        visited.(node).(flags) <- true;
        edges node flags
        |> List.iter (fun (w0, v) ->
            add (w + w0) v);
        aux ()
      end
  in

  add 0 (1, new_flags 0 1); aux ()

open Scanf
let () =
  scanf " %d %d" @@ fun n e ->
  let m = ref M.empty in
  for _ = 1 to e do
    scanf " %d %d %d" @@ fun a b c ->
    m := add_to_list a (c, b) !m;
    m := add_to_list b (c, a) !m
  done;
  scanf " %d %d" @@ fun v1 v2 ->
  dk !m n v1 v2 |> print_int
