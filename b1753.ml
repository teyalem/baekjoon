module M = Map.Make(Int)

let dijkstra g k =
  let q = ref M.empty in
  let ws = ref M.empty in

  let add w dst =
    q := M.update w (function
        | None -> Some [dst]
        | Some ds -> Some (dst::ds))
        !q
  in

  let rec next () =
    Option.bind
      (M.min_binding_opt !q)
      (fun (w, ds) ->
         match ds with
         | [] ->
           q := M.remove w !q;
           next ()
         | d::ds ->
           q := M.add w ds !q;
           if M.mem d !ws then next ()
           else Some (w, d))
  in

  let rec aux (w, n) =
    ws := M.add n w !ws;
    M.find_opt n g
    |> Option.iter
      (List.iter (fun (dw, d) -> add (w+dw) d));
    Option.iter aux @@ next ()
  in

  aux (0, k);
  !ws

let () =
  let open Scanf in
  let v, e = scanf "%d %d" (fun v e -> v, e) in
  let k = scanf " %d" Fun.id in
  let g = ref M.empty in
  for _ = 1 to e do
    scanf " %d %d %d" (fun u v w ->
        g := M.update u (function
            | None -> Some [w, v]
            | Some ds -> Some ((w, v)::ds))
            !g)
  done;

  let ws = dijkstra !g k in

  let open Printf in
  for i = 1 to v do
    match M.find_opt i ws with
    | None -> printf "INF\n"
    | Some w -> printf "%d\n" w
  done
