module M = Map.Make(Int)

let add_to_list k v g =
  M.update k (function
      | None -> Some [v]
      | Some vs -> Some (v::vs))
    g

let dk n graph src dst =
  let visited = Array.make (n+1) [] in
  let q = ref M.empty in

  let add w n = q := add_to_list w n !q in

  let rec pop () =
    match M.min_binding_opt !q with
    | None -> None
    | Some (w, ns) ->
      begin match ns with
        | [] -> q := M.remove w !q; pop ()
        | n::ns ->
          q := M.add w ns !q;
          Some (w, n)
      end
  in

  let rec aux () =
    match pop () with
    | None -> assert false
    | Some (w, (n, path)) ->
      if visited.(n) <> [] then aux ()
      else if n = dst then
        w, n :: path
      else begin
        visited.(n) <- n :: path;
        M.find_opt n graph
        |> Option.value ~default: []
        |> List.iter (fun (w0, m) ->
            add (w+w0) (m, visited.(n)));
        aux ()
      end
  in

  add 0 (src, []);
  aux ()

open Scanf
open Printf

let () =
  scanf " %d %d" @@ fun n m ->
  let graph =
    List.init m (fun _ ->
        scanf " %d %d %d" @@ fun s e w -> s, e, w)
  |> List.fold_left (fun g (s, e, w) ->
        add_to_list s (w, e) g)
    M.empty
  in
  scanf " %d %d" @@ fun src dst ->
  let cost, path = dk n graph src dst in
  printf "%d\n%d\n" cost (List.length path);
  List.iter (printf "%d ") @@ List.rev path
