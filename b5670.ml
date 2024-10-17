type edge = {
  src : string;
  idx : int;
  len : int;
}

and tree =
  | Nil
  | Tree of bool * (char, edge * tree) Hashtbl.t

let get { src; idx; len } i =
  if i >= len then None
  else Some src.[idx + i]

let find_diff a b =
  let rec aux i =
    match get a i, get b i with
    | Some c1, Some c2 ->
      if c1 = c2 then aux (i+1)
      else i
    | _ -> i
  in
  aux 0

let advance n i =
  { n with idx = n.idx + i;
           len = n.len - i; }

let empty = Nil

let singleton k v =
  Hashtbl.of_seq @@ Seq.return (k, v)

let make_tree xs =
  let xs =
    List.filter_map (fun (e, n) ->
        match get e 0 with
        | None -> None
        | Some c -> Some (c, (e, n)))
      xs
  in
  Tree (List.length xs = 1,
        Hashtbl.of_seq @@ List.to_seq xs)

let update k f t = 
  match get k 0 with
  | None ->
    begin match t with
      | Nil -> Nil
      | Tree (_, g) -> Tree (true, g)
    end
  | Some k ->
    begin match t with
      | Nil ->
        Tree (true, singleton k @@ f None)
      | Tree (b, g) ->
        Hashtbl.(replace g k (f @@ find_opt g k));
        Tree (b, g)
    end

let add tree str =
  let rec aux tree edge =
    tree |> update edge (function
        | None -> edge, empty
        | Some (a, n) ->
          let i = find_diff a edge in
          if i < a.len then
            { a with len = i },
            make_tree [
              advance a i, n;
              advance edge i, empty]
          else
            a, aux n @@ advance edge i)
  in

  let edge = {
    src = str;
    idx = 0;
    len = String.length str;
  }
  in
  aux tree edge

let sum_depth tree =
  let rec aux d tree =
    match tree with
    | Nil -> d
    | Tree (b, g) ->
      Hashtbl.to_seq g
      |> Seq.map (fun (_, (_, n)) ->
          aux (d+1) n)
      |> Seq.fold_left Int.add (if b then d else 0)
  in

  aux 0 tree

let rec loop () =
  let n = Scanf.scanf " %d" Fun.id in
  List.init n (fun _ ->
      Scanf.scanf " %s" Fun.id)
  |> List.sort String.compare
  |> List.fold_left add empty
  |>  sum_depth
  |> (fun s -> Printf.printf "%.2f\n" (float s /. float n));
  loop ()

let () =
  try loop () with End_of_file -> ()
