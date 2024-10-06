let rec iter tree f node =
  if node <> '.' then
    let l, r = List.assoc node tree in
    f (iter tree f) (l, node, r)

let () =
  let open Scanf in
  scanf "%d" @@ fun n ->
  let tree =
    List.init n (fun _ ->
        scanf " %c %c %c" @@ fun a b c ->
        a, (b, c))
  in
  let iter f = iter tree f 'A' in
  iter (fun k (l, c, r) -> print_char c; k l; k r);
  print_newline ();
  iter (fun k (l, c, r) -> k l; print_char c; k r);
  print_newline ();
  iter (fun k (l, c, r) -> k l; k r; print_char c)
