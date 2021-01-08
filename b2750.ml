let read_list n =
  List.init n (fun _ -> read_int ())

let rec sort = function
  | [] -> []
  | [n] -> [n]
  | n::ns ->
    let (less, greater) = List.partition (fun c -> c < n) ns
    in (sort less) @ [n] @ (sort greater)

let print_list =
  List.iter (fun n -> print_int n; print_newline ())

let _ =
  let n = read_int () in
  read_list n |> sort |> print_list
