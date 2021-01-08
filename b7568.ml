type dungchi = { weight: int; height: int }

let read_dungchi () =
  Scanf.scanf " %d %d" (fun w h -> {weight = w; height = h})

let rec read_dunchi_list i =
  if i = 0 then []
  else
    let d = read_dungchi ()
    in d :: (read_dunchi_list (i-1))

let bigger a b =
  a.weight > b.weight && a.height > b.height

let count_bigger ds d =
  List.filter (fun a -> bigger a d) ds |> List.length
                                         
let rec print_int_list = function
  | [] -> print_newline ()
  | n::ns ->
    Printf.printf "%d " n;
    print_int_list ns

let _ =
  let ds = read_int () |> read_dunchi_list in
  List.map (count_bigger ds) ds
  |> List.map (Int.add 1)
  |> print_int_list
