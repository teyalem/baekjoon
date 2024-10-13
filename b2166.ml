let rec window = function
  | [] | [_] -> []
  | a::b::xs ->
    (a, b) :: window (b::xs)

let shoelace xs =
  let last = List.(nth xs @@ length xs - 1) in
  window (last::xs)
  |> List.map (fun ((x1, y1), (x2, y2)) ->
      (y1+.y2) *. (x1-.x2) /. 2.0)
  |> List.fold_left Float.add 0.0
  |> Float.abs

let () =
  Scanf.scanf "%d" @@ fun n ->
  List.init n (fun _ ->
      Scanf.scanf " %d %d" @@ fun x y ->
      float x, float y)
  |> shoelace
  |> Printf.printf "%.1f"
