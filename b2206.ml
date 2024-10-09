type t = (int * int) * bool
type state = Yet of t list | Found

let edges n m map ((x, y), b) =
  [1, 0; -1, 0; 0, -1; 0, 1]
  |> List.filter_map (fun (dx, dy) ->
      let x, y = x + dx, y + dy in
      if 0 <= x && x < n && 0 <= y && y < m then
        if map.(x).[y] = '0' then
          Some ((x, y), b)
        else if b = false then
          Some ((x, y), true)
        else
          None
      else
        None)

let bfs n m map =
  let visited = Array.make_matrix n m false in
  let bvisited = Array.make_matrix n m false in
  let g b = if b then bvisited else visited in

  let rec round t ps =
    let rec aux buf = function
      | [] -> Yet buf
      | ((x, y), b) :: ps ->
        if (x, y) = (n-1, m-1) then Found
        else if (g b).(x).(y) then
          aux buf ps
        else begin
          (g b).(x).(y) <- true;
          let es = edges n m map ((x, y), b) in
          aux (es @ buf) ps
        end
    in

    match aux [] ps with
    | Found -> t
    | Yet ps ->
      if ps = [] then -1 else round (t+1) ps
  in

  round 1 [(0, 0), false]

let () =
  Scanf.scanf "%d %d" @@ fun n m ->
  let map = Array.init n (fun _ ->
      Scanf.scanf " %s" Fun.id)
  in
  bfs n m map |> print_int
