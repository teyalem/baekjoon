let idx c = Char.(code c - code 'A')
let bit c = 1 lsl idx c
let set set c = set lor bit c
let unset set c = set land lnot (bit c)
let mem set c = set land (bit c) > 0

let solve row col map =
  let visited = Array.make_matrix row col 0 in
  let rec aux (r, c) alphas =
    match
      [1, 0; -1, 0; 0, -1; 0, 1]
      |> List.filter_map (fun (dr, dc) ->
          let r, c = r+dr, c+dc in
          if 0 <= r && r < row && 0 <= c && c < col && (not @@ mem alphas map.(r).(c)) then
            Some (r, c)
          else
            None)
    with
    | [] -> 1
    | edges -> 1 + fold alphas edges

  and fold alphas = function
    | [] -> min_int
    | (r, c) :: edges ->
      if visited.(r).(c) = alphas then
        fold alphas edges
      else begin
        visited.(r).(c) <- alphas;
        let nalphas = set alphas map.(r).(c) in
        let v = aux (r, c) nalphas in
        max v @@ fold alphas edges
      end
  in

  let alphas = set 0 map.(0).(0) in
  aux (0, 0) alphas

let () =
  Scanf.scanf "%d %d" @@ fun r c ->
  let map =
    Array.init r (fun _ ->
        Array.init c (fun _ ->
            Scanf.scanf " %c" Fun.id))
  in
  solve r c map |> print_int
