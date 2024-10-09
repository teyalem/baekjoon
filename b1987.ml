let idx c = Char.(code c - code 'A')
let get arr c = arr.(idx c)
let set arr c v = arr.(idx c) <- v

let solve row col map =
  let rec aux d (r, c) alphas =
    if d = 26 then 26
    else
      match
        [1, 0; -1, 0; 0, -1; 0, 1]
        |> List.filter_map (fun (dr, dc) ->
            let r, c = r+dr, c+dc in
            if 0 <= r && r < row && 0 <= c && c < col && (not @@ get alphas map.(r).(c)) then
              Some (r, c)
            else
              None)
      with
      | [] -> d
      | edges -> fold d alphas edges

  and fold d alphas = function
    | [] -> min_int
    | (r, c) :: edges ->
      if d = 26 then 26
      else begin
        set alphas map.(r).(c) true;
        let v = aux (d+1) (r, c) alphas in
        set alphas map.(r).(c) false;
        max v @@ fold d alphas edges
      end
  in

  let alphas = Array.make 26 false in
  set alphas map.(0).(0) true;
  aux 1 (0, 0) alphas

let () =
  Scanf.scanf "%d %d" @@ fun r c ->
  let map =
    Array.init r (fun _ ->
        Array.init c (fun _ ->
            Scanf.scanf " %c" Fun.id))
  in
  solve r c map |> print_int
