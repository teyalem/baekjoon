let bfs n map (x, y) shark =
  let q = Queue.create () in
  let vd = Array.make_matrix n n false in
  let fs = ref [] in

  let rec aux () =
    match Queue.take_opt q with
    | None -> ()
    | Some (s, (x, y)) ->
      if vd.(y).(x) then aux ()
      else if
        let c = map.(y).(x) in
        0 < c && c < shark
      then begin
        fs := (s, (x, y)) :: !fs;
        aux ()
      end
      else begin
        vd.(y).(x) <- true;
        [ -1, 0; 1, 0; 0, -1; 0, 1 ]
        |> List.map (fun (dx, dy) ->
            x+dx, y+dy)
        |> List.filter (fun (x, y) ->
            0 <= x && x < n &&
            0 <= y && y < n &&
            map.(y).(x) <= shark)
        |> List.iter (fun p ->
            Queue.add (s+1, p) q);
        aux ()
      end
  in

  Queue.add (0, (x, y)) q;
  aux ();
  !fs
  |> List.sort (fun (s1, (x1, y1)) (s2, (x2, y2)) ->
      let f = Int.compare in
      let (=>) a b = if a = 0 then b else a in
      f s1 s2 => f y1 y2 => f x1 x2)
  |> (fun l -> List.nth_opt l 0)

let solve n map shark_pos =
  let rec aux t p g size =
    match bfs n map p size with
    | None -> t
    | Some (s, (x, y)) ->
      map.(y).(x) <- 0;
      let g, size =
        if g = 1 then size+1, size+1
        else g-1, size
      in
      aux (t+s) (x, y) g size
  in

  aux 0 shark_pos 2 2

open Scanf

let () =
  scanf "%d" @@ fun n ->
  let p = ref (0,0) in
  let map =
    Array.init n (fun y ->
        Array.init n (fun x ->
            let c = scanf " %d" Fun.id in
            if c = 9 then
              begin p := (x, y); 0 end
            else c))
  in
  solve n map !p |> print_int
