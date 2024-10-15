let simulate n m map vs wc =
  let q = List.to_seq vs |> Queue.of_seq in
  let visited = Array.make_matrix n m false in
  let c = ref 0 in

  while not @@ Queue.is_empty q do
    let x, y = Queue.take q in
    if not visited.(y).(x) then begin
      visited.(y).(x) <- true;
      incr c;
      [ -1, 0; 1, 0; 0, -1; 0, 1 ]
      |> List.map (fun (dx, dy) -> x+dx, y+dy)
      |> List.filter (fun (x, y) ->
          0 <= x && x < m &&
          0 <= y && y < n &&
          map.(y).(x) <> 1)
      |> List.iter (fun p -> Queue.add p q)
    end
  done;
  n*m - !c - wc

let solve n m map =
  let vs = ref [] in
  let wc = ref 3 in
  for y = 0 to n-1 do
    for x = 0 to m-1 do
      let c = map.(y).(x) in
      if c = 1 then incr wc;
      if c = 2 then vs := (x, y) :: !vs
    done
  done;

  let rec aux b i =
    if b = 0 then simulate n m map !vs !wc
    else if i >= n*m then min_int
    else
      let x, y = i mod m, i / m in
      if map.(y).(x) <> 0 then
        aux b (i+1)
      else begin
        map.(y).(x) <- 1;
        let r = aux (b-1) (i+1) in
        map.(y).(x) <- 0;
        max r @@ aux b (i+1)
      end
  in

  aux 3 0

let read () = Scanf.scanf " %d" Fun.id

let () =
  Scanf.scanf "%d %d" @@ fun n m ->
  let map =
    Array.init n (fun _ ->
        Array.init m (fun _ -> read ()))
  in
  solve n m map |> print_int
