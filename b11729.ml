let print_move f t =
  Printf.printf "%d %d\n" f t

let print_move_count n =
  let rec count_move n =
    if n = 1 then 1
    else 2 * (count_move (n-1)) + 1
  in
  print_int (count_move n);
  print_newline ()

let print_hanoi_moves n =
  let rec hanoi m s e t = (*move m rings from s to e through t*)
    if m = 1 then print_move s e
    else begin
      hanoi (m-1) s t e;
      print_move s e;
      hanoi (m-1) t e s;
    end
  in hanoi n 1 3 2

let _ =
  let n = read_int () in
  print_move_count n;
  print_hanoi_moves n
