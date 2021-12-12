let solve n =
  let a = Array.make (n+1) ~-1 in
  a.(1) <- 0;
  let rec aux i =
    if a.(i) <> -1 then a.(i)
    else begin
      a.(i) <- aux (i-1);
      if i mod 2 = 0 then a.(i) <- min a.(i) (aux (i/2));
      if i mod 3 = 0 then a.(i) <- min a.(i) (aux (i/3));
      a.(i) <- a.(i) + 1;
      a.(i)
    end
  in
  aux n

let () = read_int () |> solve |> print_int
