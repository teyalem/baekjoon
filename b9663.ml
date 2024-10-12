let nqueen n =
  let col = Array.make n false
  and diag = Array.make (2*n) false
  and rdiag = Array.make (2*n) false in
  let count = ref 0 in
  let rec aux y =
    if y = n then incr count
    else
      for x = 0 to n-1 do
          if not (col.(x) || diag.(x+y) || rdiag.(x-y+n-1)) then begin
            col.(x) <- true;
            diag.(x+y) <- true;
            rdiag.(x-y+n-1) <- true;
            aux (y+1);
            col.(x) <- false;
            diag.(x+y) <- false;
            rdiag.(x-y+n-1) <- false;
          end
        done
  in
  aux 0; !count

let () =
  Scanf.scanf "%d" @@ fun n ->
  nqueen n |> print_int
