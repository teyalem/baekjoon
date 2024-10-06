let solve n adj =
  let ps = Array.make (n+1) ~-1 in
  let rec aux parent i =
    if ps.(i) = -1 then begin
      ps.(i) <- parent;
      List.iter (fun j -> aux i j)
        adj.(i)
    end
  in
  aux 1 1;
  ps

let print n arr =
  for i = 2 to n do
    Printf.printf "%d\n" arr.(i)
  done

let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let adj = Array.make (n+1) [] in
  for _ = 1 to n-1 do
    let a = read () in
    let b = read () in
    adj.(a) <- b :: adj.(a);
    adj.(b) <- a :: adj.(b);
  done;
  solve n adj |> print n
