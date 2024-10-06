let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let m = read () in
  let mat = Array.make_matrix (n+1) (n+1) 0 in
  for x = 1 to n do
    for y = 1 to n do
      let k = read () in
      mat.(x).(y) <-
        k
        + mat.(x-1).(y)
        + mat.(x).(y-1)
        - mat.(x-1).(y-1)
    done
  done;

  for _ = 1 to m do
    Scanf.scanf " %d %d %d %d" @@ fun x1 y1 x2 y2 ->
    Printf.printf "%d\n" @@
    mat.(x2).(y2)
    - mat.(x1-1).(y2)
    - mat.(x2).(y1-1)
    + mat.(x1-1).(y1-1)
  done
