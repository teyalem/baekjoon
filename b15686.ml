let dis (a, b) (x, y) =
  abs (x-a) + abs (y-b)

let cdis hs cs =
  List.fold_left (fun acc h ->
      acc +
      List.fold_left (fun d c ->
          min d @@ dis h c)
        max_int cs)
    0 hs

let rec combi n xs =
  if n = 0 then [[]]
  else
    match xs with
    | [] -> []
    | x :: xs ->
      List.map (fun l -> x :: l) (combi (n-1) xs)
        @ combi n xs

let solve m hs cs =
  combi m cs
  |> List.map (cdis hs)
  |> List.fold_left min max_int

let read () = Scanf.scanf " %d" Fun.id

let () =
  Scanf.scanf "%d %d" @@ fun n m ->
  let hs = ref [] and cs = ref [] in
  for r = 1 to n do
    for c = 1 to n do
      match read () with
      | 1 -> hs := (r, c) :: !hs
      | 2 -> cs := (r, c) :: !cs
      | _ -> ()
    done
  done;

  solve m !hs !cs |> print_int
