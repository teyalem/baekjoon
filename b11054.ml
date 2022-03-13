type t = Inc | Dec
let d2i = function Inc -> 0 | Dec -> 1

let solve arr len =
  (* let memo = Hashtbl.create 100 in *)
  let memo = Array.init 2 (fun _ -> Array.make_matrix 1000 1001 ~-1) in
  let mem m i l = memo.(d2i m).(i).(l) <> ~-1
  and get m i l = memo.(d2i m).(i).(l)
  and set m i l v = memo.(d2i m).(i).(l) <- v
  in
  let rec aux mode last i =
    if i >= len then 0
    else if mem mode i last then
      get mode i last
    else
      let a = 1 + aux Inc arr.(i) (i+1)
      and b = aux Inc last (i+1)
      and c = 1 + aux Dec arr.(i) (i+1)
      and d = aux Dec last (i+1) in
      let r =
        if mode = Inc then
          if arr.(i) > last then
            max a @@ max b @@ max c d
          else if arr.(i) < last then
            max b @@ max c d
          else
            max b d
        else if arr.(i) < last then
          max c d
        else
          d
      in
      begin set mode i last r; r end
  in
  aux Inc 0 0

let () =
  let len = Scanf.scanf " %d" Fun.id in
  let arr = Array.init len (fun _ -> Scanf.scanf " %d" Fun.id) in
  solve arr len |> print_int
