let solve n arr =
  let ml = ref arr.(0)
  and mh = ref arr.(n-1) in

  let report l h =
    if abs (!ml + !mh) >= abs (arr.(l) + arr.(h)) then
      begin ml := arr.(l); mh := arr.(h) end
  in

  let l = ref 0 and h = ref (n-1) in
  while !l < !h do
    while !l < !h && arr.(!l) + arr.(!h) > 0 do
      report !l !h;
      decr h
    done;
    if !l < !h then report !l !h;
    incr l
  done;

  !ml, !mh

let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let l, h =
    Array.init n (fun _ -> read ())
    |> solve n
  in
  Printf.printf "%d %d\n" l h
