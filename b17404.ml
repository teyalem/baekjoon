let inf = max_int/2

let safe = function
  | 0 -> 1, 2
  | 1 -> 0, 2
  | 2 -> 0, 1
  | _ -> assert false

let solve cost n =
  let dp = Array.init 3 (fun _ ->
      Array.make_matrix n 3 inf)
  in

  for z = 0 to 2 do
    dp.(z).(0).(z) <- cost.(0).(z)
  done;

  for z = 0 to 2 do
    for i = 1 to n-2 do
      for c = 0 to 2 do
        let a, b = safe c in
        dp.(z).(i).(c) <-
          cost.(i).(c) +
          min dp.(z).(i-1).(a) dp.(z).(i-1).(b);
      done
    done
  done;

  let ans = ref inf in
  for z = 0 to 2 do
    for c = 0 to 2 do
      let a, b = safe c in
      let a = dp.(z).(n-2).(a)
      and b = dp.(z).(n-2).(b) in
      if z <> c then
        ans := min !ans
            (cost.(n-1).(c) + min a b)
    done
  done;

  !ans

let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let arr =
    Array.init n (fun _ ->
        Array.init 3 (fun _ -> read ()))
  in
  solve arr n |> print_int
