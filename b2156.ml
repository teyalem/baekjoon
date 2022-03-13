let solve arr len =
  let memo = Array.make_matrix len 3 None in
  let rec aux fn i =
    if i >= len then 0
    else
      let cache = memo.(i).(fn) in
      if Option.is_some cache then Option.get cache
      else
        let ans =
          if fn = 2 then aux 0 (i+1)
          else max (aux 0 (i+1)) (arr.(i) + aux (fn+1) (i+1))
        in
        memo.(i).(fn) <- Some ans;
        ans
  in
  aux 0 0

let () =
  let n = read_int () in
  let arr = Array.init n (fun _ -> read_int ()) in
  solve arr n |> Printf.printf "%d"
