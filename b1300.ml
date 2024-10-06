let sum n m =
  let cnt = ref 0 in
  for i = 1 to n do
    cnt := !cnt + min (m/i) n
  done;
  !cnt

let solve n k =
  let rec aux l h =
    if l = h then l
    else
      let m = (l+h)/2 in
      if sum n m >= k then
        aux l m
      else
        aux (m+1) h
  in
  aux 1 k

let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let k = read () in
  print_int @@ solve n k
