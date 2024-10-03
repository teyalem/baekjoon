module H = Hashtbl

let read () = Scanf.scanf " %d" Fun.id

let () =
  let n = read () in
  let t = H.create n in
  for _ = 1 to n do
    let k = read () in
    match H.find_opt t k with
    | None -> H.add t k 1
    | Some i -> H.replace t k (i+1)
  done;
  let m = read () in
  for _ = 1 to m do
    let i =
      match H.find_opt t @@ read () with
      | None -> 0
      | Some i -> i
    in
    Printf.printf "%d " i
  done
