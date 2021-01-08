let digits n =
  string_of_int n
  |> String.to_seq
  |> Seq.map (fun c -> (int_of_char c) - (Char.code '0'))

let digitsum n =
  let d = digits n |> Seq.fold_left (fun p n -> p+n) 0
  in n+d

let find_gen n =
  let rec inner i =
    if i = n then 0
    else
      let d = digitsum i in
      if d = n then i else inner (i+1)
  in inner 1

let _ = read_int () |> find_gen |> print_int
