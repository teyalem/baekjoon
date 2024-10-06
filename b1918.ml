let split_while f xs =
  let rec aux ps = function
    | [] -> ps, []
    | x::xs ->
      if f x then aux (x::ps) xs
      else ps, (x::xs)
  in
  aux [] xs

let priority = function
  | '(' -> 0
  | '+' | '-' -> 1
  | '*' | '/' -> 2
  | c ->
    Printf.eprintf "priority: %c\n" c;
    assert false

let shunting_yard str =
  let  aux (buf, ops) c =
    if 'A' <= c && c <= 'Z' then
      c::buf, ops
    else if c = '(' then
      buf, c::ops
    else if c = ')' then
      let bps, aps =
        split_while (fun k -> k <> '(') ops
      in
      bps @ buf, List.tl aps
    else
      let bos, aos =
        split_while (fun k ->
            priority k >= priority c)
          ops
      in
      bos @ buf, c::aos
  in

  String.to_seq str
  |> Seq.fold_left aux ([], [])
  |> (fun (buf, ops) ->
      List.rev_append ops buf)
  |> List.rev
  |> List.to_seq
  |> String.of_seq

let () =
  read_line ()
  |> shunting_yard
  |> print_string
