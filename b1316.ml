module S = Set.Make(Char)

let rec check set p s =
  match s () with
  | Seq.Nil -> true
  | Seq.Cons (c, s) ->
    if S.mem c set && c <> p then false
    else check (S.add c set) c s

let () =
  let n = read_int () in
  List.init n (fun _ -> read_line ())
  |> List.map String.to_seq
  |> List.filter (check S.empty '_')
  |> List.length
  |> print_int
