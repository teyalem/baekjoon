open Printf

let solve seq =
  let rec aux stop seq =
    match seq () with
    | Seq.Nil -> Seq.empty
    | Seq.Cons (n, ns) ->
      if n > stop then
        Seq.cons n ns
      else begin
        let ns = aux n ns |> aux stop in
        printf "%d\n" n;
        ns
      end
  in
  ignore @@ (aux max_int seq : int Seq.t)

let rec read_lines () =
  match read_line () with
  | v -> Seq.Cons (v, read_lines)
  | exception End_of_file -> Seq.Nil

let () =
  read_lines
  |> Seq.map int_of_string
  |> solve
