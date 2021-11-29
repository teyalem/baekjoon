let table = [| 1; 2; -1; 0; 3;-1; 4;-1; 4; 5; 1; 6; 7; 6; 4; 0 |]
let noi = "NOISE" and sub = "SUBMARINE"

let rec check pst seq =
  match seq () with
  | Seq.Nil -> if pst = 0 || pst = 5 then sub else noi
  | Seq.Cons (c, seq) ->
    let nst = table.(2*pst + Char.(code c - code '0')) in
    if nst < 0 then noi else check nst seq

let () =
  read_line () |> String.to_seq |> check 0 |> print_endline
