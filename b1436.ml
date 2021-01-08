let int_to_digits n =
  let s = string_of_int n in
  List.init (String.length s) (fun i -> s.[i])

let is_dn n =
  let c =
    int_to_digits n
    |> List.fold_left (fun p c ->
        if c = '6' then p+1
        else if p >= 3 then p else 0) 0
  in if c >= 3 then true else false

let rec find_dn n d =
  if is_dn d then
    if n = 1 then d
    else find_dn (n-1) (d+1)
  else find_dn n (d+1)

let solve n =
  find_dn n 666

let _ =
  read_int () |> solve |> print_int
