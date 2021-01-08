open List
open Scanf

let rec all_sum i = function
  | [] -> []
  | ns when i = 1 -> ns
  | n::ns ->
    let s = all_sum (i-1) ns |> map (fun a -> n+a)
    in s @ (all_sum i ns)

let list_max = fold_left max 0

let rec read_list i =
  if i = 0 then []
  else
    let n = scanf " %d" (fun a -> a)
    in n :: (read_list (i-1))

let prog i m = 
  read_list i
  |> all_sum 3
  |> filter (fun n -> n <= m)
  |> list_max 
  |> print_int

let rec print_list = function
  | [] -> print_newline ();
  | x::xs ->
    Printf.printf "%d " x;
    print_list xs

let _ =
  scanf "%d %d\n" prog
