open Array
type color = White | Black
type board = color array array
exception NotAColor

let color_of_char = function
  | 'W' -> White
  | 'B' -> Black
  | _ -> raise NotAColor

let char_of_color = function
  | White -> 'W'
  | Black -> 'B'
  
let inverse = function
  | White -> Black
  | Black -> White

let read_board (r: int) (c: int): board =
  let row _ = 
    init c (fun i -> Scanf.scanf " %c" color_of_char)
  in init r row

let print_board b =
  iter (fun row ->
      iter (fun c -> Printf.printf "%c" (char_of_color c)) row;
      print_newline ())
    b

let take_part b n m srow scol =
  sub b srow n |> map (fun a -> sub a scol m)

let take_parts b n m =
  let brow = length b
  and bcol = length b.(0) in
  let num_row = brow - n + 1
  and num_col = bcol - m + 1
  in List.init num_row (fun a -> List.init num_col (fun b -> a, b))
     |> List.flatten
     |> List.map (fun (srow, scol) -> take_part b n m srow scol)

let correct_color s i j =
  if (i + j) mod 2 = 0 then s
  else inverse s

let mark_rc s c i j = if c = (correct_color s i j) then 0 else 1

let count_recolor b =
  let f s =
    mapi (fun i m -> mapi (fun j c -> mark_rc s c i j) m) b
    |> to_list |> concat
    |> fold_left (fun p n -> p+n) 0
  in min (f White) (f Black)

let solve b =
  take_parts b 8 8
  |> List.map (fun b -> count_recolor b)
  |> List.fold_left (fun p n -> min p n) 65

let _ =
  let n, m = Scanf.scanf "%d %d" (fun a b -> a, b)
  in read_board n m |> solve |> print_int
