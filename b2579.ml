open Scanf
let stair n scores =
  let arr = Array.make_matrix n 2 0 in
  let rec aux i s =
    if i >= n then Int.min_int
    else if arr.(i).(s-1) > 0 then arr.(i).(s-1)
    else begin
      let k = scores.(i) +
              if i = n-1 then 0
              else if s = 1 then aux (i+2) 2
              else max (aux (i+1) 1) (aux (i+2) 2)
      in
      arr.(i).(s-1) <- k; k
    end
  in
  max (aux 0 2) (aux 1 2)

let () =
  let n = scanf " %d" Fun.id in
  let scores = Array.init n (fun _ -> scanf " %d" Fun.id) in
  print_int @@ stair n scores
