let f1 = [ 500, 1; 300, 2; 200, 3; 50, 4; 30, 5; 10, 6 ]
let f2 = [ 512, 1; 256, 2; 128, 4; 64, 8; 32, 16 ]

let rec prize n = function
  | _ when n = 0 -> 0
  | [] -> 0
  | (m, p)::ps -> if n <= p then m else prize (n-p) ps

let () =
  let t = Scanf.scanf " %d" Fun.id in
  let rec loop i =
    if i < t then begin
      let p = Scanf.scanf " %d %d"
          (fun n1 n2 -> prize n1 f1 + prize n2 f2)
      in
      Printf.printf "%d\n" (p * 10000);
      loop @@ i+1
    end
  in
  loop 0
