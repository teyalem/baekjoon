open Scanf

let mod1 n m =
  let k = n mod m in
  if k = 0 then m else k

module Graph = struct
  type t = int list array

  let make n = Array.make n []

  let add_edge g (f, t) =
    let aux n k = g.(n) <- k::g.(n) in
    aux f t; aux t f

  let del_edge g (f, t) =
    let del k n =
      g.(n) <- List.filter ((<>) k) g.(n)
    in
    let fl = g.(f) and tl = g.(t) in
    g.(f) <- []; g.(t) <- [];
    List.iter (del f) fl;
    List.iter (del t) tl

  let of_tuples n ts =
    let g = make (n+1) in
    List.iter (add_edge g) ts;
    g

  let print g =
    Array.iteri (fun i l ->
        Printf.printf "%d -> [" i;
        List.iter (Printf.printf " %d") l;
        Printf.printf " ]\n")
      g

  let find_pairs g =
    let buf = ref [] in
    let rec aux () =
      try
        for i = 0 to Array.length g - 1 do
          if List.length g.(i) = 1 then begin
            let edge = i, List.hd g.(i) in
            buf :=  edge :: !buf;
            del_edge g edge;
            aux ();
            raise Exit
          end
        done;
      with Exit -> ()
    in
    aux ();
    !buf

end

module Gon = struct
  type t = int * int array

  let get (_, arr) i = arr.(i-1)

  let find_mergable (n, arr) w =
    List.init n succ
    |> List.concat_map (fun i ->
        let next = mod1 (i+1) n in
        [ i, next;
          i + n, next + n;
          i, i + n; ])
    |> List.filter (fun (i, j) -> arr.(i-1) + arr.(j-1) <= w)

  let read n =
    n, Array.init (2*n) (fun _ -> scanf " %d" Fun.id)
end

let () =
  let t = scanf " %d" Fun.id in
  for _ = 1 to t do
    let n, w = scanf " %d %d" (fun n w -> n, w) in
    let g = Gon.read n in
    let k = 
      Gon.find_mergable g w
      |> Graph.of_tuples (2*n)
      |> Graph.find_pairs
      |> List.length
    in
    Printf.printf "%d\n" (2*n-k)
  done
