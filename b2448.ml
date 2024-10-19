let base = [[2]; [1; 3]; List.init 5 Fun.id]

let shift p i =
  List.map (List.map (Int.add i)) p

let append a b n =
  List.map2 (@) a (shift b n)

let rec make n =
  if n = 3 then base
  else 
    let p = make (n/2) in
    shift p (n/2) @ append p p n

let print nlines =
  let width = 2*nlines - 1 in
  List.iter (fun l ->
      List.fold_left (fun lastpos n ->
          Printf.printf "%s*"
            (String.make (n - lastpos) ' ');
          n+1)
        0 l
    |> (fun lastpos ->
         Printf.printf "%s"
           (String.make (width - lastpos) ' '));
      Printf.printf "\n")

let () =
  Scanf.scanf "%d" @@ fun n ->
  make n |> print n
