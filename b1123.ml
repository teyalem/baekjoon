open Printf

module M = Map.Make(Int)

let add_to_list k v g =
  M.update k (function
      | None -> Some [v]
      | Some vs -> Some (v::vs))
    g

let m = 600921647
let addmod a b = (a + b) mod m
let mulmod a b = (a * b) mod m

let combi a b ggs gss =
  let memo = Hashtbl.create 100 in

  let op (ra, ta) (rb, tb) =
    ra+rb, mulmod ta tb
  in

  let mul t (r, ta) =
    r, mulmod t ta
  in

  let rec aux l ga gb =
    if Hashtbl.mem memo (l, ga, gb) then
    else
      let songs = M.find ga gss in
      let r = ref @@ M.singleton l 1 in
      if List.for_all (fun s -> s > l) songs then
      else if
        Hashtbl.add memo (l, ga, gb) r;
        r
  in

let () =
  let scanf = Scanf.scanf in
  let n = scanf "%d" Fun.id in

  let gss =
    let g = ref M.empty in
    for _ = 1 to n do
      g := scanf " %d %d" (fun n i ->
          add_to_list n i !g)
    done;
    !g
  in

  let m = scanf " %d" Fun.id in
  let ggs =
    let g = ref M.empty in
    for i = 0 to m-1 do
      for j = 0 to m-1 do
        scanf " %c" (function
            | 'Y' -> g := add_to_list i j !g
            | 'N' -> ()
            | _ -> assert false)
      done
    done;
    !g
  in

  let a = scanf " %d" Fun.id in
  let b = scanf " %d" Fun.id in

  combi a b ggs gss
  |> print_int
