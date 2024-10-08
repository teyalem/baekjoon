let rec find_last f = function
  | [] -> None
  | x :: xs ->
    if f x then
      begin match find_last f xs with
        | None -> Some (x, xs)
        | o -> o
      end
    else
      None

let check len c cuts k =
  let rec aux remain c cuts =
    if c = 0 then
      if remain <= k then Some remain
      else None
    else match find_last (fun n -> n >= remain - k) cuts with
      | None ->
        if remain <= k then Some remain
        else None
      | Some (remain, cuts) ->
        aux remain (c-1) cuts
  in
  aux len c cuts

let solve len c cuts =
  let rec aux l h =
    if l = h then l
    else
      let m = (l+h)/2 in
      if Option.is_some @@ check len c cuts m then
        aux l m
      else
        aux (m+1) h
  in

  aux 0 len

let read () = Scanf.scanf " %d" Fun.id

let () =
  Scanf.scanf "%d %d %d" @@ fun l k c ->
  let cuts =
    List.init k (fun _ -> read ())
    |> List.sort (Fun.flip Int.compare)
  in
  let c = min k c in
  let r = solve l c cuts in
  let f = check l c cuts r |> Option.get in
  Printf.printf "%d %d" r f
