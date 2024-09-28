let dist x y =
  abs x + abs y

let () =
  Scanf.scanf "%d %d %d" (fun a b c ->
      let d = dist a b in
      if d <= c && d mod 2 = c mod 2 then
        print_string "YES"
      else
        print_string "NO")
