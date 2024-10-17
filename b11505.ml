let modular = 1_000_000_007

let make n arr =
  let st = Array.make (2*n) 0 in
  Array.blit arr 0 st n n;
  for i = n-1 downto 1 do
    st.(i) <- (st.(2*i) * st.(2*i+1)) mod modular
  done;
  st

let query n st (l, h) =
  let l = ref (l+n)
  and h = ref (h+n) in
  let s = ref 1 in

  while !l <= !h do
    if !l mod 2 = 1 then begin
      s := (!s * st.(!l)) mod modular;
      incr l
    end;
    if !h mod 2 = 0 then begin
      s := (!s * st.(!h)) mod modular;
      decr h
    end;
    l := !l/2; h := !h/2
  done;
  !s

let change n st i x = 
  let i = ref (i+n) in
  st.(!i) <- x;
  i := !i/2;
  while !i > 0 do
    st.(!i) <- (st.(!i*2) * st.(!i*2+1)) mod modular;
    i := !i/2
  done

let read () = Scanf.scanf " %d" Fun.id

let () =
  Scanf.scanf " %d %d %d" @@ fun n m k ->
  let st =
    Array.init n (fun _ -> read ())
    |> make n
  in
  for _ = 1 to m+k do
    Scanf.scanf " %d %d %d" @@ fun a b c ->
    if a = 1 then change n st (b-1) c
    else
      query n st (b-1, c-1)
      |> Printf.printf "%d\n"
  done
