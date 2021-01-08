let fibo n =
  let rec fibo_tail a b i =
    if i = n then b
    else fibo_tail b (a+b) (i+1)
  in fibo_tail 1 0 0

let _ =
  let n = read_int () in
  print_int (fibo n)
