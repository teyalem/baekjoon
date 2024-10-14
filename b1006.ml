let inf = max_int/2

let arrmin arr mask =
  List.fold_left (fun acc i ->
      min acc arr.(i))
    inf
    mask

let solve n w ring =
  let dp =
    Array.init 4 (fun _ ->
        Array.make_matrix n 4 inf)
  in

  let options os =
    os
    |> List.filter_map (fun (c, vs) ->
        if c then Some vs else None)
    |> List.fold_left min max_int
  in

  let wall i =
    ring.(0).(i) + ring.(1).(i) <= w
  and brick i l =
    ring.(l).(i) + ring.(l).((n+i-1) mod n) <= w
  in

  let all = [0;1;2;3] in

  dp.(0).(1).(0) <- options [
      wall 0 && wall 1, 2;
      brick 1 0 && brick 1 1, 2];
  dp.(0).(1).(3) <- options [wall 0, 3];
  dp.(1).(1).(1) <- options [brick 1 1, 3];
  dp.(2).(1).(2) <- options [brick 1 0, 3];
  dp.(3).(1).(0) <- options [wall 1, 3];
  dp.(3).(1).(3) <- 4;

  for z = 0 to 3 do
    let dp = dp.(z) in
    for i = 2 to n-1 do
      dp.(i).(0) <- options [
          wall i, 1 + arrmin dp.(i-1) all;
          brick i 0 && brick i 1, dp.(i-1).(3)];

      dp.(i).(1) <- options [
          brick i 1, 1 + arrmin dp.(i-1) [2;3]];

      dp.(i).(2) <- options [
          brick i 0, 1 + arrmin dp.(i-1) [1;3]];

      dp.(i).(3) <-
        2 + arrmin dp.(i-1) all;
    done
  done;

  let last = Array.make 4 max_int in
  last.(0) <- arrmin dp.(0).(n-1) all;
  last.(1) <- options [
      brick 0 0, arrmin dp.(1).(n-1) [1;3] - 1;
      true, arrmin dp.(1).(n-1) all];
  last.(2) <- options [
      brick 0 1, arrmin dp.(2).(n-1) [2;3] - 1;
      true, arrmin dp.(2).(n-1) all];
  last.(3) <- options [
      brick 0 0 && brick 0 1, dp.(3).(n-1).(3) - 2;
      brick 0 0, arrmin dp.(3).(n-1) [1; 3] - 1;
      brick 0 1, arrmin dp.(3).(n-1) [2; 3] - 1;
      true, arrmin dp.(3).(n-1) all];

  arrmin last all

let solve n w ring =
  if n = 1 then
    if ring.(0).(0) + ring.(1).(0) <= w then
      1 else 2
  else
    solve n w ring

let read () = Scanf.scanf " %d" Fun.id

let () =
  let t = read () in
  for _ = 1 to t do
    Scanf.scanf " %d %d" @@ fun n w ->
    let arr =
      Array.init 2 (fun _ ->
          Array.init n (fun _ -> read ()))
    in
    solve n w arr |> Printf.printf "%d\n"
  done
