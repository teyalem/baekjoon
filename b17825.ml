(* helper: read numbers from stdin *)
let read_list n =
  List.init n (fun _ -> Scanf.scanf " %d" (fun n -> n))

module Board = struct
  (* line of board *)
  type line = Main | Side1 | Side2 | Side3 | Side4

  (* current position of pieces *)
  type pos = On of line * int (* line, position *)
           | Out (* piece went out *)

  type t = pos list (* board = position of pieces *)

  (* debug function *)
  let print_piece = function
    | On (line, pos) ->
      let line_str = match line with
        | Main -> "Main"
        | Side1 -> "Side1"
        | Side2 -> "Side2"
        | Side3 -> "Side3"
        | Side4 -> "Side4"
      in Printf.printf "On line %s position %d\n" line_str pos
    | Out -> print_endline "Out"

  (* board score data *)
  let main_line = List.init 21 (fun i -> i*2)
  let side_1 = [ 13; 16; 19 ]
  let side_2 = [ 22; 24 ]
  let side_3 = [ 28; 27; 26 ]
  let side_4 = [ 25; 30; 35 ]

  let get_line = function
    | Main -> main_line
    | Side1 -> side_1
    | Side2 -> side_2
    | Side3 -> side_3
    | Side4 -> side_4

  let get_score = function
    | On (line, pos) ->
      let score_list = get_line line
      in List.nth score_list pos
    | Out -> 0

  let line_length line = List.length (get_line line)

  (* return new place for given piece *)
  let rec move_piece moves = function
    | Out -> Out
    | On (Main, 5) -> move_piece (moves-1) (On (Side1, 0))
    | On (Main, 10) -> move_piece (moves-1) (On (Side2, 0))
    | On (Main, 15) -> move_piece (moves-1) (On (Side3, 0))
    | On (line, pos) ->
      let len = line_length line in
      if pos + moves < len
      then On (line, pos + moves)
      else 
        let left_moves = moves - (len - pos)
        in match line with
          | Side1 | Side2 | Side3 -> move_piece left_moves (On (Side4, 0))
          | Side4 -> move_piece left_moves (On (Main, 20))
          | Main -> Out

  (* check if updated position is already occupied *)
  let is_occupied pieces = function
    | Out -> false
    | On _  as x -> List.exists (fun p -> p = x) pieces

  (* advance the piece by given number n *)
  let advance (piece: pos) (pieces: t) (n: int) : pos option =
    let next_pos = move_piece n piece in
    if is_occupied pieces next_pos then None else Some next_pos

  (* emulate board and return maximum score *)
  let rec emulate score pieces = function (* numbers rolled out *)
    | [] -> score (* no more possible move, return score *)
    | move_num::rest ->
      let nps_score = (* new piece strategy *)
        if List.length pieces < 4 (* is there remaining piece? *)
        then
          match advance (On (Main, 0)) pieces move_num with
          | Some new_piece ->
            let s = get_score new_piece
            in emulate (score + s) (new_piece::pieces) rest
          | None -> score (* placing new piece failed, just return score *)
        else score

      and aps_scores = (* advance piece strategy *)
        let advance_one idx =
          let ps = pieces |> List.mapi (fun i p ->
              if i = idx
              then advance p pieces move_num
              else Some p)
          in
          if List.for_all (fun p -> Option.is_some p) ps
          then Some (List.map Option.get ps)
          else None

        and get_score_of i ps = List.nth ps i |> get_score

        in List.(
            init (length pieces) (fun i -> i)
            |> map (fun i -> i, (advance_one i))
            |> filter (fun (i, ps) -> Option.is_some ps)
            |> map (fun (i, ps) -> i, Option.get ps)
            |> map (fun (i, ps) -> ps, (get_score_of i ps))
            |> map (fun (ps, s) -> emulate (score + s) ps rest)
          )

      (* select maximum strategy *)
      in List.fold_left max 0 (nps_score::aps_scores)
end

let _ =
  read_list 10 |> Board.emulate 0 [] |> print_int
