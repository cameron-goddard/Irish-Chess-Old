open Piece

module Board = struct
  type t = piece list

  exception InvalidMove of string

  let rec piece_at (x, y) t =
    match t with
    | [] -> None
    | p :: t -> if piece_loc p = (x, y) then Some p else piece_at (x, y) t

  let move_to t start (xf, yf) =
    create (get_piece_type start) (get_piece_color start) xf yf
    :: List.filter (fun x -> x <> start) t

  let capture t start (xf, yf) =
    move_to (List.filter (fun x -> piece_loc x <> (xf, yf)) t) start (xf, yf)

  let pawn_y start (xi, yi) (xf, yf) =
    if
      (get_piece_color start = Black && yi - yf = 1)
      || (get_piece_color start = White && yf - yi = 1)
    then true
    else false

  let rec piece_on_path t (xi, yi) (xf, yf) =
    if piece_at (xf, yf) t <> None then true
    else
      let path = (xf - xi, yf - yi) in
      match path with
      | 0, 0 -> false
      | x, 0 ->
          if x > 0 then piece_on_path t (xi, yi) (xf - 1, yf)
          else piece_on_path t (xi, yi) (xf + 1, yf)
      | 0, y ->
          if y > 0 then piece_on_path t (xi, yi) (xf, yf - 1)
          else piece_on_path t (xi, yi) (xf, yf + 1)
      | x, y ->
          if Int.abs x == Int.abs y then
            match (x > 0, y > 0) with
            | true, true -> piece_on_path t (xi, yi) (xf - 1, yf - 1)
            | false, false -> piece_on_path t (xi, yi) (xf + 1, yf + 1)
            | true, false -> piece_on_path t (xi, yi) (xf - 1, yf + 1)
            | false, true -> piece_on_path t (xi, yi) (xf + 1, yf - 1)
          else raise (InvalidMove "Does not move diagonally")

  let kinght_valid_move t (xi, yi) (xf, yf) =
    match (Int.abs xf - xi, Int.abs yf - yi) with
    | 2, 1 -> true
    | 1, 2 -> true
    | _ -> false

  let bishop_valid_move t start (xi, yi) (xf, yf) =
    match (xf - xi, yf - yi) with
    | x, y ->
        if
          (x < 0 && y < 0 && not (piece_on_path t (xi, yi) (xf + 1, yf + 1)))
          || (x > 0 && y > 0 && not (piece_on_path t (xi, yi) (xf - 1, yf - 1)))
          || (x > 0 && y < 0 && not (piece_on_path t (xi, yi) (xf - 1, yf + 1)))
          || (x < 0 && y > 0 && not (piece_on_path t (xi, yi) (xf + 1, yf - 1)))
        then capture t start (xf, yf)
        else raise (InvalidMove "Invalid move for bishop")

  let rook_valid_move t start (xi, yi) (xf, yf) =
    match (xf - xi, yf - yi) with
    | x, 0 ->
        if
          (x < 0 && not (piece_on_path t (xi, yi) (xf + 1, yf)))
          || (x > 0 && not (piece_on_path t (xi, yi) (xf - 1, yf)))
        then capture t start (xf, yf)
        else raise (InvalidMove "Invalid move for rook")
    | 0, y ->
        if
          (y < 0 && not (piece_on_path t (xi, yi) (xf, yf + 1)))
          || (y > 0 && not (piece_on_path t (xi, yi) (xf, yf - 1)))
        then capture t start (xf, yf)
        else raise (InvalidMove "Invalid move for rook")
    | _ -> raise (InvalidMove "Invalid move for rook")

  let special_move start t (xi, yi) (xf, yf) =
    match get_piece_type start with
    | Pawn ->
        if
          xi = xf
          && ((get_piece_color start = Black && yi - yf = 2)
             || (get_piece_color start = White && yf - yi = 2))
          && piece_at (xf, yf) t = None
          && ((get_piece_color start = Black && piece_at (xf, yf - 1) t = None)
             || (get_piece_color start = White && piece_at (xf, yf + 1) t = None)
             )
        then move_to t start (xf, yf) (* else if en_passant *)
        else raise (InvalidMove "Invalid Move")
    | King | Rook -> t (* implement castle *)
    | _ -> t

  let valid_move start t (xi, yi) (xf, yf) =
    match get_piece_type start with
    | Pawn ->
        if
          xi = xf
          && pawn_y start (xi, yi) (xf, yf)
          && piece_at (xf, yf) t = None
        then move_to t start (xf, yf)
        else if
          Int.abs (xf - xi) = 1
          && pawn_y start (xi, yi) (xf, yf)
          && piece_at (xf, yf) t <> None
        then capture t start (xf, yf)
        else raise (InvalidMove "Invalid Move")
    | Knight ->
        if kinght_valid_move t (xi, yi) (xf, yf) then capture t start (xf, yf)
        else raise (InvalidMove "Invalid move for knight")
    | Bishop -> bishop_valid_move t start (xi, yi) (xf, yf)
    | Rook -> rook_valid_move t start (xi, yi) (xf, yf)
    | Queen -> (
        match (xf - xi, yf - yi) with
        | x, 0 | 0, x -> rook_valid_move t start (xi, yi) (xf, yf)
        | x, y -> bishop_valid_move t start (xi, yi) (xf, yf))
    | King -> t

  let update t (xi, yi) (xf, yf) =
    let start_opt = piece_at (xi, yi) t in
    let final_opt = piece_at (xf, yf) t in
    match (start_opt, final_opt) with
    | Some start, Some final ->
        if get_piece_color start = get_piece_color final then
          raise (InvalidMove "Piece of same color on tile")
        else valid_move start t (xi, yi) (xf, yf)
    | Some start, _ -> valid_move start t (xi, yi) (xf, yf)
    | _ -> failwith "Impossible to start piece"

  let graphics_rep t = [ (0, 0) ]
end