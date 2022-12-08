open Piece

module Board = struct
  type t = piece list

  exception InvalidMove of string
  exception Checkmate of string

  let rec piece_at (x, y) t =
    match t with
    | [] -> None
    | p :: t -> if piece_loc p = (x, y) then Some p else piece_at (x, y) t

  (** [legal_coord (xi, yi) (xf, yf)] checks if [(xf, yf)] are on the board and
      returns [(xf, yf)] if so else [(xi, yi)] *)
  let legal_coord (xi, yi) (xf, yf) =
    if xf < 0 || xf > 7 || yf < 0 || yf > 7 then (xi, yi) else (xf, yf)

  (** [move_to t start (xf, yf)] changes [start] location to that of [(xf, yf)] *)
  let move_to t start (xf, yf) =
    create (get_piece_type start) (get_piece_color start) xf yf
    :: List.filter (fun x -> x <> start) t

  let rev_color color =
    match color with
    | White -> Black
    | Black -> White

  (** [find_king t color] returns the king with [color] in [t] *)
  let find_king t color =
    match
      List.filter
        (fun x -> get_piece_color x = color && get_piece_type x = King)
        t
    with
    | [ h ] -> h
    | _ -> failwith "Why there no king"

  (** [capture t start (xf, yf)] changes [start] location to that of [(xf, yf)]
      and removes the piece that was at [(xf, yf)]*)
  let capture t start (xf, yf) =
    move_to (List.filter (fun x -> piece_loc x <> (xf, yf)) t) start (xf, yf)

  (** [pawn_y start (xi, yi) (xf, yf)] checks if [start] pawn moves by a valid
      one y coordinate *)
  let pawn_y start (xi, yi) (xf, yf) =
    if
      (get_piece_color start = Black && yi - yf = 1)
      || (get_piece_color start = White && yf - yi = 1)
    then true
    else false

  (** [piece_on_path t (xi, yi) (xf, yf)] checks if there is a piece on the path
      from [(xi, yi)] to [(xf, yf)]. Raises [InvalidMove] if the piece moves in
      a x and y direction but not diagonally *)
  let rec piece_on_path t (xi, yi) (xf, yf) =
    if piece_at (xf, yf) t <> None then true
    else
      let path = (xf - xi, yf - yi) in
      match path with
      | 1, 0 | 0, 1 | 1, 1 | 0, -1 | -1, 0 | 1, -1 | -1, 1 | -1, -1 -> false
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

  (** [knight_valid_move t (xi, yi) (xf, yf)] checks if [(xi, yi)] to [(xf, yf)]
      is of a L shape *)
  let kinght_valid_move (xi, yi) (xf, yf) =
    match (Int.abs (xf - xi), Int.abs (yf - yi)) with
    | 2, 1 -> true
    | 1, 2 -> true
    | _ -> false

  (** [bishop_valid_move t start (xi, yi) (xf, yf)] . Raises [InvalidMove] if
      [(xi, yi)] to [(xf, yf)] is not diagonal *)
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

  (** [rook_valid_move t start (xi, yi) (xf, yf)] moves [start] to [(xf, yf)].
      Raises [InvalidMove] if [(xi, yi)] to [(xf, yf)] is not only horizontal,
      not only vertical, or if there is a same color piece on the path *)
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

  (** [king_valid_move (xi, yi) (xf, yf)] checks if [(xf, yf)] is a feasible
      distance a king can travel given [(xi, yi)] *)
  let king_valid_move (xi, yi) (xf, yf) =
    match (Int.abs xf - xi, Int.abs yf - yi) with
    | 1, 1 -> true
    | 1, 0 -> true
    | 0, 1 -> true
    | _ -> false

  (** [special_move start t (xi, yi) (xf, yf)] checks whether a "special" move
      can be done. Raises [InvalidMove] if special move cannot be done. Examples
      include en passant, castling, and pawns moving by y +- 2 *)
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

  (** [valid_move start t (xi, yi), (xf, yf)] returns the updated [t] if piece
      [start] at position [(xi, yi)] is able to move to position [(xf, yf)].
      Raises [InvalidMove] if move cannot be done *)
  let rec valid_move start t (xi, yi) (xf, yf) =
    match get_piece_type start with
    | Pawn ->
        if
          xi = xf
          && pawn_y start (xi, yi) (xf, yf)
          && piece_at (xf, yf) t = None
        then check_status (move_to t start (xf, yf)) start
        else if
          Int.abs (xf - xi) = 1
          && pawn_y start (xi, yi) (xf, yf)
          && piece_at (xf, yf) t <> None
        then check_status (capture t start (xf, yf)) start
        else if Int.abs (yf - yi) = 2 then
          check_status (special_move start t (xi, yi) (xf, yf)) start
        else raise (InvalidMove "Invalid Move")
    | Knight ->
        if kinght_valid_move (xi, yi) (xf, yf) then
          check_status (capture t start (xf, yf)) start
        else raise (InvalidMove "Invalid move for knight")
    | Bishop -> check_status (bishop_valid_move t start (xi, yi) (xf, yf)) start
    | Rook -> check_status (rook_valid_move t start (xi, yi) (xf, yf)) start
    | Queen -> (
        match (xf - xi, yf - yi) with
        | x, 0 | 0, x ->
            check_status (rook_valid_move t start (xi, yi) (xf, yf)) start
        | x, y ->
            check_status (bishop_valid_move t start (xi, yi) (xf, yf)) start)
    | King ->
        if check_prev t start (xf, yf) && king_valid_move (xi, yi) (xf, yf) then
          capture t start (xf, yf)
        else raise (InvalidMove "Invalid move for king")

  (** [check_prev (h :: lst) start (xf, yf)] determines if [(xf, yf)] is a valid
      king move by avoiding check *)
  and check_prev t start (xf, yf) =
    match t with
    | [] -> true
    | h :: lst -> (
        match get_piece_color h <> get_piece_color start with
        | true -> begin
            try
              ignore (valid_move h (h :: lst) (piece_loc h) (xf, yf));
              false
            with _ -> check_prev lst start (xf, yf)
          end
        | false -> check_prev lst start (xf, yf))

  (** [check_stats new_t start] determines if the updated t [new_t] is valid by
      checking if own king is in check *)
  and check_status new_t start =
    if
      check_prev new_t start
        (piece_loc (find_king new_t (get_piece_color start)))
    then new_t
    else raise (InvalidMove "Will be in check")

  (** [check_pieces t start acc] finds the pieces in [t] that can check [king]
      and add them to list [acc] *)
  and check_pieces t king acc =
    match t with
    | [] -> acc
    | h :: lst -> (
        try
          ignore (valid_move h t (piece_loc h) (piece_loc king));
          h :: acc
        with _ -> check_pieces lst king acc)

  and block_path t (xi, yi) (xf, yf) piece =
    if
      List.length
        (check_pieces t
           (create Pawn (rev_color (get_piece_color piece)) xf yf)
           [])
      <> 0
    then true
    else
      let path = (xf - xi, yf - yi) in
      match path with
      | 0, 0 -> false
      | x, 0 ->
          if x > 0 then block_path t (xi, yi) (xf - 1, yf) piece
          else block_path t (xi, yi) (xf + 1, yf) piece
      | 0, y ->
          if y > 0 then block_path t (xi, yi) (xf, yf - 1) piece
          else block_path t (xi, yi) (xf, yf + 1) piece
      | x, y ->
          if Int.abs x == Int.abs y then
            match (x > 0, y > 0) with
            | true, true -> block_path t (xi, yi) (xf - 1, yf - 1) piece
            | false, false -> block_path t (xi, yi) (xf + 1, yf + 1) piece
            | true, false -> block_path t (xi, yi) (xf - 1, yf + 1) piece
            | false, true -> block_path t (xi, yi) (xf + 1, yf - 1) piece
          else raise (InvalidMove "Invalid path ")

  (** [checkmate t start] checks whether or not the opposing king will be in
      checkmate after move *)
  and checkmate t start =
    let king_op =
      match get_piece_color start with
      | White -> find_king t Black
      | Black -> find_king t White
    in
    match piece_loc king_op with
    | x, y ->
        if
          check_prev t king_op (x, y)
          || check_prev t king_op (legal_coord (x, y) (x + 1, y))
          || check_prev t king_op (legal_coord (x, y) (x, y + 1))
          || check_prev t king_op (legal_coord (x, y) (x + 1, y + 1))
          || check_prev t king_op (legal_coord (x, y) (x - 1, y))
          || check_prev t king_op (legal_coord (x, y) (x, y - 1))
          || check_prev t king_op (legal_coord (x, y) (x - 1, y - 1))
          || check_prev t king_op (legal_coord (x, y) (x - 1, y + 1))
          || check_prev t king_op (legal_coord (x, y) (x + 1, y - 1))
        then false
        else
          let lst_opps = check_pieces t king_op [] in
          if List.length lst_opps = 1 then
            match lst_opps with
            | [ h ] ->
                if List.length (check_pieces t h []) <> 0 then
                  block_path t (piece_loc king_op) (piece_loc h) h
                else true
            | _ -> failwith "Impossible Checkmate"
          else true

  let update t (xi, yi) (xf, yf) =
    let start_opt = piece_at (xi, yi) t in
    let final_opt = piece_at (xf, yf) t in
    match (start_opt, final_opt) with
    | Some start, Some final ->
        if get_piece_color start = get_piece_color final then
          raise (InvalidMove "Piece of same color on tile")
        else if checkmate (valid_move start t (xi, yi) (xf, yf)) start then
          raise (Checkmate "Checkmate")
        else valid_move start t (xi, yi) (xf, yf)
    | Some start, _ ->
        if checkmate (valid_move start t (xi, yi) (xf, yf)) start then
          raise (Checkmate "Checkmate")
        else valid_move start t (xi, yi) (xf, yf)
    | _ -> failwith "Impossible to start piece"

  let graphics_rep t = [ (0, 0) ]
end