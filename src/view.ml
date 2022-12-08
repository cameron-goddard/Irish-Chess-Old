open Graphics
open Board
open Piece
open Util

let get_unicode_char w = function
  | King -> if w then "♔" else "♚"
  | Queen -> if w then "♕" else "♛"
  | Bishop -> if w then "♗" else "♝"
  | Knight -> if w then "♘" else "♞"
  | Rook -> if w then "♖" else "♜"
  | Pawn -> if w then "♙" else "♟︎"

let rec board_has_piece_at x y b =
  match b with
  | [] -> false
  | { piece_type = pt; color = c; x = px; y = py } :: t ->
      if px = x && py = y then true else board_has_piece_at x y t

let print_board b =
  let rec loop_board b x y count =
    let x' = (x + 1) mod 8 in
    let y' = if x = 7 then y - 1 else y in
    if count = 64 then "\n"
    else
      let line_num =
        if x' = 0 then "\n  " ^ string_of_int (y' + 1) ^ " " else ""
      in
      if board_has_piece_at x' y' b then
        match Board.piece_at (x', y') b with
        | None -> failwith "no"
        | Some { piece_type = pt; color = c; x = px; y = py } ->
            line_num
            ^ get_unicode_char (c <> White) pt
            ^ " "
            ^ loop_board b x' y' (count + 1)
      else line_num ^ "· " ^ loop_board b x' y' (count + 1)
  in
  loop_board b (-1) 7 0 ^ "    a b c d e f g h\n"

let draw_board board = ()
let init = ()