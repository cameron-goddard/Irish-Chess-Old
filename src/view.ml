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

let colored_tile clr left_x left_y size =
  Graphics.set_color clr;
  Graphics.fill_rect left_x left_y size size

let draw_rook size pos =
  match pos with
  | x, y ->
      Graphics.fill_rect
        (x + (size / 5))
        (y + (size / 20))
        (3 * size / 5)
        (size * 9 / 10)

let draw_knight size pos =
  match pos with
  | x, y ->
      Graphics.fill_ellipse
        (x + (size / 2))
        (y + (size / 2))
        (size / 4) (size / 2)

let draw_bishop size pos =
  match pos with
  | x, y ->
      Graphics.fill_poly
        [|
          (x + (size / 2), y);
          (x, y + (size / 2));
          (x + (size / 2), y + size);
          (x + size, y + (size / 2));
        |]

let draw_king size pos =
  match pos with
  | x, y -> Graphics.fill_circle (x + (size / 2)) (y + (size / 2)) (size / 2)

(* fix queen drawing *)
let draw_queen size pos =
  match pos with
  | x, y ->
      Graphics.fill_poly
        [|
          (x + (size / 3), y);
          (x + (2 * size / 3), y);
          (x, y + (size / 2));
          (x + (size / 2), y + size);
          (x + size, y + (size / 2));
        |]

let draw_pawn size pos =
  match pos with
  | x, y ->
      Graphics.fill_poly [| (x, y); (x + size, y); (x + (size / 2), size + y) |]

let draw_pt (piece : Piece.piece) size pos =
  match get_piece_type piece with
  | Pawn -> draw_pawn size pos
  | Queen -> draw_queen size pos
  | King -> draw_king size pos
  | Bishop -> draw_bishop size pos
  | Knight -> draw_knight size pos
  | Rook -> draw_rook size pos

let rec board_piece board =
  match board with
  | [] -> (0, 0)
  | h :: t ->
      piece_loc h;
      board_piece t

let rec make_positions_x num_pos lst x y inc =
  if List.length lst = num_pos then lst
  else make_positions_x num_pos ((x + inc, y) :: lst) (x + inc) y inc

let rec make_positions_y num_pos lst x y inc =
  if List.length lst = num_pos then lst
  else make_positions_y num_pos ((x, y + inc) :: lst) x (y + inc) inc

let rec make_combined_positions pos x_lst inc acc =
  match x_lst with
  | [] -> acc
  | (x, y) :: t ->
      acc
      @ make_positions_y pos [] x y inc
      @ make_combined_positions pos t inc acc

let x_lst = make_positions_x 8 [] 0 0 1
let xy_lst = make_combined_positions 8 x_lst 1 []

let rec patterned_board board xy_lst =
  match xy_lst with
  | [] -> ()
  | (x, y) :: t when board_has_piece_at (x - 1) (y - 1) board -> (
      match Board.piece_at (x - 1, y - 1) board with
      | None -> print_endline "hi"
      | Some p -> begin
          match get_piece_color p with
          | White ->
              set_color Graphics.magenta;
              draw_pt p 50 (x * 50, y * 50);
              patterned_board board t
          | Black ->
              set_color Graphics.yellow;
              draw_pt p 50 (x * 50, y * 50);
              patterned_board board t
        end)
  | (x, y) :: t ->
      (* colored_tile Graphics.magenta (x * 50) (y * 50) 50; *)
      patterned_board board t

let rec draw_helper xy_lst =
  match xy_lst with
  | [] -> []
  | (x, y) :: t when Int.abs (x - y) * 50 mod 20 = 0 ->
      colored_tile Graphics.black (x * 50) (y * 50) 50;
      draw_helper t
  | (x, y) :: t ->
      colored_tile Graphics.cyan (x * 50) (y * 50) 50;
      draw_helper t

let rec print_xy_list lst =
  match lst with
  | [] -> ()
  | (x, y) :: t ->
      print_endline (string_of_int x ^ string_of_int y);
      print_xy_list t

let rec write_letters lst size =
  match lst with
  | [] -> []
  | (x, y) :: t when (size * y) - size = 0 ->
      Graphics.moveto ((size * x) + (size / 2)) (y / 2);
      Graphics.draw_char (Char.chr (64 + (size * x / size)));
      write_letters t size
  | (x, y) :: t -> write_letters t size

let rec write_numbers lst size =
  match lst with
  | [] -> []
  | (x, y) :: t when (size * x) - size = 0 ->
      Graphics.moveto (size * x / 2) ((size * y) + (size / 2));
      Graphics.draw_string (string_of_int (size * y / size));
      write_numbers t size
  | (x, y) :: t -> write_numbers t size

let draw_board board =
  write_numbers xy_lst 50;
  write_letters xy_lst 50;
  draw_helper xy_lst;
  patterned_board board xy_lst;
  ()

let init =
  Graphics.open_graph "";
  set_window_title "IRISH CHESS"
