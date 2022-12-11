open Graphics
open Board
open Piece
open Util
open Command
open Yojson.Basic.Util

type display_colors = Graphics.color * Graphics.color

type t = {
  name : string;
  colors : display_colors;
  board : Board.t;
}

type color =
  | Light
  | Dark

type piece_container = {
  king : piece;
  queen : piece;
  bishops : piece list;
  knights : piece list;
  rooks : piece list;
  pawns : piece list;
}

let rec game_step mode (b : Board.t) =
  if mode = "cli" then (
    print_endline (View.print_board b);
    print_string "> ";
    let input = parse (read_line ()) in
    match input with
    | Move (st, en) ->
        (* ANSITerminal.erase Screen; *)
        process_move mode b st en
    | Castle dir -> raise (Failure "Unimplemented")
    | Help -> raise (Failure "Unimplemented")
    | Info ->
        Printf.printf "pressed info\n";
        game_step mode b
    | Empty -> game_step mode b
    | Quit -> exit 0)
  else if mode = "gui/w_text" then (
    View.draw_board b;
    print_string "> ";
    try
      let input = parse (read_line ()) in
      match input with
      | Move (st, en) ->
          (* ANSITerminal.erase Screen; *)
          process_move mode b st en
      | Castle dir -> raise (Failure "Unimplemented")
      | Help -> raise (Failure "Unimplemented")
      | Info ->
          Printf.printf "pressed info\n";
          game_step mode b
      | Empty -> game_step mode b
      | Quit -> exit 0
    with _ ->
      print_endline "INVALID MOVE ... try again";
      game_step mode b)
  else if mode = "gui/no_text" then (
    View.draw_board b;
    let start_status = Graphics.wait_next_event [ Button_down ] in
    let st =
      ((start_status.mouse_x / 50) - 1, (start_status.mouse_y / 50) - 1)
    in
    let end_status = Graphics.wait_next_event [ Button_down ] in
    let en = ((end_status.mouse_x / 50) - 1, (end_status.mouse_y / 50) - 1) in
    process_move mode b st en;
    print_endline
      (string_of_int start_status.mouse_x ^ string_of_int start_status.mouse_y);
    game_step mode b)
  else
    let event = Graphics.wait_next_event [ Key_pressed ] in
    if event.key == 'q' then exit 0
    else if event.button then game_step mode b
    else game_step mode b

and process_move mode b st en =
  try
    let b' = Board.update b st en in
    game_step mode b'
  with Board.InvalidMove s -> print_endline s

let display_colors_of_json j =
  let l = to_list j in
  let light_color = hex_to_rgb (to_string (List.nth l 0)) in
  let dark_color = hex_to_rgb (to_string (List.nth l 1)) in

  ( rgb (List.nth light_color 0) (List.nth light_color 1)
      (List.nth light_color 2),
    rgb (List.nth dark_color 0) (List.nth dark_color 1) (List.nth dark_color 2)
  )

let piece_location_of_json m j =
  let mem = j |> member m |> to_string in
  coords_of_notation mem

let piece_list_of_json c pt j =
  let pieces = j |> member (string_of_piece_type pt) |> to_list in
  let rec ret p =
    match p with
    | [] -> []
    | h :: t ->
        let str = to_string h in
        (* Printf.printf "%s %s at %s\n" (Piece.string_of_color c)
           (Piece.string_of_piece_type pt) str; *)
        Piece.create pt c
          (fst (coords_of_notation str))
          (snd (coords_of_notation str))
        :: ret t
  in
  ret pieces

let piece_container_of_json c j =
  {
    king =
      Piece.create King c
        (fst (piece_location_of_json "king" j))
        (snd (piece_location_of_json "king" j));
    queen =
      Piece.create Queen c
        (fst (piece_location_of_json "queen" j))
        (snd (piece_location_of_json "queen" j));
    bishops = piece_list_of_json c Bishop j;
    knights = piece_list_of_json c Knight j;
    rooks = piece_list_of_json c Rook j;
    pawns = piece_list_of_json c Pawn j;
  }

let piece_list_of_container container =
  match container with
  | { king; queen; bishops; knights; rooks; pawns } ->
      [ king ] @ [ queen ] @ bishops @ knights @ rooks @ pawns

let t_of_json j =
  {
    name = j |> member "name" |> to_string;
    colors = j |> member "colors" |> display_colors_of_json;
    board =
      (j |> member "light_init"
      |> piece_container_of_json White
      |> piece_list_of_container)
      @ (j |> member "dark_init"
        |> piece_container_of_json Black
        |> piece_list_of_container);
  }

let from_json json =
  try t_of_json json with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let main () =
  let mode = Sys.argv.(1) in
  let arg = Sys.argv.(2) in
  let f = Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ arg) in
  let t = from_json f in
  if mode = "cli" then (
    (* ANSITerminal.erase Screen; *)
    ANSITerminal.print_string [ ANSITerminal.green ]
      "☘ Welcome to Irish Chess ☘\n";
    game_step mode t.board)
  else
    let _ = View.init in
    game_step mode t.board
