open Graphics
open Board
open Piece
open Util
open Yojson.Basic.Util

type display_colors = Graphics.color * Graphics.color

type t = {
  name : string;
  colors : display_colors;
  board : piece list;
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

let rec game_step (board : Board.t) =
  Graphics.clear_graph ();
  (* View.draw_board board; *)
  let event = Graphics.wait_next_event [ Key_pressed ] in
  if event.key == 'q' then exit 0
  else if event.button then game_step board
  else game_step board

let display_colors_of_json j =
  let l = to_list j in
  let light_color = hex_to_rgb (to_string (List.nth l 0)) in
  let dark_color = hex_to_rgb (to_string (List.nth l 1)) in

  ( rgb (List.nth light_color 0) (List.nth light_color 1)
      (List.nth light_color 2),
    rgb (List.nth dark_color 0) (List.nth dark_color 1) (List.nth dark_color 2)
  )

let piece_container_of_json c j =
  {
    king = Piece.create_piece King c 0 0;
    queen = Piece.create_piece Queen c 0 0;
    bishops = [ Piece.create_piece Bishop c 0 0 ];
    knights = [ Piece.create_piece Knight c 0 0 ];
    rooks = [ Piece.create_piece Rook c 0 0 ];
    pawns = [ Piece.create_piece Pawn c 0 0 ];
  }

let piece_list_of_container c =
  match c with
  | { king; queen; bishops; knights; rooks; pawns } ->
      [ king ] @ [ queen ] @ bishops @ rooks @ pawns

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
  let _ = View.init in
  let arg = Sys.argv.(1) in
  let f = Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ arg) in
  let t = from_json f in
  game_step t.board
