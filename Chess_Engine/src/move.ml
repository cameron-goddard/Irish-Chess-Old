open Piece

(** amount moved forward first number, amount moved side is *)
type moves =(int * int)
type move_list = moves list

let moves_for_piece (piece : Piece.piece_type) =
  match piece with 
  | Pawn -> [(1,0)]
  | Knight -> [(2,1);(1,2);(-1,2);(-1,-2);(1,-2);(2,-1);(-2,1);(-2,-1)]
  | Bishop -> [(1,1);(2,2);(3,3);(4,4);(5,5);(6,6);(7,7);(8,8);
                (-1,-1);(-2,-2);(-3,-3);(-4,-4);(-5,-5);(-6,-6);(-7,-7);(-8,-8);]
  | Rook -> [(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7);(0,8);
            (0,-1);(0,-2);(0,-3);(0,-4);(0,-5);(0,-6);(0,-7);(0,-8);
          (1,0);(2,0);(3,0);(4,0);(5,0);(6,0);(7,0);(8,0);
          (-1,0);(-2,0);(-3,0);(-4,0);(-5,0);(-6,0);(-7,0);(-8,0);]
  | King -> [(0,1);(1,1);(1,0);(-1,1);(1,-1);(-1,-1);(0,-1);(-1,0);]
  | Queen ->[(0,1);(0,2);(0,3);(0,4);(0,5);(0,6);(0,7);(0,8);
    (0,-1);(0,-2);(0,-3);(0,-4);(0,-5);(0,-6);(0,-7);(0,-8);
      (1,0);(2,0);(3,0);(4,0);(5,0);(6,0);(7,0);(8,0);
        (-1,0);(-2,0);(-3,0);(-4,0);(-5,0);(-6,0);(-7,0);(-8,0);
        (1,1);(2,2);(3,3);(4,4);(5,5);(6,6);(7,7);(8,8);
      (-1,-1);(-2,-2);(-3,-3);(-4,-4);(-5,-5);(-6,-6);(-7,-7);(-8,-8);]

(** situational moves for chess pieces *)
  let special_moves_for_piece piece tile =
    match piece with 
    | Pawn ->begin match tile with 
            | (c, i)-> if i == 2 then [2,0] else []
          end 
    | Knight -> []
    | Bishop -> []
    | Rook -> []
    | King -> []
    | Queen ->[]


let rec get_in_list move lst =
  match lst with 
  |[]-> false 
  | h::t-> if match move, h with
      |(mu,mr), (h1,h2) -> mu == h1 && mr == h2 then true else  get_in_list move t 
    
    
    
let is_valid_move (move : moves) piece = get_in_list move (moves_for_piece piece)
