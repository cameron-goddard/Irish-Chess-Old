(** amount moved forward first number, amount moved side is *)
type move =(int * int)

let moves_for_piece piece =
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
  | _ -> (9909090,909090)

(** situational moves for chess pieces *)
  let special_moves_for_piece piece tile =
    match piece with 
    | Pawn ->begin match tile with 
            | (c, i)-> if c=='B' then [2,0] else []
            | _ -> []   
          end 
    | Knight -> []
    | Bishop -> []
    | Rook -> []
    | King -> []
    | Queen ->[]
    | Castle -> []
    | _ -> (9909090,909090)
let is_valid_pos move piece -> get_in_list move ((moves_for_piece piece)@(special_moves_for_piece piece piece.tile))


let rec get_in_list move lst ->
  match lst with 
  |[]-> false 
  | h::t-> if begin match move h with
            |(mu,mr) (h1,h2)->if mu==h1 && mr ==h2 then true else  get_in_list t  
            | _ -> false end 




