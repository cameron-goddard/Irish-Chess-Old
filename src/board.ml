open Piece

module Board = struct
  type t = piece list

  let update t (xi, yi) (xf, yf) = t

  let rec piece_at (x, y) t =
    match t with
    | [] -> None
    | p :: t -> if piece_loc p = (x, y) then Some p else piece_at (x, y) t

  let graphics_rep t = [ (0, 0) ]

  exception InvalidMove of string
end