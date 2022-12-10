let hex_to_rgb h =
  let r = Scanf.sscanf (String.sub h 0 2) "%x" (fun x -> x) in
  let g = Scanf.sscanf (String.sub h 2 2) "%x" (fun x -> x) in
  let b = Scanf.sscanf (String.sub h 4 2) "%x" (fun x -> x) in
  r :: g :: [ b ]

let coords_of_notation str =
  (* Printf.printf "original: %s\n" str; *)
  let fst_char = String.get str 0 in
  (* Printf.printf "First char: %c\n" fst_char; *)
  let st = Char.code fst_char - 97 in
  (* Printf.printf "st: %d\n" st; *)
  let en = int_of_char (String.get str 1) - 49 in
  (* Printf.printf "en: %d\n" en; Printf.printf "(%d, %d)\n" st en; *)
  (st, en)

let notation_of_coords x y =
  let fst = Char.chr (x + 97) in
  let snd = string_of_int (y + 1) in
  Char.escaped fst ^ snd
