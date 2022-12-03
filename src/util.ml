let hex_to_rgb h =
  let r = Scanf.sscanf (String.sub h 0 2) "%x" (fun x -> x) in
  let g = Scanf.sscanf (String.sub h 2 2) "%x" (fun x -> x) in
  let b = Scanf.sscanf (String.sub h 4 2) "%x" (fun x -> x) in
  r :: g :: [ b ]
