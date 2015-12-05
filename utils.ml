(* Convert integer to big-endian formatted bytecode *)
let bytes_of_int i =
  let c0 = Char.chr ((i lsr 24) land 0xff) in
  let c1 = Char.chr ((i lsr 16) land 0xff) in
  let c2 = Char.chr ((i lsr 8) land 0xff) in
  let c3 = Char.chr (i land 0xff) in
  List.fold_left (fun acc c -> acc ^ (String.make 1 c)) "" [c0;c1;c2;c3]
